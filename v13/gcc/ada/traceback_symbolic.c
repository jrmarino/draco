/*
  Copyright (C) 1999 by Juergen Pfeifer <juergen.pfeifer@gmx.net>
  Ada for Linux Team (ALT)
  Heavily modified by John Marino <http://www.dragonlace.net>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the
  "Software"), to deal in the Software without restriction, including
  without limitation the rights to use, copy, modify, merge, publish,
  distribute, distribute with modifications, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included
  in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
  IN NO EVENT SHALL THE ABOVE COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
  DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
  OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
  THE USE OR OTHER DEALINGS IN THE SOFTWARE.

  Except as contained in this notice, the name(s) of the above copyright
  holders shall not be used in advertising or otherwise to promote the
  sale, use or other dealings in this Software without prior written
  authorization.
*/

#ifdef IS_CROSS


/*
 * Running addr2line doesn't make sense for cross-compiled objects.
 * Create a dummy function to satisfy g-trasym.o
 */

void
convert_addresses (const char *file_name ATTRIBUTE_UNUSED,
                   void *addrs ATTRIBUTE_UNUSED,
                   int n_addr ATTRIBUTE_UNUSED,
                   void *buf ATTRIBUTE_UNUSED,
                   int *len ATTRIBUTE_UNUSED)
{
  *len = 0;
}

#else


/*
 * use the external program /usr/bin/addr2line to convert addresses
 * into file names and line numbers
 */

#include <sys/types.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>

#define CLOSE_SENDPIPE close(sendpipe[0]); close(sendpipe[1])
#define CLOSE_READPIPE close(readpipe[0]); close(readpipe[1])
#define DUP2CLOSE(oldfd, newfd) dup2(oldfd, newfd); close(oldfd);
#define RESTSIG sigaction(SIGPIPE,&oact,NULL)

#define MAX_LINE     1024
#define PARENT_READ  readpipe[0]
#define CHILD_WRITE  readpipe[1]
#define CHILD_READ   sendpipe[0]
#define PARENT_WRITE sendpipe[1]

#if defined (__sun__)
#define ADDR2LINE_PROG        "/usr/gnu/bin/addr2line"
#else
#define ADDR2LINE_PROG        "/usr/bin/addr2line"
#endif

void
convert_addresses (const char *file_name,
                   void *addrs,
                   int   n_addr,
                   void *buf,
                   int  *len)
{
  int max_len = *len;
  pid_t childpid;

  struct sigaction act, oact;

  int sendpipe[2] = {-1,-1},        /* parent -> child */
      readpipe[2] = {-1,-1};        /* parent <- child */

  *len = 0;
  act.sa_handler = SIG_IGN;
  sigemptyset(&act.sa_mask);
  act.sa_flags = 0;
  if (sigaction(SIGPIPE,&act,&oact) < 0)
    return;

  if (pipe(sendpipe) < 0) { RESTSIG; return; }
  if (pipe(readpipe) < 0) { CLOSE_SENDPIPE; RESTSIG; return; }
  if ((childpid = fork()) < 0) {
    CLOSE_READPIPE;
    CLOSE_SENDPIPE;
    RESTSIG;
    return;
  }

  if (childpid == 0) {    /* child process */
    close(PARENT_WRITE);
    close(PARENT_READ);
    if ((CHILD_READ != STDIN_FILENO) && (CHILD_WRITE != STDOUT_FILENO)) {
      if ((CHILD_READ == STDOUT_FILENO) && (CHILD_WRITE == STDIN_FILENO)) {
        const int temp_fd = dup(CHILD_WRITE);
        close (CHILD_WRITE);
        DUP2CLOSE (CHILD_READ, STDIN_FILENO);
        DUP2CLOSE (temp_fd,    STDOUT_FILENO);
      }
      else if ((CHILD_READ == STDIN_FILENO) && (CHILD_WRITE > 1)) {
        DUP2CLOSE (CHILD_WRITE, STDOUT_FILENO);
      }
      else if ((CHILD_READ > 1) && (CHILD_WRITE == STDOUT_FILENO)) {
        DUP2CLOSE (CHILD_READ, STDIN_FILENO);
      }
      else if ((CHILD_READ > 1) && (CHILD_WRITE == STDIN_FILENO)) {
        DUP2CLOSE (CHILD_WRITE, STDOUT_FILENO);
        DUP2CLOSE (CHILD_READ,  STDIN_FILENO);
      }
      else {
        /* CHILD_READ >= 1 and CHILD_WRITE > 1 */
        DUP2CLOSE (CHILD_READ,  STDIN_FILENO);
        DUP2CLOSE (CHILD_WRITE, STDOUT_FILENO);
      }
    }
    /* As pointed out by Florian Weimer to JP, it is a security threat to call
       the script with a user defined environment and using the path. That
       would be Trojans pleasure.  Therefore the absolute path to addr2line
       and an empty environment is used. That should be safe.
    */
    char *const argv[] = { "addr2line",
                           "-e", file_name,
                           "--demangle=gnat",
                           "--functions",
                           "--basenames",
                           NULL };
    char *const envp[] = { NULL };
    if (execve(ADDR2LINE_PROG, argv, envp) < 0) {
      close (CHILD_WRITE);
      close (CHILD_READ);
      RESTSIG;
      exit (1);
    }
  }

  /* Below this line is parent process */
  int i, n;
  char hex[16];
  char line[MAX_LINE + 1];
  char *p;
  char *s = buf;
  long *trace_address = addrs;

  close(CHILD_WRITE);
  close(CHILD_READ);

  for(i=0; i < n_addr; i++) {
    snprintf(hex,sizeof(hex),"%#lx\n",*trace_address);
    write(PARENT_WRITE,hex,strlen(hex));
    n = read(PARENT_READ,line,MAX_LINE);
    if (n<=0)
      break;

    line[n]=0;
    /* We have approx. 16 additional chars for "%#lx in " clause.
       We use this info to prevent a buffer overrun. */
    if (n + 16 + (*len) > max_len)
      break;

    p = strchr(line,'\n');
    if (p) {
      if (*(p+1)) {
        *p = 0;
        *len += snprintf(s, (max_len - (*len)), "%#lx in %s at %s",
                         *trace_address, line, p+1);
      }
      else {
        *len += snprintf(s, (max_len - (*len)), "%#lx at %s",
                         *trace_address, line);
      }
      s = buf + (*len);
    }
    trace_address += 1;
  }
  close (PARENT_WRITE);
  close (PARENT_READ);
  RESTSIG;
}

#endif
