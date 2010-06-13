--
--  DRACO ADA COMPILER
--  DRIVER COMPONENT
--
--
--  Copyright (c) 2010, John Marino (www.auroraux.org)
--  All rights reserved.
--
--  Permission to use, copy, modify, and/or distribute this software for any
--  purpose with or without fee is hereby granted, provided that the above
--  copyright notice and this permission notice appear in all copies.
--
--  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
--  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
--  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
--  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
--  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
--  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
--  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


with GNAT.OS_Lib;

package PipeMechanism is


   type Pipe_Type is record
      Input, Output : GNAT.OS_Lib.File_Descriptor;
   end record;


   procedure Pipe (
      program_1   : in  String;
      arg_string1 : in  String;
      program_2   : in  String;
      arg_string2 : in  String;
      success     : out Boolean
   );
   --  This procedure is the entire point of the package.  There was no
   --  specific "pipe" functionality in the GNAT library, but the components
   --  were there and this puts it all together.  Just pass the paths to two
   --  programs along with the arguments, and the output of program1 will be
   --  sent to the input stream of program2.


end PipeMechanism;

