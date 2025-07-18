/* "Bag-of-pages" garbage collector for the GNU compiler.
   Copyright (C) 1999-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "alias.h"
#include "tree.h"
#include "rtl.h"
#include "memmodel.h"
#include "tm_p.h"
#include "diagnostic-core.h"
#include "flags.h"
#include "ggc-internal.h"
#include "timevar.h"
#include "cgraph.h"
#include "cfgloop.h"
#include "plugin.h"

/* Prefer MAP_ANON(YMOUS) to /dev/zero, since we don't need to keep a
   file open.  Prefer either to valloc.  */
#ifdef HAVE_MMAP_ANON
# undef HAVE_MMAP_DEV_ZERO
# define USING_MMAP
#endif

#ifdef HAVE_MMAP_DEV_ZERO
# define USING_MMAP
#endif

#ifndef USING_MMAP
#define USING_MALLOC_PAGE_GROUPS
#endif

#if defined(HAVE_MADVISE) && HAVE_DECL_MADVISE && defined(MADV_DONTNEED) \
    && defined(USING_MMAP)
# define USING_MADVISE
#endif

/* Strategy:

   This garbage-collecting allocator allocates objects on one of a set
   of pages.  Each page can allocate objects of a single size only;
   available sizes are powers of two starting at four bytes.  The size
   of an allocation request is rounded up to the next power of two
   (`order'), and satisfied from the appropriate page.

   Each page is recorded in a page-entry, which also maintains an
   in-use bitmap of object positions on the page.  This allows the
   allocation state of a particular object to be flipped without
   touching the page itself.

   Each page-entry also has a context depth, which is used to track
   pushing and popping of allocation contexts.  Only objects allocated
   in the current (highest-numbered) context may be collected.

   Page entries are arranged in an array of singly-linked lists.  The
   array is indexed by the allocation size, in bits, of the pages on
   it; i.e. all pages on a list allocate objects of the same size.
   Pages are ordered on the list such that all non-full pages precede
   all full pages, with non-full pages arranged in order of decreasing
   context depth.

   Empty pages (of all orders) are kept on a single page cache list,
   and are considered first when new pages are required; they are
   deallocated at the start of the next collection if they haven't
   been recycled by then.  */

/* Define GGC_DEBUG_LEVEL to print debugging information.
     0: No debugging output.
     1: GC statistics only.
     2: Page-entry allocations/deallocations as well.
     3: Object allocations as well.
     4: Object marks as well.  */
#define GGC_DEBUG_LEVEL (0)

/* A two-level tree is used to look up the page-entry for a given
   pointer.  Two chunks of the pointer's bits are extracted to index
   the first and second levels of the tree, as follows:

				   HOST_PAGE_SIZE_BITS
			   32		|      |
       msb +----------------+----+------+------+ lsb
			    |    |      |
			 PAGE_L1_BITS   |
				 |      |
			       PAGE_L2_BITS

   The bottommost HOST_PAGE_SIZE_BITS are ignored, since page-entry
   pages are aligned on system page boundaries.  The next most
   significant PAGE_L2_BITS and PAGE_L1_BITS are the second and first
   index values in the lookup table, respectively.

   For 32-bit architectures and the settings below, there are no
   leftover bits.  For architectures with wider pointers, the lookup
   tree points to a list of pages, which must be scanned to find the
   correct one.  */

#define PAGE_L1_BITS	(8)
#define PAGE_L2_BITS	(32 - PAGE_L1_BITS - G.lg_pagesize)
#define PAGE_L1_SIZE	((uintptr_t) 1 << PAGE_L1_BITS)
#define PAGE_L2_SIZE	((uintptr_t) 1 << PAGE_L2_BITS)

#define LOOKUP_L1(p) \
  (((uintptr_t) (p) >> (32 - PAGE_L1_BITS)) & ((1 << PAGE_L1_BITS) - 1))

#define LOOKUP_L2(p) \
  (((uintptr_t) (p) >> G.lg_pagesize) & ((1 << PAGE_L2_BITS) - 1))

/* The number of objects per allocation page, for objects on a page of
   the indicated ORDER.  */
#define OBJECTS_PER_PAGE(ORDER) objects_per_page_table[ORDER]

/* The number of objects in P.  */
#define OBJECTS_IN_PAGE(P) ((P)->bytes / OBJECT_SIZE ((P)->order))

/* The size of an object on a page of the indicated ORDER.  */
#define OBJECT_SIZE(ORDER) object_size_table[ORDER]

/* For speed, we avoid doing a general integer divide to locate the
   offset in the allocation bitmap, by precalculating numbers M, S
   such that (O * M) >> S == O / Z (modulo 2^32), for any offset O
   within the page which is evenly divisible by the object size Z.  */
#define DIV_MULT(ORDER) inverse_table[ORDER].mult
#define DIV_SHIFT(ORDER) inverse_table[ORDER].shift
#define OFFSET_TO_BIT(OFFSET, ORDER) \
  (((OFFSET) * DIV_MULT (ORDER)) >> DIV_SHIFT (ORDER))

/* We use this structure to determine the alignment required for
   allocations.  For power-of-two sized allocations, that's not a
   problem, but it does matter for odd-sized allocations.
   We do not care about alignment for floating-point types.  */

struct max_alignment {
  char c;
  union {
    int64_t i;
    void *p;
  } u;
};

/* The biggest alignment required.  */

#define MAX_ALIGNMENT (offsetof (struct max_alignment, u))


/* The number of extra orders, not corresponding to power-of-two sized
   objects.  */

#define NUM_EXTRA_ORDERS ARRAY_SIZE (extra_order_size_table)

#define RTL_SIZE(NSLOTS) \
  (RTX_HDR_SIZE + (NSLOTS) * sizeof (rtunion))

#define TREE_EXP_SIZE(OPS) \
  (sizeof (struct tree_exp) + ((OPS) - 1) * sizeof (tree))

/* The Ith entry is the maximum size of an object to be stored in the
   Ith extra order.  Adding a new entry to this array is the *only*
   thing you need to do to add a new special allocation size.  */

static const size_t extra_order_size_table[] = {
  /* Extra orders for small non-power-of-two multiples of MAX_ALIGNMENT.
     There are a lot of structures with these sizes and explicitly
     listing them risks orders being dropped because they changed size.  */
  MAX_ALIGNMENT * 3,
  MAX_ALIGNMENT * 5,
  MAX_ALIGNMENT * 6,
  MAX_ALIGNMENT * 7,
  MAX_ALIGNMENT * 9,
  MAX_ALIGNMENT * 10,
  MAX_ALIGNMENT * 11,
  MAX_ALIGNMENT * 12,
  MAX_ALIGNMENT * 13,
  MAX_ALIGNMENT * 14,
  MAX_ALIGNMENT * 15,
  sizeof (struct tree_decl_non_common),
  sizeof (struct tree_field_decl),
  sizeof (struct tree_parm_decl),
  sizeof (struct tree_var_decl),
  sizeof (struct tree_type_non_common),
  sizeof (struct function),
  sizeof (struct basic_block_def),
  sizeof (struct cgraph_node),
  sizeof (class loop),
};

/* The total number of orders.  */

#define NUM_ORDERS (HOST_BITS_PER_PTR + NUM_EXTRA_ORDERS)

/* Compute the smallest nonnegative number which when added to X gives
   a multiple of F.  */

#define ROUND_UP_VALUE(x, f) ((f) - 1 - ((f) - 1 + (x)) % (f))

/* Round X to next multiple of the page size */

#define PAGE_ALIGN(x) ROUND_UP ((x), G.pagesize)

/* The Ith entry is the number of objects on a page or order I.  */

static unsigned objects_per_page_table[NUM_ORDERS];

/* The Ith entry is the size of an object on a page of order I.  */

static size_t object_size_table[NUM_ORDERS];

/* The Ith entry is a pair of numbers (mult, shift) such that
   ((k * mult) >> shift) mod 2^32 == (k / OBJECT_SIZE(I)) mod 2^32,
   for all k evenly divisible by OBJECT_SIZE(I).  */

static struct
{
  size_t mult;
  unsigned int shift;
}
inverse_table[NUM_ORDERS];

/* A page_entry records the status of an allocation page.  This
   structure is dynamically sized to fit the bitmap in_use_p.  */
struct page_entry
{
  /* The next page-entry with objects of the same size, or NULL if
     this is the last page-entry.  */
  struct page_entry *next;

  /* The previous page-entry with objects of the same size, or NULL if
     this is the first page-entry.   The PREV pointer exists solely to
     keep the cost of ggc_free manageable.  */
  struct page_entry *prev;

  /* The number of bytes allocated.  (This will always be a multiple
     of the host system page size.)  */
  size_t bytes;

  /* The address at which the memory is allocated.  */
  char *page;

#ifdef USING_MALLOC_PAGE_GROUPS
  /* Back pointer to the page group this page came from.  */
  struct page_group *group;
#endif

  /* This is the index in the by_depth varray where this page table
     can be found.  */
  unsigned long index_by_depth;

  /* Context depth of this page.  */
  unsigned short context_depth;

  /* The number of free objects remaining on this page.  */
  unsigned short num_free_objects;

  /* A likely candidate for the bit position of a free object for the
     next allocation from this page.  */
  unsigned short next_bit_hint;

  /* The lg of size of objects allocated from this page.  */
  unsigned char order;

  /* Discarded page? */
  bool discarded;

  /* A bit vector indicating whether or not objects are in use.  The
     Nth bit is one if the Nth object on this page is allocated.  This
     array is dynamically sized.  */
  unsigned long in_use_p[1];
};

#ifdef USING_MALLOC_PAGE_GROUPS
/* A page_group describes a large allocation from malloc, from which
   we parcel out aligned pages.  */
struct page_group
{
  /* A linked list of all extant page groups.  */
  struct page_group *next;

  /* The address we received from malloc.  */
  char *allocation;

  /* The size of the block.  */
  size_t alloc_size;

  /* A bitmask of pages in use.  */
  unsigned int in_use;
};
#endif

#if HOST_BITS_PER_PTR <= 32

/* On 32-bit hosts, we use a two level page table, as pictured above.  */
typedef page_entry **page_table[PAGE_L1_SIZE];

#else

/* On 64-bit hosts, we use the same two level page tables plus a linked
   list that disambiguates the top 32-bits.  There will almost always be
   exactly one entry in the list.  */
typedef struct page_table_chain
{
  struct page_table_chain *next;
  size_t high_bits;
  page_entry **table[PAGE_L1_SIZE];
} *page_table;

#endif

class finalizer
{
public:
  finalizer (void *addr, void (*f)(void *)) : m_addr (addr), m_function (f) {}

  void *addr () const { return m_addr; }

  void call () const { m_function (m_addr); }

private:
  void *m_addr;
  void (*m_function)(void *);
};

class vec_finalizer
{
public:
  vec_finalizer (uintptr_t addr, void (*f)(void *), size_t s, size_t n) :
    m_addr (addr), m_function (f), m_object_size (s), m_n_objects (n) {}

  void call () const
    {
      for (size_t i = 0; i < m_n_objects; i++)
	m_function (reinterpret_cast<void *> (m_addr + (i * m_object_size)));
    }

  void *addr () const { return reinterpret_cast<void *> (m_addr); }

private:
  uintptr_t m_addr;
  void (*m_function)(void *);
  size_t m_object_size;
  size_t m_n_objects;
};

#ifdef ENABLE_GC_ALWAYS_COLLECT
/* List of free objects to be verified as actually free on the
   next collection.  */
struct free_object
{
  void *object;
  struct free_object *next;
};
#endif

/* The rest of the global variables.  */
static struct ggc_globals
{
  /* The Nth element in this array is a page with objects of size 2^N.
     If there are any pages with free objects, they will be at the
     head of the list.  NULL if there are no page-entries for this
     object size.  */
  page_entry *pages[NUM_ORDERS];

  /* The Nth element in this array is the last page with objects of
     size 2^N.  NULL if there are no page-entries for this object
     size.  */
  page_entry *page_tails[NUM_ORDERS];

  /* Lookup table for associating allocation pages with object addresses.  */
  page_table lookup;

  /* The system's page size.  */
  size_t pagesize;
  size_t lg_pagesize;

  /* Bytes currently allocated.  */
  size_t allocated;

  /* Bytes currently allocated at the end of the last collection.  */
  size_t allocated_last_gc;

  /* Total amount of memory mapped.  */
  size_t bytes_mapped;

  /* Bit N set if any allocations have been done at context depth N.  */
  unsigned long context_depth_allocations;

  /* Bit N set if any collections have been done at context depth N.  */
  unsigned long context_depth_collections;

  /* The current depth in the context stack.  */
  unsigned short context_depth;

  /* A file descriptor open to /dev/zero for reading.  */
#if defined (HAVE_MMAP_DEV_ZERO)
  int dev_zero_fd;
#endif

  /* A cache of free system pages.  */
  page_entry *free_pages;

#ifdef USING_MALLOC_PAGE_GROUPS
  page_group *page_groups;
#endif

  /* The file descriptor for debugging output.  */
  FILE *debug_file;

  /* Current number of elements in use in depth below.  */
  unsigned int depth_in_use;

  /* Maximum number of elements that can be used before resizing.  */
  unsigned int depth_max;

  /* Each element of this array is an index in by_depth where the given
     depth starts.  This structure is indexed by that given depth we
     are interested in.  */
  unsigned int *depth;

  /* Current number of elements in use in by_depth below.  */
  unsigned int by_depth_in_use;

  /* Maximum number of elements that can be used before resizing.  */
  unsigned int by_depth_max;

  /* Each element of this array is a pointer to a page_entry, all
     page_entries can be found in here by increasing depth.
     index_by_depth in the page_entry is the index into this data
     structure where that page_entry can be found.  This is used to
     speed up finding all page_entries at a particular depth.  */
  page_entry **by_depth;

  /* Each element is a pointer to the saved in_use_p bits, if any,
     zero otherwise.  We allocate them all together, to enable a
     better runtime data access pattern.  */
  unsigned long **save_in_use;

  /* Finalizers for single objects.  The first index is collection_depth.  */
  vec<vec<finalizer> > finalizers;

  /* Finalizers for vectors of objects.  */
  vec<vec<vec_finalizer> > vec_finalizers;

#ifdef ENABLE_GC_ALWAYS_COLLECT
  /* List of free objects to be verified as actually free on the
     next collection.  */
  struct free_object *free_object_list;
#endif

  struct
  {
    /* Total GC-allocated memory.  */
    unsigned long long total_allocated;
    /* Total overhead for GC-allocated memory.  */
    unsigned long long total_overhead;

    /* Total allocations and overhead for sizes less than 32, 64 and 128.
       These sizes are interesting because they are typical cache line
       sizes.  */

    unsigned long long total_allocated_under32;
    unsigned long long total_overhead_under32;

    unsigned long long total_allocated_under64;
    unsigned long long total_overhead_under64;

    unsigned long long total_allocated_under128;
    unsigned long long total_overhead_under128;

    /* The allocations for each of the allocation orders.  */
    unsigned long long total_allocated_per_order[NUM_ORDERS];

    /* The overhead for each of the allocation orders.  */
    unsigned long long total_overhead_per_order[NUM_ORDERS];
  } stats;
} G;

/* True if a gc is currently taking place.  */

static bool in_gc = false;

/* The size in bytes required to maintain a bitmap for the objects
   on a page-entry.  */
#define BITMAP_SIZE(Num_objects) \
  (CEIL ((Num_objects), HOST_BITS_PER_LONG) * sizeof (long))

/* Allocate pages in chunks of this size, to throttle calls to memory
   allocation routines.  The first page is used, the rest go onto the
   free list.  This cannot be larger than HOST_BITS_PER_INT for the
   in_use bitmask for page_group.  Hosts that need a different value
   can override this by defining GGC_QUIRE_SIZE explicitly.  */
#ifndef GGC_QUIRE_SIZE
# ifdef USING_MMAP
#  define GGC_QUIRE_SIZE 512	/* 2MB for 4K pages */
# else
#  define GGC_QUIRE_SIZE 16
# endif
#endif

/* Initial guess as to how many page table entries we might need.  */
#define INITIAL_PTE_COUNT 128

static page_entry *lookup_page_table_entry (const void *);
static void set_page_table_entry (void *, page_entry *);
#ifdef USING_MMAP
static char *alloc_anon (char *, size_t, bool check);
#endif
#ifdef USING_MALLOC_PAGE_GROUPS
static size_t page_group_index (char *, char *);
static void set_page_group_in_use (page_group *, char *);
static void clear_page_group_in_use (page_group *, char *);
#endif
static struct page_entry * alloc_page (unsigned);
static void free_page (struct page_entry *);
static void clear_marks (void);
static void sweep_pages (void);
static void ggc_recalculate_in_use_p (page_entry *);
static void compute_inverse (unsigned);
static inline void adjust_depth (void);
static void move_ptes_to_front (int, int);

void debug_print_page_list (int);
static void push_depth (unsigned int);
static void push_by_depth (page_entry *, unsigned long *);

/* Push an entry onto G.depth.  */

inline static void
push_depth (unsigned int i)
{
  if (G.depth_in_use >= G.depth_max)
    {
      G.depth_max *= 2;
      G.depth = XRESIZEVEC (unsigned int, G.depth, G.depth_max);
    }
  G.depth[G.depth_in_use++] = i;
}

/* Push an entry onto G.by_depth and G.save_in_use.  */

inline static void
push_by_depth (page_entry *p, unsigned long *s)
{
  if (G.by_depth_in_use >= G.by_depth_max)
    {
      G.by_depth_max *= 2;
      G.by_depth = XRESIZEVEC (page_entry *, G.by_depth, G.by_depth_max);
      G.save_in_use = XRESIZEVEC (unsigned long *, G.save_in_use,
				  G.by_depth_max);
    }
  G.by_depth[G.by_depth_in_use] = p;
  G.save_in_use[G.by_depth_in_use++] = s;
}

#if (GCC_VERSION < 3001)
#define prefetch(X) ((void) X)
#else
#define prefetch(X) __builtin_prefetch (X)
#endif

#define save_in_use_p_i(__i) \
  (G.save_in_use[__i])
#define save_in_use_p(__p) \
  (save_in_use_p_i (__p->index_by_depth))

/* Traverse the page table and find the entry for a page.
   If the object wasn't allocated in GC return NULL.  */

static inline page_entry *
safe_lookup_page_table_entry (const void *p)
{
  page_entry ***base;
  size_t L1, L2;

#if HOST_BITS_PER_PTR <= 32
  base = &G.lookup[0];
#else
  page_table table = G.lookup;
  uintptr_t high_bits = (uintptr_t) p & ~ (uintptr_t) 0xffffffff;
  while (1)
    {
      if (table == NULL)
	return NULL;
      if (table->high_bits == high_bits)
	break;
      table = table->next;
    }
  base = &table->table[0];
#endif

  /* Extract the level 1 and 2 indices.  */
  L1 = LOOKUP_L1 (p);
  L2 = LOOKUP_L2 (p);
  if (! base[L1])
    return NULL;

  return base[L1][L2];
}

/* Traverse the page table and find the entry for a page.
   Die (probably) if the object wasn't allocated via GC.  */

static inline page_entry *
lookup_page_table_entry (const void *p)
{
  page_entry ***base;
  size_t L1, L2;

#if HOST_BITS_PER_PTR <= 32
  base = &G.lookup[0];
#else
  page_table table = G.lookup;
  uintptr_t high_bits = (uintptr_t) p & ~ (uintptr_t) 0xffffffff;
  while (table->high_bits != high_bits)
    table = table->next;
  base = &table->table[0];
#endif

  /* Extract the level 1 and 2 indices.  */
  L1 = LOOKUP_L1 (p);
  L2 = LOOKUP_L2 (p);

  return base[L1][L2];
}

/* Set the page table entry for a page.  */

static void
set_page_table_entry (void *p, page_entry *entry)
{
  page_entry ***base;
  size_t L1, L2;

#if HOST_BITS_PER_PTR <= 32
  base = &G.lookup[0];
#else
  page_table table;
  uintptr_t high_bits = (uintptr_t) p & ~ (uintptr_t) 0xffffffff;
  for (table = G.lookup; table; table = table->next)
    if (table->high_bits == high_bits)
      goto found;

  /* Not found -- allocate a new table.  */
  table = XCNEW (struct page_table_chain);
  table->next = G.lookup;
  table->high_bits = high_bits;
  G.lookup = table;
found:
  base = &table->table[0];
#endif

  /* Extract the level 1 and 2 indices.  */
  L1 = LOOKUP_L1 (p);
  L2 = LOOKUP_L2 (p);

  if (base[L1] == NULL)
    base[L1] = XCNEWVEC (page_entry *, PAGE_L2_SIZE);

  base[L1][L2] = entry;
}

/* Prints the page-entry for object size ORDER, for debugging.  */

DEBUG_FUNCTION void
debug_print_page_list (int order)
{
  page_entry *p;
  printf ("Head=%p, Tail=%p:\n", (void *) G.pages[order],
	  (void *) G.page_tails[order]);
  p = G.pages[order];
  while (p != NULL)
    {
      printf ("%p(%1d|%3d) -> ", (void *) p, p->context_depth,
	      p->num_free_objects);
      p = p->next;
    }
  printf ("NULL\n");
  fflush (stdout);
}

#ifdef USING_MMAP
/* Allocate SIZE bytes of anonymous memory, preferably near PREF,
   (if non-null).  The ifdef structure here is intended to cause a
   compile error unless exactly one of the HAVE_* is defined.  */

static inline char *
alloc_anon (char *pref ATTRIBUTE_UNUSED, size_t size, bool check)
{
#ifdef HAVE_MMAP_ANON
  char *page = (char *) mmap (pref, size, PROT_READ | PROT_WRITE,
			      MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
#endif
#ifdef HAVE_MMAP_DEV_ZERO
  char *page = (char *) mmap (pref, size, PROT_READ | PROT_WRITE,
			      MAP_PRIVATE, G.dev_zero_fd, 0);
#endif

  if (page == (char *) MAP_FAILED)
    {
      if (!check)
        return NULL;
      perror ("virtual memory exhausted");
      exit (FATAL_EXIT_CODE);
    }

  /* Remember that we allocated this memory.  */
  G.bytes_mapped += size;

  /* Pretend we don't have access to the allocated pages.  We'll enable
     access to smaller pieces of the area in ggc_internal_alloc.  Discard the
     handle to avoid handle leak.  */
  VALGRIND_DISCARD (VALGRIND_MAKE_MEM_NOACCESS (page, size));

  return page;
}
#endif
#ifdef USING_MALLOC_PAGE_GROUPS
/* Compute the index for this page into the page group.  */

static inline size_t
page_group_index (char *allocation, char *page)
{
  return (size_t) (page - allocation) >> G.lg_pagesize;
}

/* Set and clear the in_use bit for this page in the page group.  */

static inline void
set_page_group_in_use (page_group *group, char *page)
{
  group->in_use |= 1 << page_group_index (group->allocation, page);
}

static inline void
clear_page_group_in_use (page_group *group, char *page)
{
  group->in_use &= ~(1 << page_group_index (group->allocation, page));
}
#endif

/* Allocate a new page for allocating objects of size 2^ORDER,
   and return an entry for it.  The entry is not added to the
   appropriate page_table list.  */

static inline struct page_entry *
alloc_page (unsigned order)
{
  struct page_entry *entry, *p, **pp;
  char *page;
  size_t num_objects;
  size_t bitmap_size;
  size_t page_entry_size;
  size_t entry_size;
#ifdef USING_MALLOC_PAGE_GROUPS
  page_group *group;
#endif

  num_objects = OBJECTS_PER_PAGE (order);
  bitmap_size = BITMAP_SIZE (num_objects + 1);
  page_entry_size = sizeof (page_entry) - sizeof (long) + bitmap_size;
  entry_size = num_objects * OBJECT_SIZE (order);
  if (entry_size < G.pagesize)
    entry_size = G.pagesize;
  entry_size = PAGE_ALIGN (entry_size);

  entry = NULL;
  page = NULL;

  /* Check the list of free pages for one we can use.  */
  for (pp = &G.free_pages, p = *pp; p; pp = &p->next, p = *pp)
    if (p->bytes == entry_size)
      break;

  if (p != NULL)
    {
      if (p->discarded)
        G.bytes_mapped += p->bytes;
      p->discarded = false;

      /* Recycle the allocated memory from this page ...  */
      *pp = p->next;
      page = p->page;

#ifdef USING_MALLOC_PAGE_GROUPS
      group = p->group;
#endif

      /* ... and, if possible, the page entry itself.  */
      if (p->order == order)
	{
	  entry = p;
	  memset (entry, 0, page_entry_size);
	}
      else
	free (p);
    }
#ifdef USING_MMAP
  else if (entry_size == G.pagesize)
    {
      /* We want just one page.  Allocate a bunch of them and put the
	 extras on the freelist.  (Can only do this optimization with
	 mmap for backing store.)  */
      struct page_entry *e, *f = G.free_pages;
      int i, entries = GGC_QUIRE_SIZE;

      page = alloc_anon (NULL, G.pagesize * GGC_QUIRE_SIZE, false);
      if (page == NULL)
     	{
	  page = alloc_anon (NULL, G.pagesize, true);
          entries = 1;
	}

      /* This loop counts down so that the chain will be in ascending
	 memory order.  */
      for (i = entries - 1; i >= 1; i--)
	{
	  e = XCNEWVAR (struct page_entry, page_entry_size);
	  e->order = order;
	  e->bytes = G.pagesize;
	  e->page = page + (i << G.lg_pagesize);
	  e->next = f;
	  f = e;
	}

      G.free_pages = f;
    }
  else
    page = alloc_anon (NULL, entry_size, true);
#endif
#ifdef USING_MALLOC_PAGE_GROUPS
  else
    {
      /* Allocate a large block of memory and serve out the aligned
	 pages therein.  This results in much less memory wastage
	 than the traditional implementation of valloc.  */

      char *allocation, *a, *enda;
      size_t alloc_size, head_slop, tail_slop;
      int multiple_pages = (entry_size == G.pagesize);

      if (multiple_pages)
	alloc_size = GGC_QUIRE_SIZE * G.pagesize;
      else
	alloc_size = entry_size + G.pagesize - 1;
      allocation = XNEWVEC (char, alloc_size);

      page = (char *) (((uintptr_t) allocation + G.pagesize - 1) & -G.pagesize);
      head_slop = page - allocation;
      if (multiple_pages)
	tail_slop = ((size_t) allocation + alloc_size) & (G.pagesize - 1);
      else
	tail_slop = alloc_size - entry_size - head_slop;
      enda = allocation + alloc_size - tail_slop;

      /* We allocated N pages, which are likely not aligned, leaving
	 us with N-1 usable pages.  We plan to place the page_group
	 structure somewhere in the slop.  */
      if (head_slop >= sizeof (page_group))
	group = (page_group *)page - 1;
      else
	{
	  /* We magically got an aligned allocation.  Too bad, we have
	     to waste a page anyway.  */
	  if (tail_slop == 0)
	    {
	      enda -= G.pagesize;
	      tail_slop += G.pagesize;
	    }
	  gcc_assert (tail_slop >= sizeof (page_group));
	  group = (page_group *)enda;
	  tail_slop -= sizeof (page_group);
	}

      /* Remember that we allocated this memory.  */
      group->next = G.page_groups;
      group->allocation = allocation;
      group->alloc_size = alloc_size;
      group->in_use = 0;
      G.page_groups = group;
      G.bytes_mapped += alloc_size;

      /* If we allocated multiple pages, put the rest on the free list.  */
      if (multiple_pages)
	{
	  struct page_entry *e, *f = G.free_pages;
	  for (a = enda - G.pagesize; a != page; a -= G.pagesize)
	    {
	      e = XCNEWVAR (struct page_entry, page_entry_size);
	      e->order = order;
	      e->bytes = G.pagesize;
	      e->page = a;
	      e->group = group;
	      e->next = f;
	      f = e;
	    }
	  G.free_pages = f;
	}
    }
#endif

  if (entry == NULL)
    entry = XCNEWVAR (struct page_entry, page_entry_size);

  entry->bytes = entry_size;
  entry->page = page;
  entry->context_depth = G.context_depth;
  entry->order = order;
  entry->num_free_objects = num_objects;
  entry->next_bit_hint = 1;

  G.context_depth_allocations |= (unsigned long)1 << G.context_depth;

#ifdef USING_MALLOC_PAGE_GROUPS
  entry->group = group;
  set_page_group_in_use (group, page);
#endif

  /* Set the one-past-the-end in-use bit.  This acts as a sentry as we
     increment the hint.  */
  entry->in_use_p[num_objects / HOST_BITS_PER_LONG]
    = (unsigned long) 1 << (num_objects % HOST_BITS_PER_LONG);

  set_page_table_entry (page, entry);

  if (GGC_DEBUG_LEVEL >= 2)
    fprintf (G.debug_file,
	     "Allocating page at %p, object size="
	     HOST_SIZE_T_PRINT_UNSIGNED ", data %p-%p\n",
	     (void *) entry, (fmt_size_t) OBJECT_SIZE (order),
	     (void *) page, (void *) (page + entry_size - 1));

  return entry;
}

/* Adjust the size of G.depth so that no index greater than the one
   used by the top of the G.by_depth is used.  */

static inline void
adjust_depth (void)
{
  page_entry *top;

  if (G.by_depth_in_use)
    {
      top = G.by_depth[G.by_depth_in_use-1];

      /* Peel back indices in depth that index into by_depth, so that
	 as new elements are added to by_depth, we note the indices
	 of those elements, if they are for new context depths.  */
      while (G.depth_in_use > (size_t)top->context_depth+1)
	--G.depth_in_use;
    }
}

/* For a page that is no longer needed, put it on the free page list.  */

static void
free_page (page_entry *entry)
{
  if (GGC_DEBUG_LEVEL >= 2)
    fprintf (G.debug_file,
	     "Deallocating page at %p, data %p-%p\n", (void *) entry,
	     (void *) entry->page, (void *) (entry->page + entry->bytes - 1));

  /* Mark the page as inaccessible.  Discard the handle to avoid handle
     leak.  */
  VALGRIND_DISCARD (VALGRIND_MAKE_MEM_NOACCESS (entry->page, entry->bytes));

  set_page_table_entry (entry->page, NULL);

#ifdef USING_MALLOC_PAGE_GROUPS
  clear_page_group_in_use (entry->group, entry->page);
#endif

  if (G.by_depth_in_use > 1)
    {
      page_entry *top = G.by_depth[G.by_depth_in_use-1];
      int i = entry->index_by_depth;

      /* We cannot free a page from a context deeper than the current
	 one.  */
      gcc_assert (entry->context_depth == top->context_depth);

      /* Put top element into freed slot.  */
      G.by_depth[i] = top;
      G.save_in_use[i] = G.save_in_use[G.by_depth_in_use-1];
      top->index_by_depth = i;
    }
  --G.by_depth_in_use;

  adjust_depth ();

  entry->next = G.free_pages;
  G.free_pages = entry;
}

/* Release the free page cache to the system.  */

static void
release_pages (void)
{
  size_t n1 = 0;
  size_t n2 = 0;
#ifdef USING_MADVISE
  page_entry *p, *start_p;
  char *start;
  size_t len;
  size_t mapped_len;
  page_entry *next, *prev, *newprev;
  size_t free_unit = (GGC_QUIRE_SIZE/2) * G.pagesize;

  /* First free larger continuous areas to the OS.
     This allows other allocators to grab these areas if needed.
     This is only done on larger chunks to avoid fragmentation. 
     This does not always work because the free_pages list is only
     approximately sorted. */

  p = G.free_pages;
  prev = NULL;
  while (p)
    {
      start = p->page;
      start_p = p;
      len = 0;
      mapped_len = 0;
      newprev = prev;
      while (p && p->page == start + len)
        {
          len += p->bytes;
	  if (!p->discarded)
	      mapped_len += p->bytes;
	  newprev = p;
          p = p->next;
        }
      if (len >= free_unit)
        {
          while (start_p != p)
            {
              next = start_p->next;
              free (start_p);
              start_p = next;
            }
          munmap (start, len);
	  if (prev)
	    prev->next = p;
          else
            G.free_pages = p;
          G.bytes_mapped -= mapped_len;
	  n1 += len;
	  continue;
        }
      prev = newprev;
   }

  /* Now give back the fragmented pages to the OS, but keep the address 
     space to reuse it next time. */

  for (p = G.free_pages; p; )
    {
      if (p->discarded)
        {
          p = p->next;
          continue;
        }
      start = p->page;
      len = p->bytes;
      start_p = p;
      p = p->next;
      while (p && p->page == start + len)
        {
          len += p->bytes;
          p = p->next;
        }
      /* Give the page back to the kernel, but don't free the mapping.
         This avoids fragmentation in the virtual memory map of the 
 	 process. Next time we can reuse it by just touching it. */
      madvise (start, len, MADV_DONTNEED);
      /* Don't count those pages as mapped to not touch the garbage collector
         unnecessarily. */
      G.bytes_mapped -= len;
      n2 += len;
      while (start_p != p)
        {
          start_p->discarded = true;
          start_p = start_p->next;
        }
    }
#endif
#if defined(USING_MMAP) && !defined(USING_MADVISE)
  page_entry *p, *next;
  char *start;
  size_t len;

  /* Gather up adjacent pages so they are unmapped together.  */
  p = G.free_pages;

  while (p)
    {
      start = p->page;
      next = p->next;
      len = p->bytes;
      free (p);
      p = next;

      while (p && p->page == start + len)
	{
	  next = p->next;
	  len += p->bytes;
	  free (p);
	  p = next;
	}

      munmap (start, len);
      n1 += len;
      G.bytes_mapped -= len;
    }

  G.free_pages = NULL;
#endif
#ifdef USING_MALLOC_PAGE_GROUPS
  page_entry **pp, *p;
  page_group **gp, *g;

  /* Remove all pages from free page groups from the list.  */
  pp = &G.free_pages;
  while ((p = *pp) != NULL)
    if (p->group->in_use == 0)
      {
	*pp = p->next;
	free (p);
      }
    else
      pp = &p->next;

  /* Remove all free page groups, and release the storage.  */
  gp = &G.page_groups;
  while ((g = *gp) != NULL)
    if (g->in_use == 0)
      {
	*gp = g->next;
	G.bytes_mapped -= g->alloc_size;
	n1 += g->alloc_size;
	free (g->allocation);
      }
    else
      gp = &g->next;
#endif
  if (!quiet_flag && (n1 || n2))
    {
      fprintf (stderr, " {GC");
      if (n1)
	fprintf (stderr, " released " PRsa (0), SIZE_AMOUNT (n1));
      if (n2)
	fprintf (stderr, " madv_dontneed " PRsa (0), SIZE_AMOUNT (n2));
      fprintf (stderr, "}");
    }
}

/* This table provides a fast way to determine ceil(log_2(size)) for
   allocation requests.  The minimum allocation size is eight bytes.  */
#define NUM_SIZE_LOOKUP 512
static unsigned char size_lookup[NUM_SIZE_LOOKUP] =
{
  3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4,
  4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
  5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
  7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
  7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
  7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
  7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
  8, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
  9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
  9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
  9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
  9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
  9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
  9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
  9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
  9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
  9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
  9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
  9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
  9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
  9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
  9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
  9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
};

/* For a given size of memory requested for allocation, return the
   actual size that is going to be allocated, as well as the size
   order.  */

static void
ggc_round_alloc_size_1 (size_t requested_size,
			size_t *size_order,
			size_t *alloced_size)
{
  size_t order, object_size;

  if (requested_size < NUM_SIZE_LOOKUP)
    {
      order = size_lookup[requested_size];
      object_size = OBJECT_SIZE (order);
    }
  else
    {
      order = 10;
      while (requested_size > (object_size = OBJECT_SIZE (order)))
        order++;
    }

  if (size_order)
    *size_order = order;
  if (alloced_size)
    *alloced_size = object_size;
}

/* For a given size of memory requested for allocation, return the
   actual size that is going to be allocated.  */

size_t
ggc_round_alloc_size (size_t requested_size)
{
  size_t size = 0;
  
  ggc_round_alloc_size_1 (requested_size, NULL, &size);
  return size;
}

/* Push a finalizer onto the appropriate vec.  */

static void
add_finalizer (void *result, void (*f)(void *), size_t s, size_t n)
{
  if (f == NULL)
    /* No finalizer.  */;
  else if (n == 1)
    {
      finalizer fin (result, f);
      G.finalizers[G.context_depth].safe_push (fin);
    }
  else
    {
      vec_finalizer fin (reinterpret_cast<uintptr_t> (result), f, s, n);
      G.vec_finalizers[G.context_depth].safe_push (fin);
    }
}

/* Allocate a chunk of memory of SIZE bytes.  Its contents are undefined.  */

extern "C" void *
ggc_internal_alloc___ (size_t size, void (*f)(void *), size_t s, size_t n
		       MEM_STAT_DECL)
{
  size_t order, word, bit, object_offset, object_size;
  struct page_entry *entry;
  void *result;

  ggc_round_alloc_size_1 (size, &order, &object_size);

  /* If there are non-full pages for this size allocation, they are at
     the head of the list.  */
  entry = G.pages[order];

  /* If there is no page for this object size, or all pages in this
     context are full, allocate a new page.  */
  if (entry == NULL || entry->num_free_objects == 0)
    {
      struct page_entry *new_entry;
      new_entry = alloc_page (order);

      new_entry->index_by_depth = G.by_depth_in_use;
      push_by_depth (new_entry, 0);

      /* We can skip context depths, if we do, make sure we go all the
	 way to the new depth.  */
      while (new_entry->context_depth >= G.depth_in_use)
	push_depth (G.by_depth_in_use-1);

      /* If this is the only entry, it's also the tail.  If it is not
	 the only entry, then we must update the PREV pointer of the
	 ENTRY (G.pages[order]) to point to our new page entry.  */
      if (entry == NULL)
	G.page_tails[order] = new_entry;
      else
	entry->prev = new_entry;

      /* Put new pages at the head of the page list.  By definition the
	 entry at the head of the list always has a NULL pointer.  */
      new_entry->next = entry;
      new_entry->prev = NULL;
      entry = new_entry;
      G.pages[order] = new_entry;

      /* For a new page, we know the word and bit positions (in the
	 in_use bitmap) of the first available object -- they're zero.  */
      new_entry->next_bit_hint = 1;
      word = 0;
      bit = 0;
      object_offset = 0;
    }
  else
    {
      /* First try to use the hint left from the previous allocation
	 to locate a clear bit in the in-use bitmap.  We've made sure
	 that the one-past-the-end bit is always set, so if the hint
	 has run over, this test will fail.  */
      unsigned hint = entry->next_bit_hint;
      word = hint / HOST_BITS_PER_LONG;
      bit = hint % HOST_BITS_PER_LONG;

      /* If the hint didn't work, scan the bitmap from the beginning.  */
      if ((entry->in_use_p[word] >> bit) & 1)
	{
	  word = bit = 0;
	  while (~entry->in_use_p[word] == 0)
	    ++word;

#if GCC_VERSION >= 3004
	  bit = __builtin_ctzl (~entry->in_use_p[word]);
#else
	  while ((entry->in_use_p[word] >> bit) & 1)
	    ++bit;
#endif

	  hint = word * HOST_BITS_PER_LONG + bit;
	}

      /* Next time, try the next bit.  */
      entry->next_bit_hint = hint + 1;

      object_offset = hint * object_size;
    }

  /* Set the in-use bit.  */
  entry->in_use_p[word] |= ((unsigned long) 1 << bit);

  /* Keep a running total of the number of free objects.  If this page
     fills up, we may have to move it to the end of the list if the
     next page isn't full.  If the next page is full, all subsequent
     pages are full, so there's no need to move it.  */
  if (--entry->num_free_objects == 0
      && entry->next != NULL
      && entry->next->num_free_objects > 0)
    {
      /* We have a new head for the list.  */
      G.pages[order] = entry->next;

      /* We are moving ENTRY to the end of the page table list.
	 The new page at the head of the list will have NULL in
	 its PREV field and ENTRY will have NULL in its NEXT field.  */
      entry->next->prev = NULL;
      entry->next = NULL;

      /* Append ENTRY to the tail of the list.  */
      entry->prev = G.page_tails[order];
      G.page_tails[order]->next = entry;
      G.page_tails[order] = entry;
    }

  /* Calculate the object's address.  */
  result = entry->page + object_offset;
  if (GATHER_STATISTICS)
    ggc_record_overhead (OBJECT_SIZE (order), OBJECT_SIZE (order) - size,
			 result FINAL_PASS_MEM_STAT);

#ifdef ENABLE_GC_CHECKING
  /* Keep poisoning-by-writing-0xaf the object, in an attempt to keep the
     exact same semantics in presence of memory bugs, regardless of
     ENABLE_VALGRIND_CHECKING.  We override this request below.  Drop the
     handle to avoid handle leak.  */
  VALGRIND_DISCARD (VALGRIND_MAKE_MEM_UNDEFINED (result, object_size));

  /* `Poison' the entire allocated object, including any padding at
     the end.  */
  memset (result, 0xaf, object_size);

  /* Make the bytes after the end of the object unaccessible.  Discard the
     handle to avoid handle leak.  */
  VALGRIND_DISCARD (VALGRIND_MAKE_MEM_NOACCESS ((char *) result + size,
						object_size - size));
#endif

  /* Tell Valgrind that the memory is there, but its content isn't
     defined.  The bytes at the end of the object are still marked
     unaccessible.  */
  VALGRIND_DISCARD (VALGRIND_MAKE_MEM_UNDEFINED (result, size));

  /* Keep track of how many bytes are being allocated.  This
     information is used in deciding when to collect.  */
  G.allocated += object_size;

  /* For timevar statistics.  */
  timevar_ggc_mem_total += object_size;

  if (f)
    add_finalizer (result, f, s, n);

  if (GATHER_STATISTICS)
    {
      size_t overhead = object_size - size;

      G.stats.total_overhead += overhead;
      G.stats.total_allocated += object_size;
      G.stats.total_overhead_per_order[order] += overhead;
      G.stats.total_allocated_per_order[order] += object_size;

      if (size <= 32)
	{
	  G.stats.total_overhead_under32 += overhead;
	  G.stats.total_allocated_under32 += object_size;
	}
      if (size <= 64)
	{
	  G.stats.total_overhead_under64 += overhead;
	  G.stats.total_allocated_under64 += object_size;
	}
      if (size <= 128)
	{
	  G.stats.total_overhead_under128 += overhead;
	  G.stats.total_allocated_under128 += object_size;
	}
    }

  if (GGC_DEBUG_LEVEL >= 3)
    fprintf (G.debug_file,
	     "Allocating object, requested size="
	     HOST_SIZE_T_PRINT_UNSIGNED ", actual=" HOST_SIZE_T_PRINT_UNSIGNED
	     " at %p on %p\n",
	     (fmt_size_t) size, (fmt_size_t) object_size, result,
	     (void *) entry);

  return result;
}

extern void *
ggc_internal_alloc (size_t size, void (*f)(void *), size_t s,
		    size_t n MEM_STAT_DECL)
     __attribute__((__alias__ ("ggc_internal_alloc___")));
extern void *
ggc_internal_alloc_no_dtor (size_t size, void (*f)(void *), size_t s,
			    size_t n MEM_STAT_DECL)
     __attribute__((__alias__ ("ggc_internal_alloc___")));

/* Mark function for strings.  */

void
gt_ggc_m_S (const void *p)
{
  page_entry *entry;
  unsigned bit, word;
  unsigned long mask;
  unsigned long offset;

  if (!p)
    return;

  /* Look up the page on which the object is alloced.  If it was not
     GC allocated, gracefully bail out.  */
  entry = safe_lookup_page_table_entry (p);
  if (!entry)
    return;

  /* Calculate the index of the object on the page; this is its bit
     position in the in_use_p bitmap.  Note that because a char* might
     point to the middle of an object, we need special code here to
     make sure P points to the start of an object.  */
  offset = ((const char *) p - entry->page) % object_size_table[entry->order];
  if (offset)
    {
      /* Here we've seen a char* which does not point to the beginning
	 of an allocated object.  We assume it points to the middle of
	 a STRING_CST.  */
      gcc_assert (offset == offsetof (struct tree_string, str));
      p = ((const char *) p) - offset;
      gt_ggc_mx_lang_tree_node (CONST_CAST (void *, p));
      return;
    }

  bit = OFFSET_TO_BIT (((const char *) p) - entry->page, entry->order);
  word = bit / HOST_BITS_PER_LONG;
  mask = (unsigned long) 1 << (bit % HOST_BITS_PER_LONG);

  /* If the bit was previously set, skip it.  */
  if (entry->in_use_p[word] & mask)
    return;

  /* Otherwise set it, and decrement the free object count.  */
  entry->in_use_p[word] |= mask;
  entry->num_free_objects -= 1;

  if (GGC_DEBUG_LEVEL >= 4)
    fprintf (G.debug_file, "Marking %p\n", p);

  return;
}


/* User-callable entry points for marking string X.  */

void
gt_ggc_mx (const char *& x)
{
  gt_ggc_m_S (x);
}

void
gt_ggc_mx (char *& x)
{
  gt_ggc_m_S (x);
}

void
gt_ggc_mx (unsigned char *& x)
{
  gt_ggc_m_S (x);
}

void
gt_ggc_mx (unsigned char& x ATTRIBUTE_UNUSED)
{
}

/* If P is not marked, marks it and return false.  Otherwise return true.
   P must have been allocated by the GC allocator; it mustn't point to
   static objects, stack variables, or memory allocated with malloc.  */

bool
ggc_set_mark (const void *p)
{
  page_entry *entry;
  unsigned bit, word;
  unsigned long mask;

  /* Look up the page on which the object is alloced.  If the object
     wasn't allocated by the collector, we'll probably die.  */
  entry = lookup_page_table_entry (p);
  gcc_assert (entry);

  /* Calculate the index of the object on the page; this is its bit
     position in the in_use_p bitmap.  */
  bit = OFFSET_TO_BIT (((const char *) p) - entry->page, entry->order);
  word = bit / HOST_BITS_PER_LONG;
  mask = (unsigned long) 1 << (bit % HOST_BITS_PER_LONG);

  /* If the bit was previously set, skip it.  */
  if (entry->in_use_p[word] & mask)
    return true;

  /* Otherwise set it, and decrement the free object count.  */
  entry->in_use_p[word] |= mask;
  entry->num_free_objects -= 1;

  if (GGC_DEBUG_LEVEL >= 4)
    fprintf (G.debug_file, "Marking %p\n", p);

  return false;
}

/* Return true if P has been marked, zero otherwise.
   P must have been allocated by the GC allocator; it mustn't point to
   static objects, stack variables, or memory allocated with malloc.  */

bool
ggc_marked_p (const void *p)
{
  page_entry *entry;
  unsigned bit, word;
  unsigned long mask;

  /* Look up the page on which the object is alloced.  If the object
     wasn't allocated by the collector, we'll probably die.  */
  entry = lookup_page_table_entry (p);
  gcc_assert (entry);

  /* Calculate the index of the object on the page; this is its bit
     position in the in_use_p bitmap.  */
  bit = OFFSET_TO_BIT (((const char *) p) - entry->page, entry->order);
  word = bit / HOST_BITS_PER_LONG;
  mask = (unsigned long) 1 << (bit % HOST_BITS_PER_LONG);

  return (entry->in_use_p[word] & mask) != 0;
}

/* Return the size of the gc-able object P.  */

size_t
ggc_get_size (const void *p)
{
  page_entry *pe = lookup_page_table_entry (p);
  return OBJECT_SIZE (pe->order);
}

/* Release the memory for object P.  */

void
ggc_free (void *p)
{
  if (in_gc)
    return;

  page_entry *pe = lookup_page_table_entry (p);
  size_t order = pe->order;
  size_t size = OBJECT_SIZE (order);

  if (GATHER_STATISTICS)
    ggc_free_overhead (p);

  if (GGC_DEBUG_LEVEL >= 3)
    fprintf (G.debug_file,
	     "Freeing object, actual size="
	     HOST_SIZE_T_PRINT_UNSIGNED ", at %p on %p\n",
	     (fmt_size_t) size, p, (void *) pe);

#ifdef ENABLE_GC_CHECKING
  /* Poison the data, to indicate the data is garbage.  */
  VALGRIND_DISCARD (VALGRIND_MAKE_MEM_UNDEFINED (p, size));
  memset (p, 0xa5, size);
#endif
  /* Let valgrind know the object is free.  */
  VALGRIND_DISCARD (VALGRIND_MAKE_MEM_NOACCESS (p, size));

#ifdef ENABLE_GC_ALWAYS_COLLECT
  /* In the completely-anal-checking mode, we do *not* immediately free
     the data, but instead verify that the data is *actually* not
     reachable the next time we collect.  */
  {
    struct free_object *fo = XNEW (struct free_object);
    fo->object = p;
    fo->next = G.free_object_list;
    G.free_object_list = fo;
  }
#else
  {
    unsigned int bit_offset, word, bit;

    G.allocated -= size;

    /* Mark the object not-in-use.  */
    bit_offset = OFFSET_TO_BIT (((const char *) p) - pe->page, order);
    word = bit_offset / HOST_BITS_PER_LONG;
    bit = bit_offset % HOST_BITS_PER_LONG;
    pe->in_use_p[word] &= ~(1UL << bit);

    if (pe->num_free_objects++ == 0)
      {
	page_entry *p, *q;

	/* If the page is completely full, then it's supposed to
	   be after all pages that aren't.  Since we've freed one
	   object from a page that was full, we need to move the
	   page to the head of the list.

	   PE is the node we want to move.  Q is the previous node
	   and P is the next node in the list.  */
	q = pe->prev;
	if (q && q->num_free_objects == 0)
	  {
	    p = pe->next;

	    q->next = p;

	    /* If PE was at the end of the list, then Q becomes the
	       new end of the list.  If PE was not the end of the
	       list, then we need to update the PREV field for P.  */
	    if (!p)
	      G.page_tails[order] = q;
	    else
	      p->prev = q;

	    /* Move PE to the head of the list.  */
	    pe->next = G.pages[order];
	    pe->prev = NULL;
	    G.pages[order]->prev = pe;
	    G.pages[order] = pe;
	  }

	/* Reset the hint bit to point to the only free object.  */
	pe->next_bit_hint = bit_offset;
      }
  }
#endif
}

/* Subroutine of init_ggc which computes the pair of numbers used to
   perform division by OBJECT_SIZE (order) and fills in inverse_table[].

   This algorithm is taken from Granlund and Montgomery's paper
   "Division by Invariant Integers using Multiplication"
   (Proc. SIGPLAN PLDI, 1994), section 9 (Exact division by
   constants).  */

static void
compute_inverse (unsigned order)
{
  size_t size, inv;
  unsigned int e;

  size = OBJECT_SIZE (order);
  e = 0;
  while (size % 2 == 0)
    {
      e++;
      size >>= 1;
    }

  inv = size;
  while (inv * size != 1)
    inv = inv * (2 - inv*size);

  DIV_MULT (order) = inv;
  DIV_SHIFT (order) = e;
}

/* Initialize the ggc-mmap allocator.  */
void
init_ggc (void)
{
  static bool init_p = false;
  unsigned order;

  if (init_p)
    return;
  init_p = true;

  G.pagesize = getpagesize ();
  G.lg_pagesize = exact_log2 (G.pagesize);

#ifdef HAVE_MMAP_DEV_ZERO
  G.dev_zero_fd = open ("/dev/zero", O_RDONLY);
  if (G.dev_zero_fd == -1)
    internal_error ("open /dev/zero: %m");
#endif

#if 0
  G.debug_file = fopen ("ggc-mmap.debug", "w");
#else
  G.debug_file = stdout;
#endif

#ifdef USING_MMAP
  /* StunOS has an amazing off-by-one error for the first mmap allocation
     after fiddling with RLIMIT_STACK.  The result, as hard as it is to
     believe, is an unaligned page allocation, which would cause us to
     hork badly if we tried to use it.  */
  {
    char *p = alloc_anon (NULL, G.pagesize, true);
    struct page_entry *e;
    if ((uintptr_t)p & (G.pagesize - 1))
      {
	/* How losing.  Discard this one and try another.  If we still
	   can't get something useful, give up.  */

	p = alloc_anon (NULL, G.pagesize, true);
	gcc_assert (!((uintptr_t)p & (G.pagesize - 1)));
      }

    /* We have a good page, might as well hold onto it...  */
    e = XCNEW (struct page_entry);
    e->bytes = G.pagesize;
    e->page = p;
    e->next = G.free_pages;
    G.free_pages = e;
  }
#endif

  /* Initialize the object size table.  */
  for (order = 0; order < HOST_BITS_PER_PTR; ++order)
    object_size_table[order] = (size_t) 1 << order;
  for (order = HOST_BITS_PER_PTR; order < NUM_ORDERS; ++order)
    {
      size_t s = extra_order_size_table[order - HOST_BITS_PER_PTR];

      /* If S is not a multiple of the MAX_ALIGNMENT, then round it up
	 so that we're sure of getting aligned memory.  */
      s = ROUND_UP (s, MAX_ALIGNMENT);
      object_size_table[order] = s;
    }

  /* Initialize the objects-per-page and inverse tables.  */
  for (order = 0; order < NUM_ORDERS; ++order)
    {
      objects_per_page_table[order] = G.pagesize / OBJECT_SIZE (order);
      if (objects_per_page_table[order] == 0)
	objects_per_page_table[order] = 1;
      compute_inverse (order);
    }

  /* Reset the size_lookup array to put appropriately sized objects in
     the special orders.  All objects bigger than the previous power
     of two, but no greater than the special size, should go in the
     new order.  */
  for (order = HOST_BITS_PER_PTR; order < NUM_ORDERS; ++order)
    {
      int o;
      int i;

      i = OBJECT_SIZE (order);
      if (i >= NUM_SIZE_LOOKUP)
	continue;

      for (o = size_lookup[i]; o == size_lookup [i]; --i)
	size_lookup[i] = order;
    }

  G.depth_in_use = 0;
  G.depth_max = 10;
  G.depth = XNEWVEC (unsigned int, G.depth_max);

  G.by_depth_in_use = 0;
  G.by_depth_max = INITIAL_PTE_COUNT;
  G.by_depth = XNEWVEC (page_entry *, G.by_depth_max);
  G.save_in_use = XNEWVEC (unsigned long *, G.by_depth_max);

  /* Allocate space for the depth 0 finalizers.  */
  G.finalizers.safe_push (vNULL);
  G.vec_finalizers.safe_push (vNULL);
  gcc_assert (G.finalizers.length() == 1);
}

/* Merge the SAVE_IN_USE_P and IN_USE_P arrays in P so that IN_USE_P
   reflects reality.  Recalculate NUM_FREE_OBJECTS as well.  */

static void
ggc_recalculate_in_use_p (page_entry *p)
{
  unsigned int i;
  size_t num_objects;

  /* Because the past-the-end bit in in_use_p is always set, we
     pretend there is one additional object.  */
  num_objects = OBJECTS_IN_PAGE (p) + 1;

  /* Reset the free object count.  */
  p->num_free_objects = num_objects;

  /* Combine the IN_USE_P and SAVE_IN_USE_P arrays.  */
  for (i = 0;
       i < CEIL (BITMAP_SIZE (num_objects),
		 sizeof (*p->in_use_p));
       ++i)
    {
      unsigned long j;

      /* Something is in use if it is marked, or if it was in use in a
	 context further down the context stack.  */
      p->in_use_p[i] |= save_in_use_p (p)[i];

      /* Decrement the free object count for every object allocated.  */
      for (j = p->in_use_p[i]; j; j >>= 1)
	p->num_free_objects -= (j & 1);
    }

  gcc_assert (p->num_free_objects < num_objects);
}

/* Unmark all objects.  */

static void
clear_marks (void)
{
  unsigned order;

  for (order = 2; order < NUM_ORDERS; order++)
    {
      page_entry *p;

      for (p = G.pages[order]; p != NULL; p = p->next)
	{
	  size_t num_objects = OBJECTS_IN_PAGE (p);
	  size_t bitmap_size = BITMAP_SIZE (num_objects + 1);

	  /* The data should be page-aligned.  */
	  gcc_assert (!((uintptr_t) p->page & (G.pagesize - 1)));

	  /* Pages that aren't in the topmost context are not collected;
	     nevertheless, we need their in-use bit vectors to store GC
	     marks.  So, back them up first.  */
	  if (p->context_depth < G.context_depth)
	    {
	      if (! save_in_use_p (p))
		save_in_use_p (p) = XNEWVAR (unsigned long, bitmap_size);
	      memcpy (save_in_use_p (p), p->in_use_p, bitmap_size);
	    }

	  /* Reset reset the number of free objects and clear the
             in-use bits.  These will be adjusted by mark_obj.  */
	  p->num_free_objects = num_objects;
	  memset (p->in_use_p, 0, bitmap_size);

	  /* Make sure the one-past-the-end bit is always set.  */
	  p->in_use_p[num_objects / HOST_BITS_PER_LONG]
	    = ((unsigned long) 1 << (num_objects % HOST_BITS_PER_LONG));
	}
    }
}

/* Check if any blocks with a registered finalizer have become unmarked. If so
   run the finalizer and unregister it because the block is about to be freed.
   Note that no garantee is made about what order finalizers will run in so
   touching other objects in gc memory is extremely unwise.  */

static void
ggc_handle_finalizers ()
{
  unsigned dlen = G.finalizers.length();
  for (unsigned d = G.context_depth; d < dlen; ++d)
    {
      vec<finalizer> &v = G.finalizers[d];
      unsigned length = v.length ();
      for (unsigned int i = 0; i < length;)
	{
	  finalizer &f = v[i];
	  if (!ggc_marked_p (f.addr ()))
	    {
	      f.call ();
	      v.unordered_remove (i);
	      length--;
	    }
	  else
	    i++;
	}
    }

  gcc_assert (dlen == G.vec_finalizers.length());
  for (unsigned d = G.context_depth; d < dlen; ++d)
    {
      vec<vec_finalizer> &vv = G.vec_finalizers[d];
      unsigned length = vv.length ();
      for (unsigned int i = 0; i < length;)
	{
	  vec_finalizer &f = vv[i];
	  if (!ggc_marked_p (f.addr ()))
	    {
	      f.call ();
	      vv.unordered_remove (i);
	      length--;
	    }
	  else
	    i++;
	}
    }
}

/* Free all empty pages.  Partially empty pages need no attention
   because the `mark' bit doubles as an `unused' bit.  */

static void
sweep_pages (void)
{
  unsigned order;

  for (order = 2; order < NUM_ORDERS; order++)
    {
      /* The last page-entry to consider, regardless of entries
	 placed at the end of the list.  */
      page_entry * const last = G.page_tails[order];

      size_t num_objects;
      size_t live_objects;
      page_entry *p, *previous;
      int done;

      p = G.pages[order];
      if (p == NULL)
	continue;

      previous = NULL;
      do
	{
	  page_entry *next = p->next;

	  /* Loop until all entries have been examined.  */
	  done = (p == last);

	  num_objects = OBJECTS_IN_PAGE (p);

	  /* Add all live objects on this page to the count of
             allocated memory.  */
	  live_objects = num_objects - p->num_free_objects;

	  G.allocated += OBJECT_SIZE (order) * live_objects;

	  /* Only objects on pages in the topmost context should get
	     collected.  */
	  if (p->context_depth < G.context_depth)
	    ;

	  /* Remove the page if it's empty.  */
	  else if (live_objects == 0)
	    {
	      /* If P was the first page in the list, then NEXT
		 becomes the new first page in the list, otherwise
		 splice P out of the forward pointers.  */
	      if (! previous)
		G.pages[order] = next;
	      else
		previous->next = next;

	      /* Splice P out of the back pointers too.  */
	      if (next)
		next->prev = previous;

	      /* Are we removing the last element?  */
	      if (p == G.page_tails[order])
		G.page_tails[order] = previous;
	      free_page (p);
	      p = previous;
	    }

	  /* If the page is full, move it to the end.  */
	  else if (p->num_free_objects == 0)
	    {
	      /* Don't move it if it's already at the end.  */
	      if (p != G.page_tails[order])
		{
		  /* Move p to the end of the list.  */
		  p->next = NULL;
		  p->prev = G.page_tails[order];
		  G.page_tails[order]->next = p;

		  /* Update the tail pointer...  */
		  G.page_tails[order] = p;

		  /* ... and the head pointer, if necessary.  */
		  if (! previous)
		    G.pages[order] = next;
		  else
		    previous->next = next;

		  /* And update the backpointer in NEXT if necessary.  */
		  if (next)
		    next->prev = previous;

		  p = previous;
		}
	    }

	  /* If we've fallen through to here, it's a page in the
	     topmost context that is neither full nor empty.  Such a
	     page must precede pages at lesser context depth in the
	     list, so move it to the head.  */
	  else if (p != G.pages[order])
	    {
	      previous->next = p->next;

	      /* Update the backchain in the next node if it exists.  */
	      if (p->next)
		p->next->prev = previous;

	      /* Move P to the head of the list.  */
	      p->next = G.pages[order];
	      p->prev = NULL;
	      G.pages[order]->prev = p;

	      /* Update the head pointer.  */
	      G.pages[order] = p;

	      /* Are we moving the last element?  */
	      if (G.page_tails[order] == p)
	        G.page_tails[order] = previous;
	      p = previous;
	    }

	  previous = p;
	  p = next;
	}
      while (! done);

      /* Now, restore the in_use_p vectors for any pages from contexts
         other than the current one.  */
      for (p = G.pages[order]; p; p = p->next)
	if (p->context_depth != G.context_depth)
	  ggc_recalculate_in_use_p (p);
    }
}

#ifdef ENABLE_GC_CHECKING
/* Clobber all free objects.  */

static void
poison_pages (void)
{
  unsigned order;

  for (order = 2; order < NUM_ORDERS; order++)
    {
      size_t size = OBJECT_SIZE (order);
      page_entry *p;

      for (p = G.pages[order]; p != NULL; p = p->next)
	{
	  size_t num_objects;
	  size_t i;

	  if (p->context_depth != G.context_depth)
	    /* Since we don't do any collection for pages in pushed
	       contexts, there's no need to do any poisoning.  And
	       besides, the IN_USE_P array isn't valid until we pop
	       contexts.  */
	    continue;

	  num_objects = OBJECTS_IN_PAGE (p);
	  for (i = 0; i < num_objects; i++)
	    {
	      size_t word, bit;
	      word = i / HOST_BITS_PER_LONG;
	      bit = i % HOST_BITS_PER_LONG;
	      if (((p->in_use_p[word] >> bit) & 1) == 0)
		{
		  char *object = p->page + i * size;

		  /* Keep poison-by-write when we expect to use Valgrind,
		     so the exact same memory semantics is kept, in case
		     there are memory errors.  We override this request
		     below.  */
		  VALGRIND_DISCARD (VALGRIND_MAKE_MEM_UNDEFINED (object,
								 size));
		  memset (object, 0xa5, size);

		  /* Drop the handle to avoid handle leak.  */
		  VALGRIND_DISCARD (VALGRIND_MAKE_MEM_NOACCESS (object, size));
		}
	    }
	}
    }
}
#else
#define poison_pages()
#endif

#ifdef ENABLE_GC_ALWAYS_COLLECT
/* Validate that the reportedly free objects actually are.  */

static void
validate_free_objects (void)
{
  struct free_object *f, *next, *still_free = NULL;

  for (f = G.free_object_list; f ; f = next)
    {
      page_entry *pe = lookup_page_table_entry (f->object);
      size_t bit, word;

      bit = OFFSET_TO_BIT ((char *)f->object - pe->page, pe->order);
      word = bit / HOST_BITS_PER_LONG;
      bit = bit % HOST_BITS_PER_LONG;
      next = f->next;

      /* Make certain it isn't visible from any root.  Notice that we
	 do this check before sweep_pages merges save_in_use_p.  */
      gcc_assert (!(pe->in_use_p[word] & (1UL << bit)));

      /* If the object comes from an outer context, then retain the
	 free_object entry, so that we can verify that the address
	 isn't live on the stack in some outer context.  */
      if (pe->context_depth != G.context_depth)
	{
	  f->next = still_free;
	  still_free = f;
	}
      else
	free (f);
    }

  G.free_object_list = still_free;
}
#else
#define validate_free_objects()
#endif

/* Top level mark-and-sweep routine.  */

void
ggc_collect (enum ggc_collect mode)
{
  /* Avoid frequent unnecessary work by skipping collection if the
     total allocations haven't expanded much since the last
     collection.  */
  float allocated_last_gc =
    MAX (G.allocated_last_gc, (size_t)param_ggc_min_heapsize * ONE_K);

  /* It is also good time to get memory block pool into limits.  */
  memory_block_pool::trim ();

  float min_expand = allocated_last_gc * param_ggc_min_expand / 100;
  if (mode == GGC_COLLECT_HEURISTIC
      && G.allocated < allocated_last_gc + min_expand)
    return;

  timevar_push (TV_GC);
  if (GGC_DEBUG_LEVEL >= 2)
    fprintf (G.debug_file, "BEGIN COLLECTING\n");

  /* Zero the total allocated bytes.  This will be recalculated in the
     sweep phase.  */
  size_t allocated = G.allocated;
  G.allocated = 0;

  /* Release the pages we freed the last time we collected, but didn't
     reuse in the interim.  */
  release_pages ();

  /* Output this later so we do not interfere with release_pages.  */
  if (!quiet_flag)
    fprintf (stderr, " {GC " PRsa (0) " -> ", SIZE_AMOUNT (allocated));

  /* Indicate that we've seen collections at this context depth.  */
  G.context_depth_collections = ((unsigned long)1 << (G.context_depth + 1)) - 1;

  invoke_plugin_callbacks (PLUGIN_GGC_START, NULL);

  in_gc = true;
  clear_marks ();
  ggc_mark_roots ();
  ggc_handle_finalizers ();

  if (GATHER_STATISTICS)
    ggc_prune_overhead_list ();

  poison_pages ();
  validate_free_objects ();
  sweep_pages ();

  in_gc = false;
  G.allocated_last_gc = G.allocated;

  invoke_plugin_callbacks (PLUGIN_GGC_END, NULL);

  timevar_pop (TV_GC);

  if (!quiet_flag)
    fprintf (stderr, PRsa (0) "}", SIZE_AMOUNT (G.allocated));
  if (GGC_DEBUG_LEVEL >= 2)
    fprintf (G.debug_file, "END COLLECTING\n");
}

/* Return free pages to the system.  */

void
ggc_trim ()
{
  timevar_push (TV_GC);
  G.allocated = 0;
  sweep_pages ();
  release_pages ();
  if (!quiet_flag)
    fprintf (stderr, " {GC trimmed to " PRsa (0) ", " PRsa (0) " mapped}",
	     SIZE_AMOUNT (G.allocated), SIZE_AMOUNT (G.bytes_mapped));
  timevar_pop (TV_GC);
}

/* Assume that all GGC memory is reachable and grow the limits for next
   collection.  With checking, trigger GGC so -Q compilation outputs how much
   of memory really is reachable.  */

void
ggc_grow (void)
{
  if (!flag_checking)
    G.allocated_last_gc = MAX (G.allocated_last_gc,
			       G.allocated);
  else
    ggc_collect ();
  if (!quiet_flag)
    fprintf (stderr, " {GC " PRsa (0) "} ", SIZE_AMOUNT (G.allocated));
}

void
ggc_print_statistics (void)
{
  struct ggc_statistics stats;
  unsigned int i;
  size_t total_overhead = 0;

  /* Clear the statistics.  */
  memset (&stats, 0, sizeof (stats));

  /* Make sure collection will really occur.  */
  G.allocated_last_gc = 0;

  /* Collect and print the statistics common across collectors.  */
  ggc_print_common_statistics (stderr, &stats);

  /* Release free pages so that we will not count the bytes allocated
     there as part of the total allocated memory.  */
  release_pages ();

  /* Collect some information about the various sizes of
     allocation.  */
  fprintf (stderr,
           "Memory still allocated at the end of the compilation process\n");
  fprintf (stderr, "%-8s %10s  %10s  %10s\n",
	   "Size", "Allocated", "Used", "Overhead");
  for (i = 0; i < NUM_ORDERS; ++i)
    {
      page_entry *p;
      size_t allocated;
      size_t in_use;
      size_t overhead;

      /* Skip empty entries.  */
      if (!G.pages[i])
	continue;

      overhead = allocated = in_use = 0;

      /* Figure out the total number of bytes allocated for objects of
	 this size, and how many of them are actually in use.  Also figure
	 out how much memory the page table is using.  */
      for (p = G.pages[i]; p; p = p->next)
	{
	  allocated += p->bytes;
	  in_use +=
	    (OBJECTS_IN_PAGE (p) - p->num_free_objects) * OBJECT_SIZE (i);

	  overhead += (sizeof (page_entry) - sizeof (long)
		       + BITMAP_SIZE (OBJECTS_IN_PAGE (p) + 1));
	}
      fprintf (stderr, "%-8" PRIu64 " " PRsa (10) " " PRsa (10) " "
	       PRsa (10) "\n",
	       (uint64_t)OBJECT_SIZE (i),
	       SIZE_AMOUNT (allocated),
	       SIZE_AMOUNT (in_use),
	       SIZE_AMOUNT (overhead));
      total_overhead += overhead;
    }
  fprintf (stderr, "%-8s " PRsa (10) " " PRsa (10) " " PRsa (10) "\n",
	   "Total",
	   SIZE_AMOUNT (G.bytes_mapped),
	   SIZE_AMOUNT (G.allocated),
	   SIZE_AMOUNT (total_overhead));

  if (GATHER_STATISTICS)
    {
      fprintf (stderr, "\nTotal allocations and overheads during "
	       "the compilation process\n");

      fprintf (stderr, "Total Overhead:                          "
	       PRsa (9) "\n",
	       SIZE_AMOUNT (G.stats.total_overhead));
      fprintf (stderr, "Total Allocated:                         "
	       PRsa (9) "\n",
	       SIZE_AMOUNT (G.stats.total_allocated));

      fprintf (stderr, "Total Overhead  under  32B:              "
	       PRsa (9) "\n",
	       SIZE_AMOUNT (G.stats.total_overhead_under32));
      fprintf (stderr, "Total Allocated under  32B:              "
	       PRsa (9) "\n",
	       SIZE_AMOUNT (G.stats.total_allocated_under32));
      fprintf (stderr, "Total Overhead  under  64B:              "
	       PRsa (9) "\n",
	       SIZE_AMOUNT (G.stats.total_overhead_under64));
      fprintf (stderr, "Total Allocated under  64B:              "
	       PRsa (9) "\n",
	       SIZE_AMOUNT (G.stats.total_allocated_under64));
      fprintf (stderr, "Total Overhead  under 128B:              "
	       PRsa (9) "\n",
	       SIZE_AMOUNT (G.stats.total_overhead_under128));
      fprintf (stderr, "Total Allocated under 128B:              "
	       PRsa (9) "\n",
	       SIZE_AMOUNT (G.stats.total_allocated_under128));

      for (i = 0; i < NUM_ORDERS; i++)
	if (G.stats.total_allocated_per_order[i])
	  {
	    fprintf (stderr, "Total Overhead  page size %9" PRIu64 ":     "
		     PRsa (9) "\n",
		     (uint64_t)OBJECT_SIZE (i),
		     SIZE_AMOUNT (G.stats.total_overhead_per_order[i]));
	    fprintf (stderr, "Total Allocated page size %9" PRIu64 ":     "
		     PRsa (9) "\n",
		     (uint64_t)OBJECT_SIZE (i),
		     SIZE_AMOUNT (G.stats.total_allocated_per_order[i]));
	  }
  }
}

struct ggc_pch_ondisk
{
  unsigned totals[NUM_ORDERS];
};

struct ggc_pch_data
{
  struct ggc_pch_ondisk d;
  uintptr_t base[NUM_ORDERS];
  size_t written[NUM_ORDERS];
};

struct ggc_pch_data *
init_ggc_pch (void)
{
  return XCNEW (struct ggc_pch_data);
}

void
ggc_pch_count_object (struct ggc_pch_data *d, void *x ATTRIBUTE_UNUSED,
		      size_t size)
{
  unsigned order;

  if (size < NUM_SIZE_LOOKUP)
    order = size_lookup[size];
  else
    {
      order = 10;
      while (size > OBJECT_SIZE (order))
	order++;
    }

  d->d.totals[order]++;
}

size_t
ggc_pch_total_size (struct ggc_pch_data *d)
{
  size_t a = 0;
  unsigned i;

  for (i = 0; i < NUM_ORDERS; i++)
    a += PAGE_ALIGN (d->d.totals[i] * OBJECT_SIZE (i));
  return a;
}

void
ggc_pch_this_base (struct ggc_pch_data *d, void *base)
{
  uintptr_t a = (uintptr_t) base;
  unsigned i;

  for (i = 0; i < NUM_ORDERS; i++)
    {
      d->base[i] = a;
      a += PAGE_ALIGN (d->d.totals[i] * OBJECT_SIZE (i));
    }
}


char *
ggc_pch_alloc_object (struct ggc_pch_data *d, void *x ATTRIBUTE_UNUSED,
		      size_t size)
{
  unsigned order;
  char *result;

  if (size < NUM_SIZE_LOOKUP)
    order = size_lookup[size];
  else
    {
      order = 10;
      while (size > OBJECT_SIZE (order))
	order++;
    }

  result = (char *) d->base[order];
  d->base[order] += OBJECT_SIZE (order);
  return result;
}

void
ggc_pch_prepare_write (struct ggc_pch_data *d ATTRIBUTE_UNUSED,
		       FILE *f ATTRIBUTE_UNUSED)
{
  /* Nothing to do.  */
}

void
ggc_pch_write_object (struct ggc_pch_data *d,
		      FILE *f, void *x, void *newx ATTRIBUTE_UNUSED,
		      size_t size)
{
  unsigned order;
  static const char emptyBytes[256] = { 0 };

  if (size < NUM_SIZE_LOOKUP)
    order = size_lookup[size];
  else
    {
      order = 10;
      while (size > OBJECT_SIZE (order))
	order++;
    }

  if (fwrite (x, size, 1, f) != 1)
    fatal_error (input_location, "cannot write PCH file: %m");

  /* If SIZE is not the same as OBJECT_SIZE(order), then we need to pad the
     object out to OBJECT_SIZE(order).  This happens for strings.  */

  if (size != OBJECT_SIZE (order))
    {
      unsigned padding = OBJECT_SIZE (order) - size;

      /* To speed small writes, we use a nulled-out array that's larger
         than most padding requests as the source for our null bytes.  This
         permits us to do the padding with fwrite() rather than fseek(), and
         limits the chance the OS may try to flush any outstanding writes.  */
      if (padding <= sizeof (emptyBytes))
        {
          if (fwrite (emptyBytes, 1, padding, f) != padding)
	    fatal_error (input_location, "cannot write PCH file");
        }
      else
        {
          /* Larger than our buffer?  Just default to fseek.  */
          if (fseek (f, padding, SEEK_CUR) != 0)
	    fatal_error (input_location, "cannot write PCH file");
        }
    }

  d->written[order]++;
  if (d->written[order] == d->d.totals[order]
      && fseek (f, ROUND_UP_VALUE (d->d.totals[order] * OBJECT_SIZE (order),
				   G.pagesize),
		SEEK_CUR) != 0)
    fatal_error (input_location, "cannot write PCH file: %m");
}

void
ggc_pch_finish (struct ggc_pch_data *d, FILE *f)
{
  if (fwrite (&d->d, sizeof (d->d), 1, f) != 1)
    fatal_error (input_location, "cannot write PCH file: %m");
  free (d);
}

/* Move the PCH PTE entries just added to the end of by_depth, to the
   front.  */

static void
move_ptes_to_front (int count_old_page_tables, int count_new_page_tables)
{
  /* First, we swap the new entries to the front of the varrays.  */
  page_entry **new_by_depth;
  unsigned long **new_save_in_use;

  new_by_depth = XNEWVEC (page_entry *, G.by_depth_max);
  new_save_in_use = XNEWVEC (unsigned long *, G.by_depth_max);

  memcpy (&new_by_depth[0],
	  &G.by_depth[count_old_page_tables],
	  count_new_page_tables * sizeof (void *));
  memcpy (&new_by_depth[count_new_page_tables],
	  &G.by_depth[0],
	  count_old_page_tables * sizeof (void *));
  memcpy (&new_save_in_use[0],
	  &G.save_in_use[count_old_page_tables],
	  count_new_page_tables * sizeof (void *));
  memcpy (&new_save_in_use[count_new_page_tables],
	  &G.save_in_use[0],
	  count_old_page_tables * sizeof (void *));

  free (G.by_depth);
  free (G.save_in_use);

  G.by_depth = new_by_depth;
  G.save_in_use = new_save_in_use;

  /* Now update all the index_by_depth fields.  */
  for (unsigned i = G.by_depth_in_use; i--;)
    {
      page_entry *p = G.by_depth[i];
      p->index_by_depth = i;
    }

  /* And last, we update the depth pointers in G.depth.  The first
     entry is already 0, and context 0 entries always start at index
     0, so there is nothing to update in the first slot.  We need a
     second slot, only if we have old ptes, and if we do, they start
     at index count_new_page_tables.  */
  if (count_old_page_tables)
    push_depth (count_new_page_tables);
}

void
ggc_pch_read (FILE *f, void *addr)
{
  struct ggc_pch_ondisk d;
  unsigned i;
  char *offs = (char *) addr;
  unsigned long count_old_page_tables;
  unsigned long count_new_page_tables;

  count_old_page_tables = G.by_depth_in_use;

  if (fread (&d, sizeof (d), 1, f) != 1)
    fatal_error (input_location, "cannot read PCH file: %m");

  /* We've just read in a PCH file.  So, every object that used to be
     allocated is now free.  */
  clear_marks ();
#ifdef ENABLE_GC_CHECKING
  poison_pages ();
#endif
  /* Since we free all the allocated objects, the free list becomes
     useless.  Validate it now, which will also clear it.  */
  validate_free_objects ();

  /* No object read from a PCH file should ever be freed.  So, set the
     context depth to 1, and set the depth of all the currently-allocated
     pages to be 1 too.  PCH pages will have depth 0.  */
  gcc_assert (!G.context_depth);
  G.context_depth = 1;
  /* Allocate space for the depth 1 finalizers.  */
  G.finalizers.safe_push (vNULL);
  G.vec_finalizers.safe_push (vNULL);
  gcc_assert (G.finalizers.length() == 2);
  for (i = 0; i < NUM_ORDERS; i++)
    {
      page_entry *p;
      for (p = G.pages[i]; p != NULL; p = p->next)
	p->context_depth = G.context_depth;
    }

  /* Allocate the appropriate page-table entries for the pages read from
     the PCH file.  */

  for (i = 0; i < NUM_ORDERS; i++)
    {
      struct page_entry *entry;
      char *pte;
      size_t bytes;
      size_t num_objs;
      size_t j;

      if (d.totals[i] == 0)
	continue;

      bytes = PAGE_ALIGN (d.totals[i] * OBJECT_SIZE (i));
      num_objs = bytes / OBJECT_SIZE (i);
      entry = XCNEWVAR (struct page_entry, (sizeof (struct page_entry)
					    - sizeof (long)
					    + BITMAP_SIZE (num_objs + 1)));
      entry->bytes = bytes;
      entry->page = offs;
      entry->context_depth = 0;
      offs += bytes;
      entry->num_free_objects = 0;
      entry->order = i;

      for (j = 0;
	   j + HOST_BITS_PER_LONG <= num_objs + 1;
	   j += HOST_BITS_PER_LONG)
	entry->in_use_p[j / HOST_BITS_PER_LONG] = -1;
      for (; j < num_objs + 1; j++)
	entry->in_use_p[j / HOST_BITS_PER_LONG]
	  |= 1L << (j % HOST_BITS_PER_LONG);

      for (pte = entry->page;
	   pte < entry->page + entry->bytes;
	   pte += G.pagesize)
	set_page_table_entry (pte, entry);

      if (G.page_tails[i] != NULL)
	G.page_tails[i]->next = entry;
      else
	G.pages[i] = entry;
      G.page_tails[i] = entry;

      /* We start off by just adding all the new information to the
	 end of the varrays, later, we will move the new information
	 to the front of the varrays, as the PCH page tables are at
	 context 0.  */
      push_by_depth (entry, 0);
    }

  /* Now, we update the various data structures that speed page table
     handling.  */
  count_new_page_tables = G.by_depth_in_use - count_old_page_tables;

  move_ptes_to_front (count_old_page_tables, count_new_page_tables);

  /* Update the statistics.  */
  G.allocated = G.allocated_last_gc = offs - (char *)addr;
}
