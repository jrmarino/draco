/* Simple garbage collection for the GNU compiler.
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

/* Generic garbage collection (GC) functions and data, not specific to
   any particular GC implementation.  */

#include "config.h"
#define INCLUDE_MALLOC_H
#include "system.h"
#include "coretypes.h"
#include "timevar.h"
#include "diagnostic-core.h"
#include "ggc-internal.h"
#include "hosthooks.h"
#include "plugin.h"
#include "options.h"

/* When true, protect the contents of the identifier hash table.  */
bool ggc_protect_identifiers = true;

/* Statistics about the allocation.  */
static ggc_statistics *ggc_stats;

struct traversal_state;

static int compare_ptr_data (const void *, const void *);
static void relocate_ptrs (void *, void *, void *);
static void write_pch_globals (const struct ggc_root_tab * const *tab,
			       struct traversal_state *state);

/* Maintain global roots that are preserved during GC.  */

/* This extra vector of dynamically registered root_tab-s is used by
   ggc_mark_roots and gives the ability to dynamically add new GGC root
   tables, for instance from some plugins; this vector is on the heap
   since it is used by GGC internally.  */
typedef const struct ggc_root_tab *const_ggc_root_tab_t;
static vec<const_ggc_root_tab_t> extra_root_vec;

/* Dynamically register a new GGC root table RT. This is useful for
   plugins. */

void
ggc_register_root_tab (const struct ggc_root_tab* rt)
{
  if (rt)
    extra_root_vec.safe_push (rt);
}

/* Mark all the roots in the table RT.  */

static void
ggc_mark_root_tab (const_ggc_root_tab_t rt)
{
  size_t i;

  for ( ; rt->base != NULL; rt++)
    for (i = 0; i < rt->nelt; i++)
      (*rt->cb) (*(void **) ((char *)rt->base + rt->stride * i));
}

/* Zero out all the roots in the table RT.  */

static void
ggc_zero_rtab_roots (const_ggc_root_tab_t rt)
{
  size_t i;

  for ( ; rt->base != NULL; rt++)
    for (i = 0; i < rt->nelt; i++)
      (*(void **) ((char *)rt->base + rt->stride * i)) = (void*)0;
}

/* Iterate through all registered roots and mark each element.  */

void
ggc_mark_roots (void)
{
  const struct ggc_root_tab *const *rt;
  const_ggc_root_tab_t rtp, rti;
  size_t i;

  for (rt = gt_ggc_deletable_rtab; *rt; rt++)
    for (rti = *rt; rti->base != NULL; rti++)
      memset (rti->base, 0, rti->stride * rti->nelt);

  for (rt = gt_ggc_rtab; *rt; rt++)
    ggc_mark_root_tab (*rt);

  FOR_EACH_VEC_ELT (extra_root_vec, i, rtp)
    ggc_mark_root_tab (rtp);

  if (ggc_protect_identifiers)
    ggc_mark_stringpool ();

  gt_clear_caches ();

  if (! ggc_protect_identifiers)
    ggc_purge_stringpool ();

  /* Some plugins may call ggc_set_mark from here.  */
  invoke_plugin_callbacks (PLUGIN_GGC_MARKING, NULL);
}

/* Allocate a block of memory, then clear it.  */
extern "C" void *
ggc_internal_cleared_alloc_ (size_t size, void (*f)(void *), size_t s, size_t n
			     MEM_STAT_DECL)
{
  void *buf = ggc_internal_alloc (size, f, s, n PASS_MEM_STAT);
  memset (buf, 0, size);
  return buf;
}

extern void *
ggc_internal_cleared_alloc (size_t size, void (*f)(void *), size_t s,
			    size_t n MEM_STAT_DECL)
     __attribute__((__alias__ ("ggc_internal_cleared_alloc_")));

extern void *
ggc_internal_cleared_alloc_no_dtor (size_t size, void (*f)(void *),
				    size_t s, size_t n MEM_STAT_DECL)
     __attribute__((__alias__ ("ggc_internal_cleared_alloc_")));

/* Resize a block of memory, possibly re-allocating it.  */
void *
ggc_realloc (void *x, size_t size MEM_STAT_DECL)
{
  void *r;
  size_t old_size;

  if (x == NULL)
    return ggc_internal_alloc (size PASS_MEM_STAT);

  old_size = ggc_get_size (x);

  if (size <= old_size)
    {
      /* Mark the unwanted memory as unaccessible.  We also need to make
	 the "new" size accessible, since ggc_get_size returns the size of
	 the pool, not the size of the individually allocated object, the
	 size which was previously made accessible.  Unfortunately, we
	 don't know that previously allocated size.  Without that
	 knowledge we have to lose some initialization-tracking for the
	 old parts of the object.  An alternative is to mark the whole
	 old_size as reachable, but that would lose tracking of writes
	 after the end of the object (by small offsets).  Discard the
	 handle to avoid handle leak.  */
      VALGRIND_DISCARD (VALGRIND_MAKE_MEM_NOACCESS ((char *) x + size,
						    old_size - size));
      VALGRIND_DISCARD (VALGRIND_MAKE_MEM_DEFINED (x, size));
      return x;
    }

  r = ggc_internal_alloc (size PASS_MEM_STAT);

  /* Since ggc_get_size returns the size of the pool, not the size of the
     individually allocated object, we'd access parts of the old object
     that were marked invalid with the memcpy below.  We lose a bit of the
     initialization-tracking since some of it may be uninitialized.  */
  VALGRIND_DISCARD (VALGRIND_MAKE_MEM_DEFINED (x, old_size));

  memcpy (r, x, old_size);

  /* The old object is not supposed to be used anymore.  */
  ggc_free (x);

  return r;
}

void *
ggc_cleared_alloc_htab_ignore_args (size_t c ATTRIBUTE_UNUSED,
				    size_t n ATTRIBUTE_UNUSED)
{
  gcc_assert (c * n == sizeof (struct htab));
  return ggc_cleared_alloc<htab> ();
}

/* TODO: once we actually use type information in GGC, create a new tag
   gt_gcc_ptr_array and use it for pointer arrays.  */
void *
ggc_cleared_alloc_ptr_array_two_args (size_t c, size_t n)
{
  gcc_assert (sizeof (void **) == n);
  return ggc_cleared_vec_alloc<void **> (c);
}

/* These are for splay_tree_new_ggc.  */
void *
ggc_splay_alloc (int sz, void *nl)
{
  gcc_assert (!nl);
  return ggc_internal_alloc (sz);
}

void
ggc_splay_dont_free (void * x ATTRIBUTE_UNUSED, void *nl)
{
  gcc_assert (!nl);
}

void
ggc_print_common_statistics (FILE *stream ATTRIBUTE_UNUSED,
			     ggc_statistics *stats)
{
  /* Set the pointer so that during collection we will actually gather
     the statistics.  */
  ggc_stats = stats;

  /* Then do one collection to fill in the statistics.  */
  ggc_collect ();

  /* At present, we don't really gather any interesting statistics.  */

  /* Don't gather statistics any more.  */
  ggc_stats = NULL;
}

/* Functions for saving and restoring GCable memory to disk.  */

struct ptr_data
{
  void *obj;
  void *note_ptr_cookie;
  gt_note_pointers note_ptr_fn;
  gt_handle_reorder reorder_fn;
  size_t size;
  void *new_addr;
};

#define POINTER_HASH(x) (hashval_t)((intptr_t)x >> 3)

/* Helper for hashing saving_htab.  */

struct saving_hasher : free_ptr_hash <ptr_data>
{
  typedef void *compare_type;
  static inline hashval_t hash (const ptr_data *);
  static inline bool equal (const ptr_data *, const void *);
};

inline hashval_t
saving_hasher::hash (const ptr_data *p)
{
  return POINTER_HASH (p->obj);
}

inline bool
saving_hasher::equal (const ptr_data *p1, const void *p2)
{
  return p1->obj == p2;
}

static hash_table<saving_hasher> *saving_htab;
static vec<void *> callback_vec;
static vec<void *> reloc_addrs_vec;

/* Register an object in the hash table.  */

int
gt_pch_note_object (void *obj, void *note_ptr_cookie,
		    gt_note_pointers note_ptr_fn,
		    size_t length_override)
{
  struct ptr_data **slot;

  if (obj == NULL || obj == (void *) 1)
    return 0;

  slot = (struct ptr_data **)
    saving_htab->find_slot_with_hash (obj, POINTER_HASH (obj), INSERT);
  if (*slot != NULL)
    {
      gcc_assert ((*slot)->note_ptr_fn == note_ptr_fn
		  && (*slot)->note_ptr_cookie == note_ptr_cookie);
      return 0;
    }

  *slot = XCNEW (struct ptr_data);
  (*slot)->obj = obj;
  (*slot)->note_ptr_fn = note_ptr_fn;
  (*slot)->note_ptr_cookie = note_ptr_cookie;
  if (length_override != (size_t)-1)
    (*slot)->size = length_override;
  else if (note_ptr_fn == gt_pch_p_S)
    (*slot)->size = strlen ((const char *)obj) + 1;
  else
    (*slot)->size = ggc_get_size (obj);
  return 1;
}

/* Register address of a callback pointer.  */
void
gt_pch_note_callback (void *obj, void *base)
{
  void *ptr;
  memcpy (&ptr, obj, sizeof (void *));
  if (ptr != NULL)
    {
      struct ptr_data *data
	= (struct ptr_data *)
	  saving_htab->find_with_hash (base, POINTER_HASH (base));
      gcc_assert (data);
      callback_vec.safe_push ((char *) data->new_addr
			      + ((char *) obj - (char *) base));
    }
}

/* Register an object in the hash table.  */

void
gt_pch_note_reorder (void *obj, void *note_ptr_cookie,
		     gt_handle_reorder reorder_fn)
{
  struct ptr_data *data;

  if (obj == NULL || obj == (void *) 1)
    return;

  data = (struct ptr_data *)
    saving_htab->find_with_hash (obj, POINTER_HASH (obj));
  gcc_assert (data && data->note_ptr_cookie == note_ptr_cookie);
  /* The GTY 'reorder' option doesn't make sense if we don't walk pointers,
     such as for strings.  */
  gcc_checking_assert (data->note_ptr_fn != gt_pch_p_S);

  data->reorder_fn = reorder_fn;
}

/* Handy state for the traversal functions.  */

struct traversal_state
{
  FILE *f;
  struct ggc_pch_data *d;
  size_t count;
  struct ptr_data **ptrs;
  size_t ptrs_i;
};

/* Callbacks for htab_traverse.  */

int
ggc_call_count (ptr_data **slot, traversal_state *state)
{
  struct ptr_data *d = *slot;

  ggc_pch_count_object (state->d, d->obj, d->size);
  state->count++;
  return 1;
}

int
ggc_call_alloc (ptr_data **slot, traversal_state *state)
{
  struct ptr_data *d = *slot;

  d->new_addr = ggc_pch_alloc_object (state->d, d->obj, d->size);
  state->ptrs[state->ptrs_i++] = d;
  return 1;
}

/* Callback for qsort.  */

static int
compare_ptr_data (const void *p1_p, const void *p2_p)
{
  const struct ptr_data *const p1 = *(const struct ptr_data *const *)p1_p;
  const struct ptr_data *const p2 = *(const struct ptr_data *const *)p2_p;
  return (((size_t)p1->new_addr > (size_t)p2->new_addr)
	  - ((size_t)p1->new_addr < (size_t)p2->new_addr));
}

/* Callbacks for note_ptr_fn.  */

static void
relocate_ptrs (void *ptr_p, void *real_ptr_p, void *state_p)
{
  void **ptr = (void **)ptr_p;
  struct traversal_state *state
    = (struct traversal_state *)state_p;
  struct ptr_data *result;

  if (*ptr == NULL || *ptr == (void *)1)
    return;

  result = (struct ptr_data *)
    saving_htab->find_with_hash (*ptr, POINTER_HASH (*ptr));
  gcc_assert (result);
  *ptr = result->new_addr;
  if (ptr_p == real_ptr_p)
    return;
  if (real_ptr_p == NULL)
    real_ptr_p = ptr_p;
  gcc_assert (real_ptr_p >= state->ptrs[state->ptrs_i]->obj
	      && ((char *) real_ptr_p + sizeof (void *)
		  <= ((char *) state->ptrs[state->ptrs_i]->obj
		      + state->ptrs[state->ptrs_i]->size)));
  void *addr
    = (void *) ((char *) state->ptrs[state->ptrs_i]->new_addr
		+ ((char *) real_ptr_p
		   - (char *) state->ptrs[state->ptrs_i]->obj));
  reloc_addrs_vec.safe_push (addr);
}

/* Write out, after relocation, the pointers in TAB.  */
static void
write_pch_globals (const struct ggc_root_tab * const *tab,
		   struct traversal_state *state)
{
  const struct ggc_root_tab *const *rt;
  const struct ggc_root_tab *rti;
  size_t i;

  for (rt = tab; *rt; rt++)
    for (rti = *rt; rti->base != NULL; rti++)
      for (i = 0; i < rti->nelt; i++)
	{
	  void *ptr = *(void **)((char *)rti->base + rti->stride * i);
	  struct ptr_data *new_ptr;
	  if (ptr == NULL || ptr == (void *)1)
	    {
	      if (fwrite (&ptr, sizeof (void *), 1, state->f)
		  != 1)
		fatal_error (input_location, "cannot write PCH file: %m");
	    }
	  else
	    {
	      new_ptr = (struct ptr_data *)
		saving_htab->find_with_hash (ptr, POINTER_HASH (ptr));
	      if (fwrite (&new_ptr->new_addr, sizeof (void *), 1, state->f)
		  != 1)
		fatal_error (input_location, "cannot write PCH file: %m");
	    }
	}
}

/* Callback for qsort.  */

static int
compare_ptr (const void *p1_p, const void *p2_p)
{
  void *p1 = *(void *const *)p1_p;
  void *p2 = *(void *const *)p2_p;
  return (((uintptr_t)p1 > (uintptr_t)p2)
	  - ((uintptr_t)p1 < (uintptr_t)p2));
}

/* Decode one uleb128 from P, return first byte after it, store
   decoded value into *VAL.  */

static unsigned char *
read_uleb128 (unsigned char *p, size_t *val)
{
  unsigned int shift = 0;
  unsigned char byte;
  size_t result;

  result = 0;
  do
    {
      byte = *p++;
      result |= ((size_t) byte & 0x7f) << shift;
      shift += 7;
    }
  while (byte & 0x80);

  *val = result;
  return p;
}

/* Store VAL as uleb128 at P, return length in bytes.  */

static size_t
write_uleb128 (unsigned char *p, size_t val)
{
  size_t len = 0;
  do
    {
      unsigned char byte = (val & 0x7f);
      val >>= 7;
      if (val != 0)
	/* More bytes to follow.  */
	byte |= 0x80;

      *p++ = byte;
      ++len;
    }
  while (val != 0);
  return len;
}

/* Hold the information we need to mmap the file back in.  */

struct mmap_info
{
  size_t offset;
  size_t size;
  void *preferred_base;
};

/* Write out the state of the compiler to F.  */

void
gt_pch_save (FILE *f)
{
  const struct ggc_root_tab *const *rt;
  const struct ggc_root_tab *rti;
  size_t i;
  struct traversal_state state;
  char *this_object = NULL;
  size_t this_object_size = 0;
  struct mmap_info mmi;
  const size_t mmap_offset_alignment = host_hooks.gt_pch_alloc_granularity ();

  gt_pch_save_stringpool ();

  timevar_push (TV_PCH_PTR_REALLOC);
  saving_htab = new hash_table<saving_hasher> (50000);

  for (rt = gt_ggc_rtab; *rt; rt++)
    for (rti = *rt; rti->base != NULL; rti++)
      for (i = 0; i < rti->nelt; i++)
	(*rti->pchw)(*(void **)((char *)rti->base + rti->stride * i));

  /* Prepare the objects for writing, determine addresses and such.  */
  state.f = f;
  state.d = init_ggc_pch ();
  state.count = 0;
  saving_htab->traverse <traversal_state *, ggc_call_count> (&state);

  mmi.size = ggc_pch_total_size (state.d);

  /* Try to arrange things so that no relocation is necessary, but
     don't try very hard.  On most platforms, this will always work,
     and on the rest it's a lot of work to do better.
     (The extra work goes in HOST_HOOKS_GT_PCH_GET_ADDRESS and
     HOST_HOOKS_GT_PCH_USE_ADDRESS.)  */
  mmi.preferred_base = host_hooks.gt_pch_get_address (mmi.size, fileno (f));
  /* If the host cannot supply any suitable address for this, we are stuck.  */
  if (mmi.preferred_base == NULL)
    fatal_error (input_location,
		 "cannot write PCH file: required memory segment unavailable");

  ggc_pch_this_base (state.d, mmi.preferred_base);

  state.ptrs = XNEWVEC (struct ptr_data *, state.count);
  state.ptrs_i = 0;

  saving_htab->traverse <traversal_state *, ggc_call_alloc> (&state);
  timevar_pop (TV_PCH_PTR_REALLOC);

  timevar_push (TV_PCH_PTR_SORT);
  qsort (state.ptrs, state.count, sizeof (*state.ptrs), compare_ptr_data);
  timevar_pop (TV_PCH_PTR_SORT);

  /* Write out all the scalar variables.  */
  for (rt = gt_pch_scalar_rtab; *rt; rt++)
    for (rti = *rt; rti->base != NULL; rti++)
      if (fwrite (rti->base, rti->stride, 1, f) != 1)
	fatal_error (input_location, "cannot write PCH file: %m");

  /* Write out all the global pointers, after translation.  */
  write_pch_globals (gt_ggc_rtab, &state);

  /* Pad the PCH file so that the mmapped area starts on an allocation
     granularity (usually page) boundary.  */
  {
    long o;
    o = ftell (state.f) + sizeof (mmi);
    if (o == -1)
      fatal_error (input_location, "cannot get position in PCH file: %m");
    mmi.offset = mmap_offset_alignment - o % mmap_offset_alignment;
    if (mmi.offset == mmap_offset_alignment)
      mmi.offset = 0;
    mmi.offset += o;
  }
  if (fwrite (&mmi, sizeof (mmi), 1, state.f) != 1)
    fatal_error (input_location, "cannot write PCH file: %m");
  if (mmi.offset != 0
      && fseek (state.f, mmi.offset, SEEK_SET) != 0)
    fatal_error (input_location, "cannot write padding to PCH file: %m");

  ggc_pch_prepare_write (state.d, state.f);

#if defined ENABLE_VALGRIND_ANNOTATIONS && defined VALGRIND_GET_VBITS
  vec<char> vbits = vNULL;
#endif

  /* Actually write out the objects.  */
  for (i = 0; i < state.count; i++)
    {
      state.ptrs_i = i;
      if (this_object_size < state.ptrs[i]->size)
	{
	  this_object_size = state.ptrs[i]->size;
	  this_object = XRESIZEVAR (char, this_object, this_object_size);
	}
#if defined ENABLE_VALGRIND_ANNOTATIONS && defined VALGRIND_GET_VBITS
      /* obj might contain uninitialized bytes, e.g. in the trailing
	 padding of the object.  Avoid warnings by making the memory
	 temporarily defined and then restoring previous state.  */
      int get_vbits = 0;
      size_t valid_size = state.ptrs[i]->size;
      if (UNLIKELY (RUNNING_ON_VALGRIND))
	{
	  if (vbits.length () < valid_size)
	    vbits.safe_grow (valid_size, true);
	  get_vbits = VALGRIND_GET_VBITS (state.ptrs[i]->obj,
					  vbits.address (), valid_size);
	  if (get_vbits == 3)
	    {
	      /* We assume that first part of obj is addressable, and
		 the rest is unaddressable.  Find out where the boundary is
		 using binary search.  */
	      size_t lo = 0, hi = valid_size;
	      while (hi > lo)
		{
		  size_t mid = (lo + hi) / 2;
		  get_vbits = VALGRIND_GET_VBITS ((char *) state.ptrs[i]->obj
						  + mid, vbits.address (),
						  1);
		  if (get_vbits == 3)
		    hi = mid;
		  else if (get_vbits == 1)
		    lo = mid + 1;
		  else
		    break;
		}
	      if (get_vbits == 1 || get_vbits == 3)
		{
		  valid_size = lo;
		  get_vbits = VALGRIND_GET_VBITS (state.ptrs[i]->obj,
						  vbits.address (),
						  valid_size);
		}
	    }
	  if (get_vbits == 1)
	    VALGRIND_DISCARD (VALGRIND_MAKE_MEM_DEFINED (state.ptrs[i]->obj,
							 state.ptrs[i]->size));
	}
#endif
      memcpy (this_object, state.ptrs[i]->obj, state.ptrs[i]->size);
      if (state.ptrs[i]->reorder_fn != NULL)
	state.ptrs[i]->reorder_fn (state.ptrs[i]->obj,
				   state.ptrs[i]->note_ptr_cookie,
				   relocate_ptrs, &state);
      gt_note_pointers note_ptr_fn = state.ptrs[i]->note_ptr_fn;
      gcc_checking_assert (note_ptr_fn != NULL);
      /* 'gt_pch_p_S' enables certain special handling, but otherwise
     corresponds to no 'note_ptr_fn'.  */
      if (note_ptr_fn == gt_pch_p_S)
	note_ptr_fn = NULL;
      if (note_ptr_fn != NULL)
	note_ptr_fn (state.ptrs[i]->obj, state.ptrs[i]->note_ptr_cookie,
		     relocate_ptrs, &state);
      ggc_pch_write_object (state.d, state.f, state.ptrs[i]->obj,
			    state.ptrs[i]->new_addr, state.ptrs[i]->size);
      if (state.ptrs[i]->reorder_fn != NULL
	  || note_ptr_fn != NULL)
	memcpy (state.ptrs[i]->obj, this_object, state.ptrs[i]->size);
#if defined ENABLE_VALGRIND_ANNOTATIONS && defined VALGRIND_GET_VBITS
      if (UNLIKELY (get_vbits == 1))
	{
	  (void) VALGRIND_SET_VBITS (state.ptrs[i]->obj, vbits.address (),
				     valid_size);
	  if (valid_size != state.ptrs[i]->size)
	    VALGRIND_DISCARD (VALGRIND_MAKE_MEM_NOACCESS ((char *)
							  state.ptrs[i]->obj
							  + valid_size,
							  state.ptrs[i]->size
							  - valid_size));
	}
#endif
    }
#if defined ENABLE_VALGRIND_ANNOTATIONS && defined VALGRIND_GET_VBITS
  vbits.release ();
#endif

  reloc_addrs_vec.qsort (compare_ptr);

  size_t reloc_addrs_size = 0;
  void *last_addr = NULL;
  unsigned char uleb128_buf[sizeof (size_t) * 2];
  for (void *addr : reloc_addrs_vec)
    {
      gcc_assert ((uintptr_t) addr >= (uintptr_t) mmi.preferred_base
		  && ((uintptr_t) addr + sizeof (void *)
		      <= (uintptr_t) mmi.preferred_base + mmi.size));
      if (addr == last_addr)
	continue;
      if (last_addr == NULL)
	last_addr = mmi.preferred_base;
      size_t diff = (uintptr_t) addr - (uintptr_t) last_addr;
      reloc_addrs_size += write_uleb128 (uleb128_buf, diff);
      last_addr = addr;
    }
  if (fwrite (&reloc_addrs_size, sizeof (reloc_addrs_size), 1, f) != 1)
    fatal_error (input_location, "cannot write PCH file: %m");
  last_addr = NULL;
  for (void *addr : reloc_addrs_vec)
    {
      if (addr == last_addr)
	continue;
      if (last_addr == NULL)
	last_addr = mmi.preferred_base;
      size_t diff = (uintptr_t) addr - (uintptr_t) last_addr;
      reloc_addrs_size = write_uleb128 (uleb128_buf, diff);
      if (fwrite (uleb128_buf, 1, reloc_addrs_size, f) != reloc_addrs_size)
	fatal_error (input_location, "cannot write PCH file: %m");
      last_addr = addr;
    }

  ggc_pch_finish (state.d, state.f);

  gt_pch_fixup_stringpool ();

  unsigned num_callbacks = callback_vec.length ();
  void (*pch_save) (FILE *) = &gt_pch_save;
  if (fwrite (&pch_save, sizeof (pch_save), 1, f) != 1
      || fwrite (&num_callbacks, sizeof (num_callbacks), 1, f) != 1
      || (num_callbacks
	  && fwrite (callback_vec.address (), sizeof (void *), num_callbacks,
		     f) != num_callbacks))
    fatal_error (input_location, "cannot write PCH file: %m");

  XDELETE (state.ptrs);
  XDELETE (this_object);
  delete saving_htab;
  saving_htab = NULL;
  callback_vec.release ();
  reloc_addrs_vec.release ();
}

/* Read the state of the compiler back in from F.  */

void
gt_pch_restore (FILE *f)
{
  const struct ggc_root_tab *const *rt;
  const struct ggc_root_tab *rti;
  size_t i;
  struct mmap_info mmi;
  int result;

  /* We are about to reload the line maps along with the rest of the PCH
     data, which means that the (loaded) ones cannot be guaranteed to be
     in any valid state for reporting diagnostics that happen during the
     load.  Save the current table (and use it during the loading process
     below).  */
  class line_maps *save_line_table = line_table;

  /* Delete any deletable objects.  This makes ggc_pch_read much
     faster, as it can be sure that no GCable objects remain other
     than the ones just read in.  */
  for (rt = gt_ggc_deletable_rtab; *rt; rt++)
    for (rti = *rt; rti->base != NULL; rti++)
      memset (rti->base, 0, rti->stride);

  /* Read in all the scalar variables.  */
  for (rt = gt_pch_scalar_rtab; *rt; rt++)
    for (rti = *rt; rti->base != NULL; rti++)
      if (fread (rti->base, rti->stride, 1, f) != 1)
	fatal_error (input_location, "cannot read PCH file: %m");

  /* Read in all the global pointers, in 6 easy loops.  */
  bool error_reading_pointers = false;
  for (rt = gt_ggc_rtab; *rt; rt++)
    for (rti = *rt; rti->base != NULL; rti++)
      for (i = 0; i < rti->nelt; i++)
	if (fread ((char *)rti->base + rti->stride * i,
		   sizeof (void *), 1, f) != 1)
	  error_reading_pointers = true;

  /* Stash the newly read-in line table pointer - it does not point to
     anything meaningful yet, so swap the old one back in.  */
  class line_maps *new_line_table = line_table;
  line_table = save_line_table;
  if (error_reading_pointers)
    fatal_error (input_location, "cannot read PCH file: %m");

  if (fread (&mmi, sizeof (mmi), 1, f) != 1)
    fatal_error (input_location, "cannot read PCH file: %m");

  void *orig_preferred_base = mmi.preferred_base;
  result = host_hooks.gt_pch_use_address (mmi.preferred_base, mmi.size,
					  fileno (f), mmi.offset);

  /* We could not mmap or otherwise allocate the required memory at the
     address needed.  */
  if (result < 0)
    {
      sorry_at (input_location, "PCH allocation failure");
      /* There is no point in continuing from here, we will only end up
	 with a crashed (most likely hanging) compiler.  */
      exit (-1);
    }

  /* (0) We allocated memory, but did not mmap the file, so we need to read
     the data in manually.  (>0) Otherwise the mmap succeed for the address
     we wanted.  */
  if (result == 0)
    {
      if (fseek (f, mmi.offset, SEEK_SET) != 0
	  || fread (mmi.preferred_base, mmi.size, 1, f) != 1)
	fatal_error (input_location, "cannot read PCH file: %m");
    }
  else if (fseek (f, mmi.offset + mmi.size, SEEK_SET) != 0)
    fatal_error (input_location, "cannot read PCH file: %m");

  size_t reloc_addrs_size;
  if (fread (&reloc_addrs_size, sizeof (reloc_addrs_size), 1, f) != 1)
    fatal_error (input_location, "cannot read PCH file: %m");

  if (orig_preferred_base != mmi.preferred_base)
    {
      uintptr_t bias
	= (uintptr_t) mmi.preferred_base - (uintptr_t) orig_preferred_base;

      /* Adjust all the global pointers by bias.  */
      line_table = new_line_table;
      for (rt = gt_ggc_rtab; *rt; rt++)
	for (rti = *rt; rti->base != NULL; rti++)
      for (i = 0; i < rti->nelt; i++)
	{
	  char *addr = (char *)rti->base + rti->stride * i;
	  char *p;
	  memcpy (&p, addr, sizeof (void *));
	  if ((uintptr_t) p >= (uintptr_t) orig_preferred_base
	      && (uintptr_t) p < (uintptr_t) orig_preferred_base + mmi.size)
	    {
	      p = (char *) ((uintptr_t) p + bias);
	      memcpy (addr, &p, sizeof (void *));
	    }
	}
      new_line_table = line_table;
      line_table = save_line_table;

      /* And adjust all the pointers in the image by bias too.  */
      char *addr = (char *) mmi.preferred_base;
      unsigned char uleb128_buf[4096], *uleb128_ptr = uleb128_buf;
      while (reloc_addrs_size != 0)
	{
	  size_t this_size
	    = MIN (reloc_addrs_size,
		   (size_t) (4096 - (uleb128_ptr - uleb128_buf)));
	  if (fread (uleb128_ptr, 1, this_size, f) != this_size)
	    fatal_error (input_location, "cannot read PCH file: %m");
	  unsigned char *uleb128_end = uleb128_ptr + this_size;
	  if (this_size != reloc_addrs_size)
	    uleb128_end -= 2 * sizeof (size_t);
	  uleb128_ptr = uleb128_buf;
	  while (uleb128_ptr < uleb128_end)
	    {
	      size_t diff;
	      uleb128_ptr = read_uleb128 (uleb128_ptr, &diff);
	      addr = (char *) ((uintptr_t) addr + diff);

	      char *p;
	      memcpy (&p, addr, sizeof (void *));
	      gcc_assert ((uintptr_t) p >= (uintptr_t) orig_preferred_base
			  && ((uintptr_t) p
			      < (uintptr_t) orig_preferred_base + mmi.size));
	      p = (char *) ((uintptr_t) p + bias);
	      memcpy (addr, &p, sizeof (void *));
	    }
	  reloc_addrs_size -= this_size;
	  if (reloc_addrs_size == 0)
	    break;
	  this_size = uleb128_end + 2 * sizeof (size_t) - uleb128_ptr;
	  memcpy (uleb128_buf, uleb128_ptr, this_size);
	  uleb128_ptr = uleb128_buf + this_size;
	}
    }
  else if (fseek (f, (mmi.offset + mmi.size + sizeof (reloc_addrs_size)
		      + reloc_addrs_size), SEEK_SET) != 0)
    fatal_error (input_location, "cannot read PCH file: %m");

  ggc_pch_read (f, mmi.preferred_base);

  void (*pch_save) (FILE *);
  unsigned num_callbacks;
  if (fread (&pch_save, sizeof (pch_save), 1, f) != 1
      || fread (&num_callbacks, sizeof (num_callbacks), 1, f) != 1)
    fatal_error (input_location, "cannot read PCH file: %m");
  if (pch_save != &gt_pch_save)
    {
      uintptr_t binbias = (uintptr_t) &gt_pch_save - (uintptr_t) pch_save;
      void **ptrs = XNEWVEC (void *, num_callbacks);
      unsigned i;
      uintptr_t bias
	= (uintptr_t) mmi.preferred_base - (uintptr_t) orig_preferred_base;

      if (fread (ptrs, sizeof (void *), num_callbacks, f) != num_callbacks)
	fatal_error (input_location, "cannot read PCH file: %m");
      for (i = 0; i < num_callbacks; ++i)
	{
	  void *ptr = (void *) ((uintptr_t) ptrs[i] + bias);
	  memcpy (&pch_save, ptr, sizeof (pch_save));
	  pch_save = (void (*) (FILE *)) ((uintptr_t) pch_save + binbias);
	  memcpy (ptr, &pch_save, sizeof (pch_save));
	}
      XDELETE (ptrs);
    }
  else if (fseek (f, num_callbacks * sizeof (void *), SEEK_CUR) != 0)
    fatal_error (input_location, "cannot read PCH file: %m");

  gt_pch_restore_stringpool ();

  /* Barring corruption of the PCH file, the restored line table should be
     complete and usable.  */
  line_table = new_line_table;
}

/* Default version of HOST_HOOKS_GT_PCH_GET_ADDRESS when mmap is not present.
   Select no address whatsoever, and let gt_pch_save choose what it will with
   malloc, presumably.  */

void *
default_gt_pch_get_address (size_t size ATTRIBUTE_UNUSED,
			    int fd ATTRIBUTE_UNUSED)
{
  return NULL;
}

/* Default version of HOST_HOOKS_GT_PCH_USE_ADDRESS when mmap is not present.
   Allocate SIZE bytes with malloc.  Return 0 if the address we got is the
   same as base, indicating that the memory has been allocated but needs to
   be read in from the file.  Return -1 if the address differs, to relocation
   of the PCH file would be required.  */

int
default_gt_pch_use_address (void *&base, size_t size, int fd ATTRIBUTE_UNUSED,
			    size_t offset ATTRIBUTE_UNUSED)
{
  void *addr = xmalloc (size);
  return (addr == base) - 1;
}

/* Default version of HOST_HOOKS_GT_PCH_GET_ADDRESS.   Return the
   alignment required for allocating virtual memory. Usually this is the
   same as pagesize.  */

size_t
default_gt_pch_alloc_granularity (void)
{
  return getpagesize ();
}

#if HAVE_MMAP_FILE
/* Default version of HOST_HOOKS_GT_PCH_GET_ADDRESS when mmap is present.
   We temporarily allocate SIZE bytes, and let the kernel place the data
   wherever it will.  If it worked, that's our spot, if not we're likely
   to be in trouble.  */

void *
mmap_gt_pch_get_address (size_t size, int fd)
{
  void *ret;

  ret = mmap (NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);
  if (ret == (void *) MAP_FAILED)
    ret = NULL;
  else
    munmap ((caddr_t) ret, size);

  return ret;
}

/* Default version of HOST_HOOKS_GT_PCH_USE_ADDRESS when mmap is present.
   Map SIZE bytes of FD+OFFSET at BASE.  Return 1 if we succeeded at
   mapping the data at BASE, -1 if we couldn't.

   This version assumes that the kernel honors the START operand of mmap
   even without MAP_FIXED if START through START+SIZE are not currently
   mapped with something.  */

int
mmap_gt_pch_use_address (void *&base, size_t size, int fd, size_t offset)
{
  void *addr;

  /* We're called with size == 0 if we're not planning to load a PCH
     file at all.  This allows the hook to free any static space that
     we might have allocated at link time.  */
  if (size == 0)
    return -1;

  addr = mmap ((caddr_t) base, size, PROT_READ | PROT_WRITE, MAP_PRIVATE,
	       fd, offset);

  return addr == base ? 1 : -1;
}
#endif /* HAVE_MMAP_FILE */

#if !defined ENABLE_GC_CHECKING && !defined ENABLE_GC_ALWAYS_COLLECT

/* Modify the bound based on rlimits.  */
static double
ggc_rlimit_bound (double limit)
{
#if defined(HAVE_GETRLIMIT)
  struct rlimit rlim;
# if defined (RLIMIT_AS)
  /* RLIMIT_AS is what POSIX says is the limit on mmap.  Presumably
     any OS which has RLIMIT_AS also has a working mmap that GCC will use.  */
  if (getrlimit (RLIMIT_AS, &rlim) == 0
      && rlim.rlim_cur != (rlim_t) RLIM_INFINITY
      && rlim.rlim_cur < limit)
    limit = rlim.rlim_cur;
# elif defined (RLIMIT_DATA)
  /* ... but some older OSs bound mmap based on RLIMIT_DATA, or we
     might be on an OS that has a broken mmap.  (Others don't bound
     mmap at all, apparently.)  */
  if (getrlimit (RLIMIT_DATA, &rlim) == 0
      && rlim.rlim_cur != (rlim_t) RLIM_INFINITY
      && rlim.rlim_cur < limit
      /* Darwin has this horribly bogus default setting of
	 RLIMIT_DATA, to 6144Kb.  No-one notices because RLIMIT_DATA
	 appears to be ignored.  Ignore such silliness.  If a limit
	 this small was actually effective for mmap, GCC wouldn't even
	 start up.  */
      && rlim.rlim_cur >= 8 * ONE_M)
    limit = rlim.rlim_cur;
# endif /* RLIMIT_AS or RLIMIT_DATA */
#endif /* HAVE_GETRLIMIT */

  return limit;
}

/* Heuristic to set a default for GGC_MIN_EXPAND.  */
static int
ggc_min_expand_heuristic (void)
{
  double min_expand = physmem_total ();

  /* Adjust for rlimits.  */
  min_expand = ggc_rlimit_bound (min_expand);

  /* The heuristic is a percentage equal to 30% + 70%*(RAM/1GB), yielding
     a lower bound of 30% and an upper bound of 100% (when RAM >= 1GB).  */
  min_expand /= ONE_G;
  min_expand *= 70;
  min_expand = MIN (min_expand, 70);
  min_expand += 30;

  return min_expand;
}

/* Heuristic to set a default for GGC_MIN_HEAPSIZE.  */
static int
ggc_min_heapsize_heuristic (void)
{
  double phys_kbytes = physmem_total ();
  double limit_kbytes = ggc_rlimit_bound (phys_kbytes * 2);

  phys_kbytes /= ONE_K; /* Convert to Kbytes.  */
  limit_kbytes /= ONE_K;

  /* The heuristic is RAM/8, with a lower bound of 4M and an upper
     bound of 128M (when RAM >= 1GB).  */
  phys_kbytes /= 8;

#if defined(HAVE_GETRLIMIT) && defined (RLIMIT_RSS)
  /* Try not to overrun the RSS limit while doing garbage collection.
     The RSS limit is only advisory, so no margin is subtracted.  */
 {
   struct rlimit rlim;
   if (getrlimit (RLIMIT_RSS, &rlim) == 0
       && rlim.rlim_cur != (rlim_t) RLIM_INFINITY)
     phys_kbytes = MIN (phys_kbytes, rlim.rlim_cur / ONE_K);
 }
# endif

  /* Don't blindly run over our data limit; do GC at least when the
     *next* GC would be within 20Mb of the limit or within a quarter of
     the limit, whichever is larger.  If GCC does hit the data limit,
     compilation will fail, so this tries to be conservative.  */
  limit_kbytes = MAX (0, limit_kbytes - MAX (limit_kbytes / 4, 20 * ONE_K));
  limit_kbytes = (limit_kbytes * 100) / (110 + ggc_min_expand_heuristic ());
  phys_kbytes = MIN (phys_kbytes, limit_kbytes);

  phys_kbytes = MAX (phys_kbytes, 4 * ONE_K);
  phys_kbytes = MIN (phys_kbytes, 128 * ONE_K);

  return phys_kbytes;
}
#endif

void
init_ggc_heuristics (void)
{
#if !defined ENABLE_GC_CHECKING && !defined ENABLE_GC_ALWAYS_COLLECT
  param_ggc_min_expand = ggc_min_expand_heuristic ();
  param_ggc_min_heapsize = ggc_min_heapsize_heuristic ();
#endif
}

/* GGC memory usage.  */
class ggc_usage: public mem_usage
{
public:
  /* Default constructor.  */
  ggc_usage (): m_freed (0), m_collected (0), m_overhead (0) {}
  /* Constructor.  */
  ggc_usage (size_t allocated, size_t times, size_t peak,
	     size_t freed, size_t collected, size_t overhead)
    : mem_usage (allocated, times, peak),
    m_freed (freed), m_collected (collected), m_overhead (overhead) {}

  /* Equality operator.  */
  inline bool
  operator== (const ggc_usage &second) const
  {
    return (get_balance () == second.get_balance ()
	    && m_peak == second.m_peak
	    && m_times == second.m_times);
  }

  /* Comparison operator.  */
  inline bool
  operator< (const ggc_usage &second) const
  {
    if (*this == second)
      return false;

    return (get_balance () == second.get_balance () ?
	    (m_peak == second.m_peak ? m_times < second.m_times
	     : m_peak < second.m_peak)
	      : get_balance () < second.get_balance ());
  }

  /* Register overhead of ALLOCATED and OVERHEAD bytes.  */
  inline void
  register_overhead (size_t allocated, size_t overhead)
  {
    m_allocated += allocated;
    m_overhead += overhead;
    m_times++;
  }

  /* Release overhead of SIZE bytes.  */
  inline void
  release_overhead (size_t size)
  {
    m_freed += size;
  }

  /* Sum the usage with SECOND usage.  */
  ggc_usage
  operator+ (const ggc_usage &second)
  {
    return ggc_usage (m_allocated + second.m_allocated,
		      m_times + second.m_times,
		      m_peak + second.m_peak,
		      m_freed + second.m_freed,
		      m_collected + second.m_collected,
		      m_overhead + second.m_overhead);
  }

  /* Dump usage with PREFIX, where TOTAL is sum of all rows.  */
  inline void
  dump (const char *prefix, ggc_usage &total) const
  {
    size_t balance = get_balance ();
    fprintf (stderr,
	     "%-48s " PRsa (9) ":%5.1f%%" PRsa (9) ":%5.1f%%"
	     PRsa (9) ":%5.1f%%" PRsa (9) ":%5.1f%%" PRsa (9) "\n",
	     prefix,
	     SIZE_AMOUNT (balance), get_percent (balance, total.get_balance ()),
	     SIZE_AMOUNT (m_collected),
	     get_percent (m_collected, total.m_collected),
	     SIZE_AMOUNT (m_freed), get_percent (m_freed, total.m_freed),
	     SIZE_AMOUNT (m_overhead),
	     get_percent (m_overhead, total.m_overhead),
	     SIZE_AMOUNT (m_times));
  }

  /* Dump usage coupled to LOC location, where TOTAL is sum of all rows.  */
  inline void
  dump (mem_location *loc, ggc_usage &total) const
  {
    char *location_string = loc->to_string ();

    dump (location_string, total);

    free (location_string);
  }

  /* Dump footer.  */
  inline void
  dump_footer ()
  {
    dump ("Total", *this);
  }

  /* Get balance which is GGC allocation leak.  */
  inline size_t
  get_balance () const
  {
    return m_allocated + m_overhead - m_collected - m_freed;
  }

  typedef std::pair<mem_location *, ggc_usage *> mem_pair_t;

  /* Compare wrapper used by qsort method.  */
  static int
  compare (const void *first, const void *second)
  {
    const mem_pair_t mem1 = *(const mem_pair_t *) first;
    const mem_pair_t mem2 = *(const mem_pair_t *) second;

    size_t balance1 = mem1.second->get_balance ();
    size_t balance2 = mem2.second->get_balance ();

    return balance1 == balance2 ? 0 : (balance1 < balance2 ? 1 : -1);
  }

  /* Dump header with NAME.  */
  static inline void
  dump_header (const char *name)
  {
    fprintf (stderr, "%-48s %11s%17s%17s%16s%17s\n", name, "Leak", "Garbage",
	     "Freed", "Overhead", "Times");
  }

  /* Freed memory in bytes.  */
  size_t m_freed;
  /* Collected memory in bytes.  */
  size_t m_collected;
  /* Overhead memory in bytes.  */
  size_t m_overhead;
};

/* GCC memory description.  */
static mem_alloc_description<ggc_usage> ggc_mem_desc;

/* Dump per-site memory statistics.  */

void
dump_ggc_loc_statistics ()
{
  if (! GATHER_STATISTICS)
    return;

  ggc_collect (GGC_COLLECT_FORCE);

  ggc_mem_desc.dump (GGC_ORIGIN);
}

/* Record ALLOCATED and OVERHEAD bytes to descriptor NAME:LINE (FUNCTION).  */
void
ggc_record_overhead (size_t allocated, size_t overhead, void *ptr MEM_STAT_DECL)
{
  ggc_usage *usage = ggc_mem_desc.register_descriptor (ptr, GGC_ORIGIN, false
						       FINAL_PASS_MEM_STAT);

  ggc_mem_desc.register_object_overhead (usage, allocated + overhead, ptr);
  usage->register_overhead (allocated, overhead);
}

/* Notice that the pointer has been freed.  */
void
ggc_free_overhead (void *ptr)
{
  ggc_mem_desc.release_object_overhead (ptr);
}

/* After live values has been marked, walk all recorded pointers and see if
   they are still live.  */
void
ggc_prune_overhead_list (void)
{
  typedef hash_map<const void *, std::pair<ggc_usage *, size_t > > map_t;

  map_t::iterator it = ggc_mem_desc.m_reverse_object_map->begin ();

  for (; it != ggc_mem_desc.m_reverse_object_map->end (); ++it)
    if (!ggc_marked_p ((*it).first))
      {
        (*it).second.first->m_collected += (*it).second.second;
	ggc_mem_desc.m_reverse_object_map->remove ((*it).first);
      }
}

/* Print memory used by heap if this info is available.  */

void
report_heap_memory_use ()
{
#if defined(HAVE_MALLINFO) || defined(HAVE_MALLINFO2)
#ifdef HAVE_MALLINFO2
  #define MALLINFO_FN mallinfo2
#else
  #define MALLINFO_FN mallinfo
#endif
  if (!quiet_flag)
    fprintf (stderr, " {heap " PRsa (0) "}",
	     SIZE_AMOUNT (MALLINFO_FN ().arena));
#endif
}

/* Forcibly clear all GTY roots.  */

void
ggc_common_finalize ()
{
  const struct ggc_root_tab *const *rt;
  const_ggc_root_tab_t rti;

  for (rt = gt_ggc_deletable_rtab; *rt; rt++)
    for (rti = *rt; rti->base != NULL; rti++)
      memset (rti->base, 0, rti->stride * rti->nelt);

  for (rt = gt_ggc_rtab; *rt; rt++)
    ggc_zero_rtab_roots (*rt);

  for (rt = gt_pch_scalar_rtab; *rt; rt++)
    for (rti = *rt; rti->base != NULL; rti++)
      memset (rti->base, 0, rti->stride * rti->nelt);
}
