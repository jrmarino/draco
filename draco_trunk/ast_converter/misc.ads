--
--  DRACO ADA COMPILER
--  Misc Functions
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


with Core_h; use Core_h;

package Misc is

   function default_pass_by_ref (llvm_type : in LLVMTypeRef) return Boolean;
   --  We pass aggregates by reference if they are sufficiently large.  The
   --  choice of constant her is somewhat arbitrary.  We also pass by
   --  reference if the target machine would either pass or return by
   --  reference.  Strictly speaking, we need only check the return if this
   --  is an In Out parameter, but it's probably best to err on the side of
   --  passing more things by reference.



   function must_pass_by_ref (llvm_type : in LLVMTypeRef) return Boolean;
   --  We pass only unconstrained objects, those required by the language
   --  to be passed by reference, and objects of variable size.  The latter
   --  is more efficient, avoids problems with variable size temporaries,
   --  and does not produce compatibility problems with C, since C does not
   --  have such objects.


end Misc;
