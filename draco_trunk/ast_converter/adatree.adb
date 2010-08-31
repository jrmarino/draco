--
--  DRACO ADA COMPILER
--  DLC Global Variables
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

package body AdaTree is

   -------------------------
   --  type_is_padding_p  --
   -------------------------

   function type_is_padding_p (node : TTree)
   return Boolean is
      result      : Boolean := False;
      TypeRef     : LLVMTypeRef;
      record_type : Boolean;
   begin
      TypeRef  := get_typeref_from_tree (node);
      record_type := (LLVMGetTypeKind (TypeRef) = LLVMStructTypeKind);
      if record_type then
         result := node.type_lang_flag (5);
      end if;
      return result;
   end type_is_padding_p;


end AdaTree;
