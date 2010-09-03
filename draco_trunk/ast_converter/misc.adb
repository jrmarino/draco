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


with Sinfo;   use Sinfo;
with Einfo;   use Einfo;
with Sem_Aux; use Sem_Aux;
with Uintp;   use Uintp;
with Atree;   use Atree;

package body Misc is

   ------------------------
   --  must_pass_by_ref  --
   ------------------------

   function must_pass_by_ref (type_node : in Node_Id)
   return Boolean is
      kind   : constant Node_Kind := Nkind (type_node);
      entity : constant Entity_Id := Etype (type_node);
      result : Boolean;
   begin
      result := (kind = N_Unconstrained_Array_Definition) or else
                Is_By_Reference_Type (entity) or else
                not Size_Known_At_Compile_Time (entity);

      return result;
   end must_pass_by_ref;



   ---------------------------
   --  default_pass_by_ref  --
   ---------------------------

   function default_pass_by_ref (type_node : in Node_Id)
   return Boolean is
      kind        : constant Node_Kind := Nkind (type_node);
      entity      : constant Entity_Id := Etype (type_node);
      type_align  : constant Uint := Alignment (entity);
      type_size   : constant Uint := Esize (entity);
   begin
      if Is_By_Reference_Type (entity) then
         return True;
      end if;

      if not Size_Known_At_Compile_Time (entity) then
         return True;
      end if;

      if (kind = N_Aggregate) and (type_size > type_align * 8) then
         return True;
      end if;

      return False;
   end default_pass_by_ref;

end Misc;
