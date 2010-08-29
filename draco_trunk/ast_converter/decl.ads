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


with Core_h;   use Core_h;
with Types;    use Types;
with System;   use System;
with LlvmTree; use LlvmTree;
with Utils01;

package Decl is

   type TDefinition is (none, exists, special);

   function gnat_to_llvm_entity (gnat_entity  : Entity_Id;
                                 llvm_expr    : TTree;
                                 definition   : TDefinition)
   return TTree;
   --  Given GNAT_ENTITY, a GNAT defining identifier node, which denotes some
   --  Ada entity, return the equivalent LLVM tree for that entity and
   --  associate the tree with the input GNAT defining identifier.
   --
   --  If GNAT_ENTITY is a variable or a constant declaration, LLVM_EXPR gives
   --  its initial value (how?).  This is optional for a variable.  For a
   --  renamed entity, LLVM_EXPR gives the object being renamed.
   --
   --  DEFINITION is not "none" if this call is intended for a definition.
   --  This is used for separate compilation where it is necessary to know
   --  whether an external declaration or a definition must be created if the
   --  LLVM equivalent was not created previously.  The value of "exists" is
   --  normally used for an existing DEFINITION, but a value of "special" is
   --  used in special circumstances, defined in the code.



   function get_unpadded_type (gnat_entity : Entity_Id) return TTree;
   --  Similar, but the GNAT_ENTITY is assumed to refer to a GNAT type.
   --  Return the unpadded version of the LLVM type corresponding to that
   --  entity.



   function gnat_to_llvm_type (gnat_entity  : Entity_Id) return TTree;
   --  Similar, but GNAT_ENTITY is assumed to refer to a GNAT type.
   --  Return the LLVM type corresponding to that entity

end Decl;
