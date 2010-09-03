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


with Types; use Types;

package Misc is

   function default_pass_by_ref (type_node : Node_Id) return Boolean;
   --  We pass aggregates by reference if they are sufficiently large.  The
   --  choice of constant here is somewhat arbitrary.  Objects of variable
   --  size and those required to be passed by reference also return True;



   function must_pass_by_ref (type_node : Node_Id) return Boolean;
   --  We pass only unconstrained objects, those required by the language
   --  to be passed by reference, and objects of variable size.  The latter
   --  is more efficient, avoids problems with variable size temporaries,
   --  and does not produce compatibility problems with C, since C does not
   --  have such objects.


end Misc;
