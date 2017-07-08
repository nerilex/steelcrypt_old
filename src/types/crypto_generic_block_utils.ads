--  Copyright (C) 2015  bg nerilex <bg@nerilex.org>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Crypto_Core_Types; use Crypto_Core_Types;
with System;

-- --------------------------
-- - Generic Functions / Procedures -
-- --------------------------

generic
   type T is mod <>;

   with function Shift_Left (Value : T; Amount : Natural) return T is <>;

   with function Shift_Right (Value : T; Amount : Natural) return T is <>;

   with function Rotate_Left (Value : T; Amount : Natural) return T is <>;

   with function Rotate_Right (Value : T; Amount : Natural) return T is <>;

   type T_Array_Index is new Integer;

   type T_Array is array (T_Array_Index) of T;

   -- --------------------------
   -- - Functions / Procedures -
   -- --------------------------
package Crypto_Generic_Block_Utils is

   subtype Bit_Address_T is Natural range 0 .. (T'Size - 1);
   subtype Bit_Address_Array_T is Natural range 0 .. (T_Array'Length * T'Size - 1);

   Bytes : constant Positive := (T'Size + 7) / 8;
   -- compare two array with timing independent of content
   -- function "="(Left, Right : T_Array ) return Boolean;

   -- xor each element on the left with the corresponding element on the right
   function "xor" (Left, Right : T_Array) return T_Array;

   -- xor the left element with each element on the right
   function "xor" (Left : T; Right : T_Array) return T;

   -- xor each element on the left with the element on the right
   function "xor" (Left : T_Array; Right : T) return T_Array;

   -- and each element on the left with the corresponding element on the right
   function "and" (Left, Right : T_Array) return T_Array;

   -- and the left element with each element on the right
   function "and" (Left : T; Right : T_Array) return T;

   -- and each element on the left with the element on the right
   function "and" (Left : T_Array; Right : T) return T_Array;

   -- or each element on the left with the corresponding element on the right
   function "or" (Left, Right : T_Array) return T_Array;

   -- or the left element with each element on the right
   function "or" (Left : T; Right : T_Array) return T;

   -- or each element on the left with the element on the right
   function "or" (Left : T_Array; Right : T) return T_Array;

   -- add each element on the left with the corresponding element on the right
   function "+" (Left, Right : T_Array) return T_Array;

   -- add the left element with each element on the right
   function "+" (Left : T; Right : T_Array) return T;

   -- add each element on the left with the element on the right
   function "+" (Left : T_Array; Right : T) return T_Array;

   -- subtract from each element on the left the corresponding element on the right
   function "-" (Left, Right : T_Array) return T_Array;

   -- subtract from the left element each element on the right
   function "-" (Left : T; Right : T_Array) return T;

   -- subtract from each element on the left the element on the right
   function "-" (Left : T_Array; Right : T) return T_Array;

   -- rotate the Array to the left without changing the values of the elements
   function Rotate_Array_Left (A : T_Array; Amount : Natural) return T_Array;

   -- rotate the Array to the right without changing the values of the elements
   function Rotate_Array_Right (A : T_Array; Amount : Natural) return T_Array;

   -- rotate the whole Array as continues big-endian integer; positive Amount rotates left (towards lower address)
   function Rotate_be (A : T_Array; Amount : Integer) return T_Array;

   -- rotate the whole Array as continues little-endian integer; positive Amount rotates left (towards higher address)
   function Rotate_le (A : T_Array; Amount : Integer) return T_Array;

   -- rotate each element by Amount to the left; negative values for Amount rotate to the right
   function Rotate_each (A : T_Array; Amount : Integer) return T_Array;

   -- shift the whole Array as continues big-endian integer; positive Amount shifts left (towards lower address)
   function Shift_be (A : T_Array; Amount : Integer) return T_Array;

   -- Shift the whole Array as continues little-endian integer; positive Amount shifts left (towards higher address)
   function Shift_le (A : T_Array; Amount : Integer) return T_Array;

   -- shift each element by Amount to the left; negative values for Amount shift to the right
   function Shift_each (A : T_Array; Amount : Integer) return T_Array;
   --

   pragma Inline ("xor");
   pragma Inline ("and");
   pragma Inline ("or");
   pragma Inline ("+");
   pragma Inline ("-");
   pragma Inline (Rotate_be);
   pragma Inline (Rotate_le);
   pragma Inline (Rotate_each);
   pragma Inline (Rotate_Array_Left);
   pragma Inline (Rotate_Array_Right);
   pragma Inline (Shift_be);
   pragma Inline (Shift_le);
   pragma Inline (Shift_each);

end Crypto_Generic_Block_Utils;
