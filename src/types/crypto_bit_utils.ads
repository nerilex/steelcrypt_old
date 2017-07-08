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


   -- --------------------------
   -- - Functions / Procedures -
   -- --------------------------
package Crypto_Bit_Utils is

   subtype Bit_Address_T is Natural range 0 .. T'Size - 1;

   Bytes : constant Positive := (T'Size + 7) / 8;

   --
   function Bit_Get
     (A           : in T;
      Bit_Address :    Bit_Address_T;
      Order : in System.Bit_Order := System.Default_Bit_Order) return Bit;

   --
   procedure Bit_Clear
     (A           : in out T;
      Bit_Address :        Bit_Address_T;
      Order       : in     System.Bit_Order := System.Default_Bit_Order);

   --
   procedure Bit_Set
     (A           : in out T;
      Bit_Address :        Bit_Address_T;
      Value       :        Bit              := 1;
      Order       : in     System.Bit_Order := System.Default_Bit_Order);

   --
   procedure Bit_Toggle
     (A           : in out T;
      Bit_Address :        Bit_Address_T;
      Order       : in     System.Bit_Order := System.Default_Bit_Order);


   -- swap two elements
   procedure Swap (A, B : in out T);

   pragma Inline (Bit_Get);
   pragma Inline (Bit_Clear);
   pragma Inline (Bit_Set);
   pragma Inline (Bit_Toggle);
   pragma Inline (Swap);


end Crypto_Bit_Utils;
