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

with Crypto.Types; use Crypto.Types;
with System;

-- --------------------------
-- - Generic Functions / Procedures -
-- --------------------------

generic
   type T is mod <>;
   type T_Array_Index is range <>;
   type T_Array is array (T_Array_Index) of T;
   type Bit_Address_T is range <>;

with function Bit_Get
  (A           : in T;
   Bit_Address :    Bit_Address_T;
   Order : in System.Bit_Order := System.Default_Bit_Order) return Bit;

--
with procedure Bit_Clear
  (A           : in out T;
   Bit_Address :        Bit_Address_T;
   Order       : in     System.Bit_Order := System.Default_Bit_Order);

--
with procedure Bit_Set
  (A           : in out T;
   Bit_Address :        Bit_Address_T;
   Value       :        Bit              := 1;
   Order       : in     System.Bit_Order := System.Default_Bit_Order);

--
with procedure Bit_Toggle
  (A           : in out T;
   Bit_Address :        Bit_Address_T;
   Order       : in     System.Bit_Order := System.Default_Bit_Order);

   -- --------------------------
   -- - Functions / Procedures -
   -- --------------------------
package Crypto.Types.Constrained_Block_Bit_Utils is


   subtype Bit_Block_Address_T is Natural;

   Bytes : constant Positive := (T'Size + 7) / 8;

   --
   function Bit_Get
     (A           : in T_Array;
      Bit_Address :    Bit_Block_Address_T;
      Order : in System.Bit_Order := System.Default_Bit_Order) return Bit;

   --
   procedure Bit_Clear
     (A           : in out T_Array;
      Bit_Address :        Bit_Block_Address_T;
      Order       : in     System.Bit_Order := System.Default_Bit_Order);

   --
   procedure Bit_Set
     (A           : in out T_Array;
      Bit_Address :        Bit_Block_Address_T;
      Value       :        Bit              := 1;
      Order       : in     System.Bit_Order := System.Default_Bit_Order);

   --
   procedure Bit_Toggle
     (A           : in out T_Array;
      Bit_Address :        Bit_Block_Address_T;
      Order       : in     System.Bit_Order := System.Default_Bit_Order);

 pragma Inline (Bit_Get);
 pragma Inline (Bit_Clear);
 pragma Inline (Bit_Set);
 pragma Inline (Bit_Toggle);

end Crypto.Types.Constrained_Block_Bit_Utils;
