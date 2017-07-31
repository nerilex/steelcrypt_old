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

with Crypto.Types.Bit_Utils;
with Crypto.Types.Constrained_Block_Bit_Utils;
with Crypto.Types.Block_Bit_Utils;

-- --------------------------
-- - Generic Functions / Procedures -
-- --------------------------

generic
   type T is mod <>;

   with function Shift_Left (Value : T; Amount : Natural) return T is <>;

   with function Shift_Right (Value : T; Amount : Natural) return T is <>;

   type T_Array_Index is new Integer;

   type T_Array is array (T_Array_Index) of T;

   -- --------------------------
   -- - Functions / Procedures -
   -- --------------------------
package Crypto.Types.Convert_Utils is
   Bytes : constant Positive := (T'Size + 7) / 8;
   Array_Bytes : constant Positive := (T'Size * T_Array'Length + 7) / 8;

--   type T_Byte_Array_Index is range 1 .. Array_Bytes;
--   type T_Byte_Array is Array(T_Byte_Array_Index) of Byte;

   package Bit_Utils is new Crypto.Types.Bit_Utils  (T => T);
   use Bit_Utils;
   package u8_Bit_Utils is new Crypto.Types.Bit_Utils
     ( T => u8,
       Shift_Left => Crypto.Types.Shift_Left,
       Shift_Right => Crypto.Types.Shift_Right );

   package T_Block_Utils is new Crypto.Types.Constrained_Block_Bit_Utils
     (T             => T,
      T_Array_Index => T_Array_Index,
      T_Array       => T_Array,
      Bit_Address_T => Bit_Address_T,
      Bit_Get    => Bit_Get,
      Bit_Clear  => Bit_Clear,
      Bit_Set    => Bit_Set,
      Bit_Toggle => Bit_Toggle);
   use T_Block_Utils;

   package u8_Block_Utils is new Crypto.Types.Block_Bit_Utils
     (T             => u8,
      T_Array       => u8_Array,
      Bit_Address_T => u8_Bit_Utils.Bit_Address_T,
      Bit_Get       => u8_Bit_Utils.Bit_Get,
      Bit_Clear     => u8_Bit_Utils.Bit_Clear,
      Bit_Set       => u8_Bit_Utils.Bit_Set,
      Bit_Toggle    => u8_Bit_Utils.Bit_Toggle);
   use u8_Block_Utils;


   -- compare two array with timing independent of content
   -- function "="(Left, Right : T_Array ) return Boolean;

   function To_Bytes (A : T) return u8_Array;
   function From_Bytes (A : u8_Array) return T;

   function To_Bytes (A : T_Array; In_Order : Bit_Order; Out_Order : Bit_Order) return u8_Array;
   function From_Bytes (A : u8_Array;  In_Order : Bit_Order; Out_Order : Bit_Order) return T_Array;

   function To_Hex (A : T; Upper_Case : Boolean := False) return String;

   pragma Inline (To_Bytes);
   pragma Inline (From_Bytes);
   pragma Inline (To_Hex);


end Crypto.Types.Convert_Utils;
