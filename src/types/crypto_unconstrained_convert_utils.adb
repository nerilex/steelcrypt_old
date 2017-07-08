--  Copyright (C) 2017  bg nerilex <bg@nerilex.org>
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

-- ----------------------------------
-- - Generic Functions / Procedures -
-- ----------------------------------

-- --------------------------
-- - Functions / Procedures -
-- --------------------------
package body Crypto_Unconstrained_Convert_Utils is

   function To_Bytes (A : T) return u8_Array is
      Temp : u8_Array (1 .. Bytes) := (others => 0);
      j : Natural range 0 .. Bytes := Bytes;
      k : Natural range 0 .. 8 := 8;
   begin
      for i in Bit_Address_T'Range loop
         if k = 0 then
            j := j - 1;
            k := 8;
         end if;
         Temp(j) := Shift_Left(Temp(j), 1);
         Temp(j) := Temp(j) or u8(Bit_Get(A, i));
         k := k - 1;
      end loop;
      return Temp;
   end To_Bytes;

   function From_Bytes (A : u8_Array) return T is
      Temp : T := 0;
      x : u8;
      j : Integer range 0 .. Bytes := Bytes;
      k : Natural range 0 .. 8 := 0;
   begin
      for i in Bit_Address_T'Range loop
         if k = 0 then
            x := A(j);
            j := j - 1;
            k := 8;
         end if;
         Bit_Set(Temp, i, Bit(x and 1));
         x := Shift_Right(x, 1);
         k := k - 1;
      end loop;
      return Temp;
   end From_Bytes;

   function To_Bytes (A : T_Array; In_Order : Bit_Order; Out_Order : Bit_Order) return u8_Array is
      r : u8_Array(1 .. (A'Length * T'Size + 7) / 8) := (others => 0);
   begin
      for Index in 0 .. A'Length * T'Size - 1 loop
         Bit_Set(A => r,
                 Bit_Address => Index,
                 Value => Bit_Get(A, Index, In_Order),
                 Order => Out_order);
         end loop;
      return r;
   end To_Bytes;

   function From_Bytes (A : u8_Array;  In_Order : Bit_Order; Out_Order : Bit_Order) return T_Array is
      r : T_Array(1 .. (A'Length * 8 + T'Size - 1) / T'Size) := (others => 0);
   begin
      for Index in 0 .. r'Length * T'Size - 1 loop
         Bit_Set(A => r,
                 Bit_Address => Index,
                 Value => Bit_Get(A, Index, In_Order),
                 Order => Out_order);
      end loop;
      return r;
   end From_Bytes;

   function To_Hex (A : T; Upper_Case : Boolean := False) return String is
   begin
      return To_Hex (To_Bytes(A), Upper_Case => Upper_Case);
   end To_Hex;

end Crypto_Unconstrained_Convert_Utils;
