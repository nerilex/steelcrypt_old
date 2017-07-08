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

-- ----------------------------------
-- - Generic Functions / Procedures -
-- ----------------------------------

-- --------------------------
-- - Functions / Procedures -
-- --------------------------
   package body Crypto_Bit_Utils is

   --
   function Bit_Get
     (A           : in T;
      Bit_Address :    Bit_Address_T;
      Order       : in System.Bit_Order := System.Default_Bit_Order) return Bit
   is
      use System;
      Temp : T;
   begin
      if Order = High_Order_First then
         Temp := Shift_Right (A, T'Size - Bit_Address - 1);
      else
         Temp := Shift_Right (A, Bit_Address);
      end if;
      return Bit (Temp and 1);
   end Bit_Get;

   --
   procedure Bit_Clear
     (A           : in out T;
      Bit_Address :        Bit_Address_T;
      Order       : in     System.Bit_Order := System.Default_Bit_Order)
   is
      use System;
   begin
      if Order = High_Order_First then
         A := A and not Shift_Left (1, T'Size - Bit_Address - 1);
      else
         A := A and not Shift_Left (1, Bit_Address);
      end if;
   end Bit_Clear;

   --
   procedure Bit_Set
     (A           : in out T;
      Bit_Address :        Bit_Address_T;
      Value       :        Bit              := 1;
      Order       : in     System.Bit_Order := System.Default_Bit_Order)
   is
      use System;
   begin
      if Value = 0 then
         Bit_Clear (A => A, Bit_Address => Bit_Address, Order => Order);
      else
         if Order = High_Order_First then
            A := A or Shift_Left (1, T'Size - Bit_Address - 1);
         else
            A := A or Shift_Left (1, Bit_Address);
         end if;
      end if;
   end Bit_Set;

   --
   procedure Bit_Toggle
     (A           : in out T;
      Bit_Address :        Bit_Address_T;
      Order       : in     System.Bit_Order := System.Default_Bit_Order)
   is
      use System;
   begin
      if Order = High_Order_First then
         A := A xor Shift_Left (1, T'Size - Bit_Address - 1);
      else
         A := A xor Shift_Left (1, Bit_Address);
      end if;
   end Bit_Toggle;

   -- swap two elements
   procedure Swap (A, B : in out T) is
      temp : T;
   begin
      temp := A;
      A    := B;
      B    := temp;
   end Swap;

   function To_Bytes (A : T) return u8_Array is
      Temp : u8_Array (1 .. Bytes) := (others => 0);
      j : Integer range 0 .. Bytes := Bytes;
      k : Natural range 0 .. 8 := 8;
   begin
      for i in 0 .. T'Size - 1 loop
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
      for i in 0 .. T'Size - 1 loop
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

end Crypto_Bit_Utils;
