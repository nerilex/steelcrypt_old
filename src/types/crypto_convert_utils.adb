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
package body Crypto_Convert_Utils is

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

   -- The semantics of "Bit-Order" and "Byte-Order" get a bit complex here ...
   -- we keep the relative numeric value and use the semantic of which bit is
   -- "first" to derive the order in which values are stored.
--     function From_Packed (A : u8_Array; Order : System.Bit_Order := System.Default_Bit_Order) return T_Array is
--        Total_Bits : Integer := T_Array'Length * T'Size;
--        ret : T_Array := (others => 0);
--     begin
--        for i in 0 .. Total_Bits - 1 loop
--           Bit_Set(A => ret,
--                   Bit_Address => i,
--                   Value => Bit_Get(A, i, Order),
--                   Order => Order);
--        end loop;
--        return ret;
--     end From_Packed;

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
      r : T_Array := (others => 0);
   begin
      for Index in 0 .. r'Length * T'Size - 1 loop
         Bit_Set(A => r,
                 Bit_Address => Index,
                 Value => Bit_Get(A, Index, In_Order),
                 Order => Out_order);
      end loop;
      return r;
   end From_Bytes;

--     -- load a value which is stored big-endian in byte Array
--     function Load_le (A : u8_Array) return T_Array is
--        tmp : u8;
--        ret : T_Array := (others => 0);
--        j : Integer := A'First;
--        k : Natural := 0;
--     begin
--        for i in T_Array_Index loop
--           for z in 0 .. T'Size - 1 loop
--              if k = 0 then
--                 tmp := A(j);
--                 j := j + 1;
--              end if;
--              Bit_Set(A => ret(i), Bit_Address => z, Value => Bit_Get(tmp, k));
--              k := (k + 1) mod u8'Size;
--           end loop;
--        end loop;
--        return ret;
--     end Load_le;

   -- XXX load a value which is stored big-endian in byte Array
   --     function Load_be (A : u8_Array) return T_Array is
   --        r : T_Array;
   --     begin
   --        for i in r'Range loop
   --           r (i) :=
   --             Load_be
   --               (A
   --                  (((i - 1) * Bytes + A'First) ..
   --                   ((i - 1) * Bytes + Bytes + A'First - 1)));
   --        end loop;
   --        return r;
   --     end Load_be;
   --
   --     -- load a value which is stored little-endian in byte Array
   --     function Load_le (A : u8_Array) return T is
   --        r : T := 0;
   --     begin
   --        for i in reverse 0 .. (T'Size / 8 - 1) loop
   --           r := Shift_Left (r, 8) or T (A (A'First + i));
   --        end loop;
   --        return r;
   --     end Load_le;
   --
   --     -- XXX load a value which is stored big-endian in byte Array
   --     function Load_le (A : u8_Array) return T_Array is
   --        r : T_Array (1 .. (A'Length * 8 + T'Size - 1) / T'Size);
   --     begin
   --        for i in r'Range loop
   --           r (i) :=
   --             Load_le
   --               (A
   --                  (((i - 1) * Bytes + A'First) ..
   --                   ((i - 1) * Bytes + Bytes + A'First - 1)));
   --        end loop;
   --        return r;
   --     end Load_le;
   --
   --     -- store a value in big-endian format in a byte Array
   --     procedure Store_be (A : out u8_Array; value : in T) is
   --        x : T := value;
   --        b : u8;
   --     begin
   --        for i in reverse 0 .. (T'Size / 8 - 1) loop
   --           b               := u8 (x and 16#FF#);
   --           A (A'First + i) := b;
   --           x               := Shift_Right (x, 8);
   --        end loop;
   --     end Store_be;
   --
   --     -- XXX store a value in big-endian format in a byte Array
   --     procedure Store_be (A : out u8_Array; value : in T_Array) is
   --     begin
   --        for i in value'Range loop
   --           Store_be
   --             (A
   --                (((i - value'First) * Bytes + A'First) ..
   --                 ((i - value'First) * Bytes + A'First) + Bytes - 1),
   --              value (i));
   --        end loop;
   --     end Store_be;
   --
   --     -- store a value in little-endian format in a byte Array
   --     procedure Store_le (A : out u8_Array; value : in T) is
   --        x : T := value;
   --        b : u8;
   --     begin
   --        for i in 0 .. (T'Size / 8 - 1) loop
   --           b               := u8 (x and 16#FF#);
   --           A (A'First + i) := b;
   --           x               := Shift_Right (x, 8);
   --        end loop;
   --     end Store_le;
   --
   --     -- XXX store a value in big-endian format in a byte Array
   --     procedure Store_le (A : out u8_Array; value : in T_Array) is
   --     begin
   --        for i in value'Range loop
   --           Store_le
   --             (A
   --                (((i - value'First) * Bytes + A'First) ..
   --                 ((i - value'First) * Bytes + A'First) + Bytes - 1),
   --              value (i));
   --        end loop;
   --     end Store_le;


   function To_Hex (A : T; Upper_Case : Boolean := False) return String is
   begin
      return To_Hex (To_Bytes(A), Upper_Case => Upper_Case);
   end To_Hex;

end Crypto_Convert_Utils;
