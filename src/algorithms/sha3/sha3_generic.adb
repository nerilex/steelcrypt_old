--  Copyright (C) 2015  Daniel Otte <bg@nerilex.org>
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


package body Sha3_Generic is

   procedure Initialize(Context : out Context_T) is
   begin
      Context.A := (others => (others => 0));
   end Initialize;

   procedure Next_Block(Context : in out Context_T; Block : in Block_T) is
      temp : u64;
   begin
      for i in 0 .. Rate_Bytes / 8 - 1 loop
         temp := Load_le(Block(Block'First + i * 8 .. Block'First + i * 8 + 7));
         Context.A(x_T(i mod 5), y_T(i / 5)) := Context.A(x_T(i mod 5), y_T(i / 5)) xor temp;
      end loop;
      Permute(Context.A);
   end Next_Block;

   procedure Append_Bit(Context : in out Context_T; Index : in out Capacity_T; Value : in Bit) is
   begin
      Context.A(x_t((Index / w) mod 5), y_t(Index / w / 5))  := Context.A(x_t((Index / w) mod 5), y_t(Index / w / 5)) xor Shift_Left(Word_T(Value), Index mod w);
      Index := Index + 1;
      if Index = Rate_Bits then
         Permute(Context.A);
         Index := 0;
      end if;
   end Append_Bit;

   procedure Append_Byte(Context : in out Context_T; Index : in out Capacity_T; Value : in u8) is
   begin
      if Index mod 8 /= 0 then
         raise Constraint_Error;
      end if;
      Context.A(x_t((Index / w) mod 5), y_t(Index / w / 5))  := Context.A(x_t((Index / w) mod 5), y_t(Index / w / 5)) xor Shift_Left(Word_T(Value), Index mod w);
      Index := Index + 8;
      if Index = Rate_Bits then
         Permute(Context.A);
         Index := 0;
      end if;
   end Append_Byte;

   procedure Last_Block(Context : in out Context_T; Block : in u8_Array; Bits : in Integer := -1) is
      Length : Natural;
      Index : Integer := Block'First;
      Dest_Index : Natural := 0;
   begin
      if Bits < 0 then
         Length := Block'Length * 8;
      else
         Length := Bits;
      end if;
      while Length >= Rate_Bits loop
         Next_Block(Context, Block(Index .. Index + Rate_Bytes - 1));
         Index := Index + Rate_Bytes;
         Length := Length - Rate_bits;
      end loop;
      -- append trailing bytes
      while Length >= 8 loop
         Append_Byte(Context, Dest_Index, Block(Index));
         Index := Index + 1;
         Length := Length - 8;
      end loop;
      -- append trailing bits
      for j in 0 .. Length - 1 loop
         Append_Bit(Context, Dest_Index, Bit(Shift_Right(Block(Index), j) mod 2));
      end loop;
      -- append suffix
      for j in 0 .. Suffix_Bit_Length - 1 loop
         Append_Bit(Context, Dest_Index, Bit(Shift_Right(Suffix, Natural(j)) mod 2));
      end loop;
      -- insert Padding
      Append_Bit(Context, Dest_Index, 1);
      Dest_Index := Rate_Bits - 1;
      Append_Bit(Context, Dest_Index, 1);
   end Last_Block;

   procedure Squeeze(Context : in out Context_T; Data : out u8_Array) is
      Temp : u8_Array(1 .. 8);
      Length_A : Integer := Data'First;
      Length_B : Integer;
      j : Natural;
   begin
      for i in 0 .. Data'Length / Rate_Bytes loop
         Length_B := Length_A + Rate_Bytes - 1;
         if Length_B > Data'Last then
            Length_B := Data'Last;
         end if;
         j := 0;
         while Length_A + 8 <= Length_B loop
            Store_le(Data(Length_A .. Length_A + 7), Context.A(x_t(j mod 5), y_t(j / 5)));
            Length_A := Length_A + 8;
            j := j + 1;
         end loop;

         if Length_A <= Length_B then
            Store_le(Temp, Context.A(x_t(j mod 5), y_t(j / 5)));
            Data(Length_A .. Length_B) := Temp(Temp'First .. Temp'First + Length_B - Length_A);
         end if;

         Length_A := Length_B;
         if Length_B < Data'Last then
            Permute(Context.A);
         end if;
      end loop;
   end Squeeze;

   procedure Get_Digest(Context : in out Context_T; Digest : out Digest_T) is
   begin
      Squeeze(Context, Digest);
   end Get_Digest;

   procedure Hash(Data : in u8_array; Digest : out Digest_T; Bits : in Integer := -1) is
      Ctx : Context_T;
   begin
      Initialize(Ctx);
      Last_Block(Ctx, Data, Bits);
      Get_Digest(Ctx, Digest);
   end Hash;

end Sha3_Generic;


