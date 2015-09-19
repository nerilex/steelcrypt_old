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

with Crypto_Types; use Crypto_Types;
use Crypto_Types.Crypto_Utils_u8;
use Crypto_Types.Crypto_Utils_u32;
use Crypto_Types.Crypto_Utils_u64;
with System; use System;

with Ada.Text_IO; use Ada.Text_IO;

package body GCM128_Spec is

   function GFmul(X,Y : Block_128_Bit) return Block_128_Bit is
      Mask : constant array(Bit) of u8 := (0, 2#1110_0001#);
      V : array (Bit) of Block_128_Bit := ( (others => 0), Y);
      Z : Block_128_Bit := (others => 0);
      Temp : Bit;
   begin
      for i in 0 .. 127 loop
         Z := Z xor V(Bit_Get(X, i, High_Order_First));
         Temp := Bit_Get(V(1), 127, High_Order_First);
         V(1) := Shift_be(V(1), -1);
         V(1)(1) := V(1)(1) xor Mask(Temp);
      end loop;
      return Z;
   end GFmul;

   function GHash(Key : in Block_128_Bit; Data : in u8_Array; Seed : Block_128_Bit := (others => 0)) return Block_128_Bit is
      A : Block_128_Bit := Seed;
      Index : Integer := Data'First;
      Length : Integer := Data'Length * 8;
   begin
      -- for each complete block
      for i in 1 .. Length / Block_Size_Bits loop
         A := A xor Data(Index .. Index + Block_Size_Bytes - 1);
         Index := Index + Block_Size_Bytes;
         Length := Length - Block_Size_Bits;
         A := GFmul(A, Key);
      end loop;
      if Length > 0 then
         A(1 .. Length / 8) := A(1 .. Length / 8) xor Data(Index .. Index + Length / 8 - 1);
         if Length mod 8 /= 0 then
            A(Length / 8 + 1) := A(Length / 8 + 1) xor (Data(Index + Length / 8) and not Shift_Right(u8'(16#FF#), Length mod 8));
         end if;
         A := GFmul(A, Key);
      end if;
      return A;
   end;

   procedure Inc_32(A : in out Block_128_Bit) is
      c : u32;
   begin
      c := Load_be(A(13 .. 16));
      c := c + 1;
      Store_be(A(13 .. 16), c);
   end Inc_32;

   procedure Initialize(Context : out Context_T; Key : in u8_Array; IV : in u8_Array) is
   begin
      Cipher.Initialize(Context.Cipher_Ctx, Key);
      Context.Header_Length := 0;
      Context.Plaintext_Length := 0;
      Context.Y := (others => 0);
      Context.H := (others => 0);
      Cipher.Encrypt(Context.Cipher_Ctx, Context.H);
      if IV'Length = 96 / 8 then
         Context.J0( 1 .. 12) := IV;
         Context.J0(13 .. 16) := (0, 0, 0, 1);
      else
         declare
            IV_Length_Block : Block_64_Bit;
         begin
            Context.J0 := GHash(Context.H, IV);
            Store_be(IV_Length_Block, u64(IV'Length * 8));
            Context.J0(9 .. 16) := Context.J0(9 .. 16) xor IV_Length_Block;
            Context.J0 := GHash(Context.H, Context.J0);
         end;
      end if;
      Context.ICB := Context.J0;
      -- encrypt J0 for final XOR on tag
      Cipher.Encrypt(Context.Cipher_Ctx, Context.J0);
   end Initialize;

   procedure Header_Next_Block(Context : in out Context_T; Header : in u8_Array) is
   begin
      if Header'Length mod Block_Size_Bytes /= 0 then
         raise Format_Violation;
      end if;
      Context.Y := GHash(Context.H, Header, Seed => Context.Y);
      Context.Header_Length := Context.Header_Length + Header'Length * 8;
   end;

   procedure Header_Last_Block(Context : in out Context_T; Header : in u8_Array) is
   begin
      Context.Y := GHash(Context.H, Header, Seed => Context.Y);
      Context.Header_Length := Context.Header_Length + Header'Length * 8;
   end;

   procedure Encrypt_Next_Block(Context : in out Context_T; Block : in out u8_Array) is
      Temp : Block_128_Bit;
   begin
      if Block'Length /= Block_Size_Bytes then
         raise Format_Violation;
      end if;
      Inc_32(Context.ICB);
      Temp := Context.ICB;
      Cipher.Encrypt(Context.Cipher_Ctx, Temp);
      Block := Block xor Temp;
      Context.Y := GHash(Context.H, Block, Seed => Context.Y);
      Context.Plaintext_Length := Context.Plaintext_Length + u64(Block_Size_Bits);
   end;

   procedure Encrypt_Last_Block(Context : in out Context_T; Block : in out u8_Array) is
      Length : Integer := Block'Length * 8;
      Index : Integer := Block'First;
      Temp : Block_128_Bit := (others => 0);
      Fin_Block : Block_128_Bit;
   begin
      while Length >= Block_Size_Bits loop
         Encrypt_Next_Block(Context, Block(Index .. Index + Block_Size_Bytes - 1));
         Length := Length - Block_Size_Bits;
         Index := Index + Block_Size_Bytes;
      end loop;
      if Length > 0 then
         Inc_32(Context.ICB);
         Temp := Context.ICB;
         Cipher.Encrypt(Context.Cipher_Ctx, Temp);
         Block(Index .. Index + (Length + 7) / 8 - 1) := Block(Index .. Index + (Length + 7) / 8 - 1) xor Temp(1 .. 1 + (Length + 7) / 8 - 1);
         Context.Y := GHash(Context.H, Block(Index .. Index + (Length + 7) / 8 - 1), Seed => Context.Y);
         Context.Plaintext_Length := Context.Plaintext_Length + u64(Length);
         Put_Line("DBG: Trailing Length: " & Integer'Image(Length));
      end if;
      Store_be(Fin_Block(1 .. 8), Context.Header_Length);
      Store_be(Fin_Block(9 .. 16), Context.Plaintext_Length);
      Context.Y := GHash(Context.H, Fin_Block, Seed => Context.Y);
   end;

   procedure Decrypt_Next_Block(Context : in out Context_T; Block : in out u8_Array) is
      Temp : Block_128_Bit;
   begin
      if Block'Length /= Block_Size_Bytes then
         raise Format_Violation;
      end if;
      Context.Y := GHash(Context.H, Block, Seed => Context.Y);
      Context.Plaintext_Length := Context.Plaintext_Length + u64(Block_Size_Bits);
      Inc_32(Context.ICB);
      Temp := Context.ICB;
      Cipher.Encrypt(Context.Cipher_Ctx, Temp);
      Block := Block xor Temp;
   end;

   procedure Decrypt_Last_Block(Context : in out Context_T; Block : in out u8_Array) is
      Length : Integer := Block'Length * 8;
      Index : Integer := Block'First;
      Temp : Block_128_Bit := (others => 0);
      Fin_Block : Block_128_Bit;
   begin
      while Length >= Block_Size_Bits loop
         Decrypt_Next_Block(Context, Block(Index .. Index + Block_Size_Bytes - 1));
         Length := Length - Block_Size_Bits;
         Index := Index + Block_Size_Bytes;
      end loop;
      if Length > 0 then
         Context.Y := GHash(Context.H, Block(Index .. Index + (Length + 7) / 8 - 1), Seed => Context.Y);
         Context.Plaintext_Length := Context.Plaintext_Length + u64(Length);
         Inc_32(Context.ICB);
         Temp := Context.ICB;
         Cipher.Encrypt(Context.Cipher_Ctx, Temp);
         Block(Index .. Index + (Length + 7) / 8 - 1) := Block(Index .. Index + (Length + 7) / 8 - 1) xor Temp(1 .. 1 + (Length + 7) / 8 - 1);
      end if;
      Store_be(Fin_Block(1 .. 8), Context.Header_Length);
      Store_be(Fin_Block(9 .. 16), Context.Plaintext_Length);
      Context.Y := GHash(Context.H, Fin_Block, Seed => Context.Y);
   end;

   procedure Get_Tag(Context : in Context_T; Tag : out u8_Array) is
   begin
      Tag := Context.J0(1 .. Tag'Length) xor Context.Y(1 .. Tag'Length);
   end Get_Tag;

   function Is_Valid(Context : in Context_T; Tag : in u8_Array) return Boolean is
      Is_Tag : u8_Array(Tag'Range);
   begin
      Get_Tag(Context, Is_Tag);
      return Is_Tag = Tag;
   end;


end GCM128_Spec;
