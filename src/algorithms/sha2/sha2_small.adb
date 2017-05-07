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

with Crypto_Types; use Crypto_Types;
use Crypto_Types.Crypto_Utils_u32;
use Crypto_Types.Crypto_Utils_u64;

package body SHA2_Small is

   K : constant u32_Array (1 .. 64) :=
     (16#428a2f98#,
      16#71374491#,
      16#b5c0fbcf#,
      16#e9b5dba5#,
      16#3956c25b#,
      16#59f111f1#,
      16#923f82a4#,
      16#ab1c5ed5#,
      16#d807aa98#,
      16#12835b01#,
      16#243185be#,
      16#550c7dc3#,
      16#72be5d74#,
      16#80deb1fe#,
      16#9bdc06a7#,
      16#c19bf174#,
      16#e49b69c1#,
      16#efbe4786#,
      16#0fc19dc6#,
      16#240ca1cc#,
      16#2de92c6f#,
      16#4a7484aa#,
      16#5cb0a9dc#,
      16#76f988da#,
      16#983e5152#,
      16#a831c66d#,
      16#b00327c8#,
      16#bf597fc7#,
      16#c6e00bf3#,
      16#d5a79147#,
      16#06ca6351#,
      16#14292967#,
      16#27b70a85#,
      16#2e1b2138#,
      16#4d2c6dfc#,
      16#53380d13#,
      16#650a7354#,
      16#766a0abb#,
      16#81c2c92e#,
      16#92722c85#,
      16#a2bfe8a1#,
      16#a81a664b#,
      16#c24b8b70#,
      16#c76c51a3#,
      16#d192e819#,
      16#d6990624#,
      16#f40e3585#,
      16#106aa070#,
      16#19a4c116#,
      16#1e376c08#,
      16#2748774c#,
      16#34b0bcb5#,
      16#391c0cb3#,
      16#4ed8aa4a#,
      16#5b9cca4f#,
      16#682e6ff3#,
      16#748f82ee#,
      16#78a5636f#,
      16#84c87814#,
      16#8cc70208#,
      16#90befffa#,
      16#a4506ceb#,
      16#bef9a3f7#,
      16#c67178f2#);

   function Ch (x, y, z : u32) return u32 is
   begin
      return (x and y) xor ((not x) and z);
   end Ch;

   function Maj (x, y, z : u32) return u32 is
   begin
      return (x and y) xor (x and z) xor (y and z);
   end Maj;

   function Sigma_A0 (x : u32) return u32 is
   begin
      return Rotate_Right (x, 2) xor
        Rotate_Right (x, 13) xor
        Rotate_Right (x, 22);
   end Sigma_A0;

   function Sigma_A1 (x : u32) return u32 is
   begin
      return Rotate_Right (x, 6) xor
        Rotate_Right (x, 11) xor
        Rotate_Right (x, 25);
   end Sigma_A1;

   function Sigma_B0 (x : u32) return u32 is
   begin
      return Rotate_Right (x, 7) xor
        Rotate_Right (x, 18) xor
        Shift_Right (x, 3);
   end Sigma_B0;

   function Sigma_B1 (x : u32) return u32 is
   begin
      return Rotate_Right (x, 17) xor
        Rotate_Right (x, 19) xor
        Shift_Right (x, 10);
   end Sigma_B1;

   procedure Next_Block
     (Context : in out Context_T;
      Block   : in     Block_512_Bit)
   is
      W          : u32_Array (1 .. 16) := Load_be (Block);
      T1, T2, Wx : u32;
      A          : u32_Array (1 .. 8)  := Context.H;
   begin
      for i in 1 .. 64 loop
         if i > 16 then
            Wx := Sigma_B1 (W (15)) + W (10) + Sigma_B0 (W (2)) + W (1);
            W (1 .. 15) := W (2 .. 16);
            W (16)      := Wx;
         else
            Wx := W (i);
         end if;
         T1 :=
           A (8) + Sigma_A1 (A (5)) + Ch (A (5), A (6), A (7)) + K (i) + Wx;
         T2         := Sigma_A0 (A (1)) + Maj (A (1), A (2), A (3));
         A (2 .. 8) := A (1 .. 7);
         A (5)      := A (5) + T1;
         A (1)      := T1 + T2;
      end loop;
      Context.H       := Context.H + A;
      Context.counter := Context.counter + 1;
   end Next_Block;

   procedure Last_Block
     (Context : in out Context_T;
      Block   : in     u8_Array;
      Bits    : in     Integer := -1)
   is
      c : u64           := u64 (Context.counter * 512);
      s : Integer       := Bits;
      b : Block_512_Bit := (others => 0);
      i : Integer       := Block'First;
      r : Natural;
   begin
      if s < 0 or else s > Block'Length * 8 then
         s := Block'Length * 8;
      end if;
      c := u64 (Integer (c) + s);
      while s >= 512 loop
         Next_Block (Context, Block (i .. i + 63));
         i := i + 64;
         s := s - 512;
      end loop;
      r             := (s + 7) / 8;
      b (1 .. r)    := Block (i .. i + r - 1);
      b (1 + s / 8) := b (1 + s / 8) or Shift_Right (u8 (16#80#), s mod 8);
      s             := s + 1;
      if s > 512 - 64 then
         Next_Block (Context, b);
         b := (others => 0);
      end if;
      Store_be (b (57 .. 64), c);
      Next_Block (Context, b);
   end Last_Block;

end SHA2_Small;
