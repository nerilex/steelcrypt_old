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

package body SHA2_Large is

   K : constant u64_Array(1 .. 80) :=
     (
      16#428a2f98d728ae22#, 16#7137449123ef65cd#, 16#b5c0fbcfec4d3b2f#, 16#e9b5dba58189dbbc#,
      16#3956c25bf348b538#, 16#59f111f1b605d019#, 16#923f82a4af194f9b#, 16#ab1c5ed5da6d8118#,
      16#d807aa98a3030242#, 16#12835b0145706fbe#, 16#243185be4ee4b28c#, 16#550c7dc3d5ffb4e2#,
      16#72be5d74f27b896f#, 16#80deb1fe3b1696b1#, 16#9bdc06a725c71235#, 16#c19bf174cf692694#,
      16#e49b69c19ef14ad2#, 16#efbe4786384f25e3#, 16#0fc19dc68b8cd5b5#, 16#240ca1cc77ac9c65#,
      16#2de92c6f592b0275#, 16#4a7484aa6ea6e483#, 16#5cb0a9dcbd41fbd4#, 16#76f988da831153b5#,
      16#983e5152ee66dfab#, 16#a831c66d2db43210#, 16#b00327c898fb213f#, 16#bf597fc7beef0ee4#,
      16#c6e00bf33da88fc2#, 16#d5a79147930aa725#, 16#06ca6351e003826f#, 16#142929670a0e6e70#,
      16#27b70a8546d22ffc#, 16#2e1b21385c26c926#, 16#4d2c6dfc5ac42aed#, 16#53380d139d95b3df#,
      16#650a73548baf63de#, 16#766a0abb3c77b2a8#, 16#81c2c92e47edaee6#, 16#92722c851482353b#,
      16#a2bfe8a14cf10364#, 16#a81a664bbc423001#, 16#c24b8b70d0f89791#, 16#c76c51a30654be30#,
      16#d192e819d6ef5218#, 16#d69906245565a910#, 16#f40e35855771202a#, 16#106aa07032bbd1b8#,
      16#19a4c116b8d2d0c8#, 16#1e376c085141ab53#, 16#2748774cdf8eeb99#, 16#34b0bcb5e19b48a8#,
      16#391c0cb3c5c95a63#, 16#4ed8aa4ae3418acb#, 16#5b9cca4f7763e373#, 16#682e6ff3d6b2b8a3#,
      16#748f82ee5defb2fc#, 16#78a5636f43172f60#, 16#84c87814a1f0ab72#, 16#8cc702081a6439ec#,
      16#90befffa23631e28#, 16#a4506cebde82bde9#, 16#bef9a3f7b2c67915#, 16#c67178f2e372532b#,
      16#ca273eceea26619c#, 16#d186b8c721c0c207#, 16#eada7dd6cde0eb1e#, 16#f57d4f7fee6ed178#,
      16#06f067aa72176fba#, 16#0a637dc5a2c898a6#, 16#113f9804bef90dae#, 16#1b710b35131c471b#,
      16#28db77f523047d84#, 16#32caab7b40c72493#, 16#3c9ebe0a15c9bebc#, 16#431d67c49c100d4c#,
      16#4cc5d4becb3e42b6#, 16#597f299cfc657e2a#, 16#5fcb6fab3ad6faec#, 16#6c44198c4a475817#
      );

   function Ch(x, y, z : u64) return u64 is
   begin
      return (x and y) xor ((not x) and z);
   end Ch;

   function Maj(x, y, z : u64) return u64 is
   begin
      return (x and y) xor (x and z) xor (y and z);
   end Maj;

   function Sigma_A0(x : u64) return u64 is
   begin
      return Rotate_Right(x, 28) xor Rotate_Right(x, 34) xor Rotate_Right(x, 39);
   end Sigma_A0;

   function Sigma_A1(x : u64) return u64 is
   begin
      return Rotate_Right(x, 14) xor Rotate_Right(x, 18) xor Rotate_Right(x, 41);
   end Sigma_A1;

   function Sigma_B0(x : u64) return u64 is
   begin
      return Rotate_Right(x, 1) xor Rotate_Right(x, 8) xor Shift_Right(x, 7);
   end Sigma_B0;

   function Sigma_B1(x : u64) return u64 is
   begin
      return Rotate_Right(x, 19) xor Rotate_Right(x, 61) xor Shift_Right(x, 6);
   end Sigma_B1;

   procedure Next_Block(Context : in out Context_T; Block : in Block_1024_Bit) is
      W : u64_Array(1 .. 16) := Load_BE(Block);
      T1, T2, Wx : u64;
      A : u64_Array(1 .. 8) := Context.H;
   begin
      for i in 1 .. 80 loop
         if i > 16 then
            Wx := Sigma_B1(W(15)) + W(10) + Sigma_B0(W(2)) + W(1);
            W(1 .. 15) := W(2 .. 16);
            W(16) := Wx;
         else
            Wx := W(i);
         end if;
         T1 := A(8) + Sigma_A1(A(5)) + Ch(A(5), A(6), A(7)) + K(i) + Wx;
         T2 := Sigma_A0(A(1)) + Maj(A(1), A(2), A(3));
         A(2 .. 8) := A(1 .. 7);
         A(5) := A(5) + T1;
         A(1) := T1 + T2;
      end loop;
      Context.H := Context.H + A;
      Context.Counter := Context.Counter + 1;
   end Next_Block;

   procedure Last_Block(Context : in out Context_T; Block : in u8_Array; Bits : in Integer := -1) is
      c : u64 := u64(Context.Counter * 1024);
      s : Integer := Bits;
      b : Block_1024_Bit := (others => 0);
      i : Integer := Block'First;
      r : Natural;
   begin
      if s < 0 or else s > Block'Length * 8 then
         s := Block'Length * 8;
      end if;
      c := u64(Integer(c) + s);
      while s >= 1024 loop
         Next_Block(Context, Block(i .. i + 127));
         i := i + 128;
         s := s - 1024;
      end loop;
      r := (s + 7) / 8;
      b(1 .. r) := Block(i .. i + r - 1);
      b(1 + s / 8) := b(1 + s / 8) or Shift_Right(u8(16#80#), s mod 8);
      s := s + 1;
      if s > 1024 - 128 then
         Next_Block(Context, b);
         b := (others => 0);
      end if;
      Store_BE(b(121 .. 128), c);
      Next_Block(Context, b);
   end Last_Block;


end SHA2_Large;

