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


with Ada.Text_IO; use Ada.Text_IO;
-- with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body AES is

   sbox : constant u8_Array(0 .. 255) := (
      16#63#, 16#7c#, 16#77#, 16#7b#, 16#f2#, 16#6b#, 16#6f#, 16#c5#, 16#30#, 16#01#, 16#67#, 16#2b#, 16#fe#, 16#d7#, 16#ab#, 16#76#,
      16#ca#, 16#82#, 16#c9#, 16#7d#, 16#fa#, 16#59#, 16#47#, 16#f0#, 16#ad#, 16#d4#, 16#a2#, 16#af#, 16#9c#, 16#a4#, 16#72#, 16#c0#,
      16#b7#, 16#fd#, 16#93#, 16#26#, 16#36#, 16#3f#, 16#f7#, 16#cc#, 16#34#, 16#a5#, 16#e5#, 16#f1#, 16#71#, 16#d8#, 16#31#, 16#15#,
      16#04#, 16#c7#, 16#23#, 16#c3#, 16#18#, 16#96#, 16#05#, 16#9a#, 16#07#, 16#12#, 16#80#, 16#e2#, 16#eb#, 16#27#, 16#b2#, 16#75#,
      16#09#, 16#83#, 16#2c#, 16#1a#, 16#1b#, 16#6e#, 16#5a#, 16#a0#, 16#52#, 16#3b#, 16#d6#, 16#b3#, 16#29#, 16#e3#, 16#2f#, 16#84#,
      16#53#, 16#d1#, 16#00#, 16#ed#, 16#20#, 16#fc#, 16#b1#, 16#5b#, 16#6a#, 16#cb#, 16#be#, 16#39#, 16#4a#, 16#4c#, 16#58#, 16#cf#,
      16#d0#, 16#ef#, 16#aa#, 16#fb#, 16#43#, 16#4d#, 16#33#, 16#85#, 16#45#, 16#f9#, 16#02#, 16#7f#, 16#50#, 16#3c#, 16#9f#, 16#a8#,
      16#51#, 16#a3#, 16#40#, 16#8f#, 16#92#, 16#9d#, 16#38#, 16#f5#, 16#bc#, 16#b6#, 16#da#, 16#21#, 16#10#, 16#ff#, 16#f3#, 16#d2#,
      16#cd#, 16#0c#, 16#13#, 16#ec#, 16#5f#, 16#97#, 16#44#, 16#17#, 16#c4#, 16#a7#, 16#7e#, 16#3d#, 16#64#, 16#5d#, 16#19#, 16#73#,
      16#60#, 16#81#, 16#4f#, 16#dc#, 16#22#, 16#2a#, 16#90#, 16#88#, 16#46#, 16#ee#, 16#b8#, 16#14#, 16#de#, 16#5e#, 16#0b#, 16#db#,
      16#e0#, 16#32#, 16#3a#, 16#0a#, 16#49#, 16#06#, 16#24#, 16#5c#, 16#c2#, 16#d3#, 16#ac#, 16#62#, 16#91#, 16#95#, 16#e4#, 16#79#,
      16#e7#, 16#c8#, 16#37#, 16#6d#, 16#8d#, 16#d5#, 16#4e#, 16#a9#, 16#6c#, 16#56#, 16#f4#, 16#ea#, 16#65#, 16#7a#, 16#ae#, 16#08#,
      16#ba#, 16#78#, 16#25#, 16#2e#, 16#1c#, 16#a6#, 16#b4#, 16#c6#, 16#e8#, 16#dd#, 16#74#, 16#1f#, 16#4b#, 16#bd#, 16#8b#, 16#8a#,
      16#70#, 16#3e#, 16#b5#, 16#66#, 16#48#, 16#03#, 16#f6#, 16#0e#, 16#61#, 16#35#, 16#57#, 16#b9#, 16#86#, 16#c1#, 16#1d#, 16#9e#,
      16#e1#, 16#f8#, 16#98#, 16#11#, 16#69#, 16#d9#, 16#8e#, 16#94#, 16#9b#, 16#1e#, 16#87#, 16#e9#, 16#ce#, 16#55#, 16#28#, 16#df#,
      16#8c#, 16#a1#, 16#89#, 16#0d#, 16#bf#, 16#e6#, 16#42#, 16#68#, 16#41#, 16#99#, 16#2d#, 16#0f#, 16#b0#, 16#54#, 16#bb#, 16#16#
   );


   rcon : constant u8_Array(1 .. 10) := (
      16#01#, 16#02#, 16#04#, 16#08#, 16#10#, 16#20#, 16#40#, 16#80#, 16#1b#, 16#36#
   );

   procedure print_hex(value : in u8) is
      hex_table : constant array (0 .. 15) of Character :=
        ( '0', '1', '2', '3',
          '4', '5', '6', '7',
          '8', '9', 'a', 'b',
         'c', 'd', 'e', 'f');
   begin
      Put(hex_table(Integer(Shift_Right(value, 4))));
      Put(hex_table(Integer(value and 16#F#)));
   end;

   procedure print_hex(value : in u8_Array) is
   begin
      for i in value'Range loop
         print_hex(value(i));
         Put(" ");
      end loop;
   end;

   function gf256mul(a, b : u8) return u8 is
      r : u8 := 0;
      t1 : u8 := a;
      t2 : u8 := b;
   begin
      for i in 1 .. 8 loop
         if (t1 and 1) = 1 then
            r := r xor t2;
         end if;
         t1 := Shift_Right(t1, 1);
         if (t2 and 16#80#) = 16#80# then
            t2 := t2 xor polynom;
         end if;
         t2 := Shift_Left(t2, 1);
      end loop;
      return r;
   end gf256mul;

   function RotWord(A : u8_Array) return u8_Array is
   begin
      return Rotate_be(A, 8);
   end;

   function SubArray(A : u8_Array) return u8_Array is
      r : u8_Array(A'Range);
   begin
      for i in A'Range loop
         r(i) := sbox(Natural(A(i)));
      end loop;
      return r;
   end;

   function Initialize(Key : Key_128) return Context_128 is
      Ret : Context_128;
      w : u8_Array(1 .. Ret.Roundkeys'Length * RoundKey_T'Length);
      temp : u8_Array(1 .. 4);
      Nk : Constant := Key'Length / 4;
      k : Integer range rcon'First .. rcon'Last + 1 := 1;
      j : Integer range w'First .. w'Last + 1 := 1;
   begin
      New_Line;
      Put("== Debug ==");
      New_Line;

      w(1 .. Key'Length) := u8_Array(Key);
      for i in 1 + Key'Length / 4 .. Ret.RoundKeys'Length * RoundKey_T'Length / 4 loop
         temp := w((i - 1) * 4 - 3 .. (i - 1) * 4);
         print_hex(temp);
         if (i - 1) mod Nk = 0 then
            temp := SubArray(RotWord(temp));
            Put(" - ");
            print_hex(temp);
            temp(1) := temp(1) xor rcon(k);
            k := k  + 1;
            Put(" - ");
            print_hex(temp);
         end if;
         temp := temp xor w((i - Nk) * 4 - 3 .. (i - Nk) * 4);
         Put(" - ");
         print_hex(temp);
         w(i * 4 - 3 .. i * 4) := temp;
         New_Line;
      end loop;
      for i in Ret.RoundKeys'Range loop
         Ret.RoundKeys(i) := RoundKey_T(w(j .. j + RoundKey_T'Length - 1));
         j := j + RoundKey_T'Length;
      end loop;
      return Ret;
   end Initialize;

   function Initialize(Key : Key_192) return Context_192 is
      Ret : Context_192;
      w : u8_Array(1 .. Ret.Roundkeys'Length * RoundKey_T'Length);
      temp : u8_Array(1 .. 4);
      Nk : Constant := Key'Length / 4;
      k : Integer range rcon'First .. rcon'Last + 1 := 1;
      j : Integer range w'First .. w'Last + 1 := 1;
   begin
      New_Line;
      Put("== Debug ==");
      New_Line;

      w(1 .. Key'Length) := u8_Array(Key);
      for i in 1 + Key'Length / 4 .. Ret.RoundKeys'Length * RoundKey_T'Length / 4 loop
         temp := w((i - 1) * 4 - 3 .. (i - 1) * 4);
         print_hex(temp);
         if (i - 1) mod Nk = 0 then
            temp := SubArray(RotWord(temp));
            Put(" - ");
            print_hex(temp);
            temp(1) := temp(1) xor rcon(k);
            k := k  + 1;
            Put(" - ");
            print_hex(temp);
         end if;
         temp := temp xor w((i - Nk) * 4 - 3 .. (i - Nk) * 4);
         Put(" - ");
         print_hex(temp);
         w(i * 4 - 3 .. i * 4) := temp;
         New_Line;
      end loop;
      for i in Ret.RoundKeys'Range loop
         Ret.RoundKeys(i) := RoundKey_T(w(j .. j + RoundKey_T'Length - 1));
         j := j + RoundKey_T'Length;
      end loop;
      return Ret;
   end Initialize;

   function Initialize(Key : Key_256) return Context_256 is
      Ret : Context_256;
      w : u8_Array(1 .. Ret.Roundkeys'Length * RoundKey_T'Length);
      temp : u8_Array(1 .. 4);
      Nk : Constant := Key'Length / 4;
      k : Integer range rcon'First .. rcon'Last + 1 := 1;
      j : Integer range w'First .. w'Last + 1 := 1;
   begin
      New_Line;
      Put("== Debug ==");
      New_Line;

      w(1 .. Key'Length) := u8_Array(Key);
      for i in 1 + Key'Length / 4 .. Ret.RoundKeys'Length * RoundKey_T'Length / 4 loop
         temp := w((i - 1) * 4 - 3 .. (i - 1) * 4);
         print_hex(temp);
         if (i - 1) mod Nk = 0 then
            temp := SubArray(RotWord(temp));
            Put(" - ");
            print_hex(temp);
            temp(1) := temp(1) xor rcon(k);
            k := k  + 1;
            Put(" - ");
            print_hex(temp);
         else if Nk > 6 and then (i - 1) mod Nk = 4 then
               temp := SubArray(temp);
            end if;
         end if;
         temp := temp xor w((i - Nk) * 4 - 3 .. (i - Nk) * 4);
         Put(" - ");
         print_hex(temp);
         w(i * 4 - 3 .. i * 4) := temp;
         New_Line;
      end loop;
      for i in Ret.RoundKeys'Range loop
         Ret.RoundKeys(i) := RoundKey_T(w(j .. j + RoundKey_T'Length - 1));
         j := j + RoundKey_T'Length;
      end loop;
      return Ret;
   end Initialize;

--     function Encrypt(Ctx : Context_128; Source : Plaintext) return Ciphertext;
--     function Decrypt(Ctx : Context_128; Source : Ciphertext) return Plaintext;
--
--     function Initialize(Key : Key_192) return Context_192;
--     function Encrypt(Ctx : Context_192; Source : Plaintext) return Ciphertext;
--     function Decrypt(Ctx : Context_192; Source : Ciphertext) return Plaintext;
--
--     function Initialize(Key : Key_256) return Context_256;
--     function Encrypt(Ctx : Context_256; Source : Plaintext) return Ciphertext;
--     function Decrypt(Ctx : Context_256; Source : Ciphertext) return Plaintext;

end AES;
