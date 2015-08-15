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

package body AES is

   Sbox : constant array (u8 range 0 .. 255) of u8 := (
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

   InvSbox : constant array (u8 range 0 .. 255) of u8 := (
      16#52#, 16#09#, 16#6a#, 16#d5#, 16#30#, 16#36#, 16#a5#, 16#38#, 16#bf#, 16#40#, 16#a3#, 16#9e#, 16#81#, 16#f3#, 16#d7#, 16#fb#,
      16#7c#, 16#e3#, 16#39#, 16#82#, 16#9b#, 16#2f#, 16#ff#, 16#87#, 16#34#, 16#8e#, 16#43#, 16#44#, 16#c4#, 16#de#, 16#e9#, 16#cb#,
      16#54#, 16#7b#, 16#94#, 16#32#, 16#a6#, 16#c2#, 16#23#, 16#3d#, 16#ee#, 16#4c#, 16#95#, 16#0b#, 16#42#, 16#fa#, 16#c3#, 16#4e#,
      16#08#, 16#2e#, 16#a1#, 16#66#, 16#28#, 16#d9#, 16#24#, 16#b2#, 16#76#, 16#5b#, 16#a2#, 16#49#, 16#6d#, 16#8b#, 16#d1#, 16#25#,
      16#72#, 16#f8#, 16#f6#, 16#64#, 16#86#, 16#68#, 16#98#, 16#16#, 16#d4#, 16#a4#, 16#5c#, 16#cc#, 16#5d#, 16#65#, 16#b6#, 16#92#,
      16#6c#, 16#70#, 16#48#, 16#50#, 16#fd#, 16#ed#, 16#b9#, 16#da#, 16#5e#, 16#15#, 16#46#, 16#57#, 16#a7#, 16#8d#, 16#9d#, 16#84#,
      16#90#, 16#d8#, 16#ab#, 16#00#, 16#8c#, 16#bc#, 16#d3#, 16#0a#, 16#f7#, 16#e4#, 16#58#, 16#05#, 16#b8#, 16#b3#, 16#45#, 16#06#,
      16#d0#, 16#2c#, 16#1e#, 16#8f#, 16#ca#, 16#3f#, 16#0f#, 16#02#, 16#c1#, 16#af#, 16#bd#, 16#03#, 16#01#, 16#13#, 16#8a#, 16#6b#,
      16#3a#, 16#91#, 16#11#, 16#41#, 16#4f#, 16#67#, 16#dc#, 16#ea#, 16#97#, 16#f2#, 16#cf#, 16#ce#, 16#f0#, 16#b4#, 16#e6#, 16#73#,
      16#96#, 16#ac#, 16#74#, 16#22#, 16#e7#, 16#ad#, 16#35#, 16#85#, 16#e2#, 16#f9#, 16#37#, 16#e8#, 16#1c#, 16#75#, 16#df#, 16#6e#,
      16#47#, 16#f1#, 16#1a#, 16#71#, 16#1d#, 16#29#, 16#c5#, 16#89#, 16#6f#, 16#b7#, 16#62#, 16#0e#, 16#aa#, 16#18#, 16#be#, 16#1b#,
      16#fc#, 16#56#, 16#3e#, 16#4b#, 16#c6#, 16#d2#, 16#79#, 16#20#, 16#9a#, 16#db#, 16#c0#, 16#fe#, 16#78#, 16#cd#, 16#5a#, 16#f4#,
      16#1f#, 16#dd#, 16#a8#, 16#33#, 16#88#, 16#07#, 16#c7#, 16#31#, 16#b1#, 16#12#, 16#10#, 16#59#, 16#27#, 16#80#, 16#ec#, 16#5f#,
      16#60#, 16#51#, 16#7f#, 16#a9#, 16#19#, 16#b5#, 16#4a#, 16#0d#, 16#2d#, 16#e5#, 16#7a#, 16#9f#, 16#93#, 16#c9#, 16#9c#, 16#ef#,
      16#a0#, 16#e0#, 16#3b#, 16#4d#, 16#ae#, 16#2a#, 16#f5#, 16#b0#, 16#c8#, 16#eb#, 16#bb#, 16#3c#, 16#83#, 16#53#, 16#99#, 16#61#,
      16#17#, 16#2b#, 16#04#, 16#7e#, 16#ba#, 16#77#, 16#d6#, 16#26#, 16#e1#, 16#69#, 16#14#, 16#63#, 16#55#, 16#21#, 16#0c#, 16#7d#
   );

   rcon : constant u8_Array(1 .. 10) := (
      16#01#, 16#02#, 16#04#, 16#08#, 16#10#, 16#20#, 16#40#, 16#80#, 16#1b#, 16#36#
   );

   function gf256mul(a, b : u8) return u8 is
      r : u8 := 0;
      t1 : u8 := a;
      t2 : u8 := b;
      f : boolean;
   begin
      for i in 1 .. 8 loop
         if (t1 and 1) = 1 then
            r := r xor t2;
         end if;
         t1 := Shift_Right(t1, 1);
         f := (t2 and 16#80#) = 16#80#;
         t2 := Shift_Left(t2, 1);
         if f then
            t2 := t2 xor polynom;
         end if;
      end loop;
      return r;
   end gf256mul;

   function RotWord(A : u8_Array) return u8_Array is
   begin
      return Rotate_be(A, 8);
   end RotWord;

   procedure SubArray(A : in out u8_Array) is
   begin
      for i in A'Range loop
         A(i) := Sbox(A(i));
      end loop;
   end SubArray;

   procedure InvSubArray(A : in out u8_Array) is
   begin
      for i in A'Range loop
         A(i) := InvSbox(A(i));
      end loop;
   end InvSubArray;

   procedure Initialize_Generic(Key : in T_In; Context : out T_Out) is
      w : u8_Array(1 .. Context.Roundkeys'Length * RoundKey_T'Length);
      temp : u8_Array(1 .. 4);
      Nk : constant Integer := Key'Length / 4;
      k : Integer range rcon'First .. rcon'Last + 1 := 1;
      j : Integer range w'First .. w'Last + 1 := 1;
   begin
      w(1 .. Key'Length) := u8_Array(Key);
      for i in 1 + Key'Length / 4 .. Context.RoundKeys'Length * RoundKey_T'Length / 4 loop
         temp := w((i - 1) * 4 - 3 .. (i - 1) * 4);
         if (i - 1) mod Nk = 0 then
            temp := RotWord(temp);
            SubArray(temp);
            temp(1) := temp(1) xor rcon(k);
            k := k  + 1;
         else if Nk > 6 and then (i - 1) mod Nk = 4 then
               SubArray(temp);
            end if;
         end if;
               temp := temp xor w((i - Nk) * 4 - 3 .. (i - Nk) * 4);
         w(i * 4 - 3 .. i * 4) := temp;
      end loop;
      for i in Context.RoundKeys'Range loop
         Context.RoundKeys(i) := RoundKey_T(w(j .. j + RoundKey_T'Length - 1));
         j := j + RoundKey_T'Length;
      end loop;
   end Initialize_Generic;

   procedure Initialize_priv is new Initialize_Generic(T_In => Key_128, T_Out => Context_128);
   procedure Initialize_priv is new Initialize_Generic(T_In => Key_192, T_Out => Context_192);
   procedure Initialize_priv is new Initialize_Generic(T_In => Key_256, T_Out => Context_256);

   procedure Initialize(Context : out Context_128; Key : in Key_128) is
   begin
      Initialize_priv(Key, Context);
   end Initialize;

   procedure Initialize(Context : out Context_192; Key : in Key_192) is
   begin
      Initialize_priv(Key, Context);
   end Initialize;

   procedure Initialize(Context : out Context_256; Key : in Key_256) is
   begin
      Initialize_priv(Key, Context);
   end Initialize;

   procedure ShiftColumns(Block : in out Block_128_Bit) is
      temp : u8;
   begin
      temp      := Block( 2);
      Block( 2) := Block( 6);
      Block( 6) := Block(10);
      Block(10) := Block(14);
      Block(14) := temp;
      Swap(Block(3), Block(11));
      Swap(Block(7), Block(15));
      temp      := Block(16);
      Block(16) := Block(12);
      Block(12) := Block( 8);
      Block( 8) := Block( 4);
      Block( 4) := temp;
   end ShiftColumns;

   procedure InvShiftColumns(Block : in out Block_128_Bit) is
      temp : u8;
   begin
      temp      := Block(14);
      Block(14) := Block(10);
      Block(10) := Block( 6);
      Block( 6) := Block( 2);
      Block( 2) := temp;
      Swap(Block(3), Block(11));
      Swap(Block(7), Block(15));
      temp      := Block( 4);
      Block( 4) := Block( 8);
      Block( 8) := Block(12);
      Block(12) := Block(16);
      Block(16) := temp;
   end InvShiftColumns;

   procedure MixRows(Block : in out Block_128_Bit) is
      temp : u8_Array(1 .. 4);
   begin
      for i in 0 .. 3 loop
         temp := Block(i * 4 + 1 .. i * 4 + 4);
--           Put("  (" & Integer'Image(i) & ") t: "); print_hex(temp); New_Line;
         Block(i * 4 + 1) := gf256mul(2, temp(1)) xor gf256mul(3, temp(2)) xor temp(3) xor temp(4);
         Block(i * 4 + 2) := gf256mul(2, temp(2)) xor gf256mul(3, temp(3)) xor temp(4) xor temp(1);
         Block(i * 4 + 3) := gf256mul(2, temp(3)) xor gf256mul(3, temp(4)) xor temp(1) xor temp(2);
         Block(i * 4 + 4) := gf256mul(2, temp(4)) xor gf256mul(3, temp(1)) xor temp(2) xor temp(3);
--           Put("  (" & Integer'Image(i) & ") r: "); print_hex(Block(i * 4 + 1 .. i * 4 + 4)); New_Line;
      end loop;
   end MixRows;

   procedure InvMixRows(Block : in out Block_128_Bit) is
      temp : u8_Array(1 .. 4);
   begin
      for i in 0 .. 3 loop
         temp := Block(i * 4 + 1 .. i * 4 + 4);
         --           Put("  (" & Integer'Image(i) & ") t: "); print_hex(temp); New_Line;
         Block(i * 4 + 1) := gf256mul(14, temp(1)) xor gf256mul(11, temp(2)) xor gf256mul(13, temp(3)) xor gf256mul( 9, temp(4));
         Block(i * 4 + 2) := gf256mul( 9, temp(1)) xor gf256mul(14, temp(2)) xor gf256mul(11, temp(3)) xor gf256mul(13, temp(4));
         Block(i * 4 + 3) := gf256mul(13, temp(1)) xor gf256mul( 9, temp(2)) xor gf256mul(14, temp(3)) xor gf256mul(11, temp(4));
         Block(i * 4 + 4) := gf256mul(11, temp(1)) xor gf256mul(13, temp(2)) xor gf256mul( 9, temp(3)) xor gf256mul(14, temp(4));
         --           Put("  (" & Integer'Image(i) & ") r: "); print_hex(Block(i * 4 + 1 .. i * 4 + 4)); New_Line;
      end loop;
   end InvMixRows;


   procedure Encrypt_Generic(Context : in Context_T; Block : in out Block_128_Bit) is
   begin
--        Put("( 1) pre-add:   "); print_hex(Block); New_Line;
      Block := u8_Array(Block) xor u8_Array(Context.RoundKeys(Context.RoundKeys'First));
      for i in Context.RoundKeys'First + 1 .. Context.RoundKeys'Last - 1 loop
--           Put("(" & Integer'Image(i) & ") pre-sub:   "); print_hex(Block); New_Line;
         SubArray(u8_Array(Block));
--           Put("(" & Integer'Image(i) & ") pre-shift: "); print_hex(Block); New_Line;
         ShiftColumns(Block);
--           Put("(" & Integer'Image(i) & ") pre-mix:   "); print_hex(Block); New_Line;
         MixRows(Block);
--           Put("(" & Integer'Image(i) & ") pre-add:   "); print_hex(Block); New_Line;
         Block := u8_Array(Block) xor u8_Array(Context.RoundKeys(i));
      end loop;
      SubArray(u8_Array(Block));
      ShiftColumns(Block);
      Block := u8_Array(Block) xor u8_Array(Context.RoundKeys(Context.RoundKeys'Last));
   end Encrypt_Generic;

   procedure Encrypt(Context : in Context_128; Block: in out Block_128_Bit) is
   begin
      Encrypt_Generic(Context_T(Context), Block);
   end Encrypt;

   procedure Encrypt(Context : in Context_192; Block: in out Block_128_Bit) is
   begin
      Encrypt_Generic(Context_T(Context), Block);
   end Encrypt;

   procedure Encrypt(Context : in Context_256; Block: in out Block_128_Bit) is
   begin
      Encrypt_Generic(Context_T(Context), Block);
   end Encrypt;


   procedure Decrypt_Generic(Context : in Context_T; Block : in out Block_128_Bit) is
   begin
      --        Put("( 1) pre-add:   "); print_hex(Block); New_Line;
      Block := u8_Array(Block) xor u8_Array(Context.RoundKeys(Context.RoundKeys'Last));
      for i in reverse Context.RoundKeys'First + 1 .. Context.RoundKeys'Last - 1 loop
         --           Put("(" & Integer'Image(i) & ") pre-shift: "); print_hex(Block); New_Line;
         InvShiftColumns(Block);
         --           Put("(" & Integer'Image(i) & ") pre-sub:   "); print_hex(Block); New_Line;
         InvSubArray(u8_Array(Block));
         --           Put("(" & Integer'Image(i) & ") pre-add:   "); print_hex(Block); New_Line;
         Block := u8_Array(Block) xor u8_Array(Context.RoundKeys(i));
         --           Put("(" & Integer'Image(i) & ") pre-mix:   "); print_hex(Block); New_Line;
         InvMixRows(Block);
      end loop;
      InvSubArray(u8_Array(Block));
      InvShiftColumns(Block);
      Block := u8_Array(Block) xor u8_Array(Context.RoundKeys(Context.RoundKeys'First));
   end Decrypt_Generic;

   procedure Decrypt(Context : in Context_128; Block: in out Block_128_Bit) is
   begin
      Decrypt_Generic(Context_T(Context), Block);
   end Decrypt;

   procedure Decrypt(Context : in Context_192; Block: in out Block_128_Bit) is
   begin
      Decrypt_Generic(Context_T(Context), Block);
   end Decrypt;

   procedure Decrypt(Context : in Context_256; Block: in out Block_128_Bit) is
   begin
      Decrypt_Generic(Context_T(Context), Block);
   end Decrypt;
--     function Encrypt(Ctx : Context_128; Source : Plaintext) return Ciphertext;
--     function Decrypt(Ctx : Context_128; Source : Ciphertext) return Plaintext;
--
--     function Encrypt(Ctx : Context_192; Source : Plaintext) return Ciphertext;
--     function Decrypt(Ctx : Context_192; Source : Ciphertext) return Plaintext;
--
--     function Encrypt(Ctx : Context_256; Source : Plaintext) return Ciphertext;
--     function Decrypt(Ctx : Context_256; Source : Ciphertext) return Plaintext;

end AES;
