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

with System;

package body DES_Spec is

   function SBox(X : Block_48_Bit) return Block_32_Bit is
      function Map_To_Index(X : u8) return u8 is
         i, j : u8;
      begin
         i := X and 2#10_0001#;
         i := (i or Shift_Right(i, 4)) and 3;
         j := Shift_Right(X and 2#01_1110#, 1);
         return i * 16 + j;
      end;


      box : constant array (1 .. 8, 0 .. 31) of u8 :=
        (
           ( -- S-box 1
         16#E4#, 16#D1#, 16#2F#, 16#B8#, 16#3A#, 16#6C#, 16#59#, 16#07#,
         16#0F#, 16#74#, 16#E2#, 16#D1#, 16#A6#, 16#CB#, 16#95#, 16#38#,
         16#41#, 16#E8#, 16#D6#, 16#2B#, 16#FC#, 16#97#, 16#3A#, 16#50#,
         16#FC#, 16#82#, 16#49#, 16#17#, 16#5B#, 16#3E#, 16#A0#, 16#6D#
        ),
         (  -- S-box 2
          16#F1#, 16#8E#, 16#6B#, 16#34#, 16#97#, 16#2D#, 16#C0#, 16#5A#,
          16#3D#, 16#47#, 16#F2#, 16#8E#, 16#C0#, 16#1A#, 16#69#, 16#B5#,
          16#0E#, 16#7B#, 16#A4#, 16#D1#, 16#58#, 16#C6#, 16#93#, 16#2F#,
          16#D8#, 16#A1#, 16#3F#, 16#42#, 16#B6#, 16#7C#, 16#05#, 16#E9#
         ),
         (-- S-box 3
          16#A0#, 16#9E#, 16#63#, 16#F5#, 16#1D#, 16#C7#, 16#B4#, 16#28#,
          16#D7#, 16#09#, 16#34#, 16#6A#, 16#28#, 16#5E#, 16#CB#, 16#F1#,
          16#D6#, 16#49#, 16#8F#, 16#30#, 16#B1#, 16#2C#, 16#5A#, 16#E7#,
          16#1A#, 16#D0#, 16#69#, 16#87#, 16#4F#, 16#E3#, 16#B5#, 16#2C#
         ),
         (-- S-box 4
          16#7D#, 16#E3#, 16#06#, 16#9A#, 16#12#, 16#85#, 16#BC#, 16#4F#,
          16#D8#, 16#B5#, 16#6F#, 16#03#, 16#47#, 16#2C#, 16#1A#, 16#E9#,
          16#A6#, 16#90#, 16#CB#, 16#7D#, 16#F1#, 16#3E#, 16#52#, 16#84#,
          16#3F#, 16#06#, 16#A1#, 16#D8#, 16#94#, 16#5B#, 16#C7#, 16#2E#
         ),
         (-- S-box 5
          16#2C#, 16#41#, 16#7A#, 16#B6#, 16#85#, 16#3F#, 16#D0#, 16#E9#,
          16#EB#, 16#2C#, 16#47#, 16#D1#, 16#50#, 16#FA#, 16#39#, 16#86#,
          16#42#, 16#1B#, 16#AD#, 16#78#, 16#F9#, 16#C5#, 16#63#, 16#0E#,
          16#B8#, 16#C7#, 16#1E#, 16#2D#, 16#6F#, 16#09#, 16#A4#, 16#53#
         ),
         (-- S-box 6
          16#C1#, 16#AF#, 16#92#, 16#68#, 16#0D#, 16#34#, 16#E7#, 16#5B#,
          16#AF#, 16#42#, 16#7C#, 16#95#, 16#61#, 16#DE#, 16#0B#, 16#38#,
          16#9E#, 16#F5#, 16#28#, 16#C3#, 16#70#, 16#4A#, 16#1D#, 16#B6#,
          16#43#, 16#2C#, 16#95#, 16#FA#, 16#BE#, 16#17#, 16#60#, 16#8D#
         ),
         (-- S-box 7
          16#4B#, 16#2E#, 16#F0#, 16#8D#, 16#3C#, 16#97#, 16#5A#, 16#61#,
          16#D0#, 16#B7#, 16#49#, 16#1A#, 16#E3#, 16#5C#, 16#2F#, 16#86#,
          16#14#, 16#BD#, 16#C3#, 16#7E#, 16#AF#, 16#68#, 16#05#, 16#92#,
          16#6B#, 16#D8#, 16#14#, 16#A7#, 16#95#, 16#0F#, 16#E2#, 16#3C#
         ),
         (-- S-box 8
          16#D2#, 16#84#, 16#6F#, 16#B1#, 16#A9#, 16#3E#, 16#50#, 16#C7#,
          16#1F#, 16#D8#, 16#A3#, 16#74#, 16#C5#, 16#6B#, 16#0E#, 16#92#,
          16#7B#, 16#41#, 16#9C#, 16#E2#, 16#06#, 16#AD#, 16#F3#, 16#58#,
          16#21#, 16#E7#, 16#4A#, 16#8D#, 16#FC#, 16#90#, 16#35#, 16#6B#
         )
        );
      temp : u8_Array(1 .. 8);
      idx : u8;
      ret : Block_32_Bit;
   begin
      temp(1) := Shift_Right(X(1), 2);
      temp(2) := (Shift_Left(X(1), 4) or Shift_Right(X(2), 4)) and 16#3F#;
      temp(3) := (Shift_Left(X(2), 2) or Shift_Right(X(3), 6)) and 16#3F#;
      temp(4) := X(3) and 16#3F#;
      temp(5) := Shift_Right(X(4), 2);
      temp(6) := (Shift_Left(X(4), 4) or Shift_Right(X(5), 4)) and 16#3F#;
      temp(7) := (Shift_Left(X(5), 2) or Shift_Right(X(6), 6)) and 16#3F#;
      temp(8) := X(6) and 16#3F#;
      for i in temp'Range loop
         idx := Map_To_Index(temp(i));
         temp(i) := box(i, Integer(idx / 2));
         if idx mod 2 = 0 then
            temp(i) := Shift_Right(temp(i), 4);
         else
            temp(i) := temp(i) and 16#0F#;
         end if;
      end loop;
      for i in 0..3 loop
         ret(i + 1) := Shift_Left(temp(2 * i + 1), 4) or temp(2 * i + 2);
      end loop;
      return ret;
   end SBox;


   subtype Bit_Index_T is Integer range 1 .. 64;
   type Bit_Index_Table_T is array (Bit_Index_T range <>) of Bit_Index_T;

   type Permutation_Table_T(Out_Bits : Bit_Index_T) is record
      In_Bits : Bit_Index_T;
      Table :  Bit_Index_Table_T(1 .. Out_Bits);
   end record;

   E_Table : constant Permutation_Table_T :=
     ( In_Bits => 32,
       Out_Bits => 48,
       Table =>
         (
          32,  1,  2,  3,  4,  5,
           4,  5,  6,  7,  8,  9,
           8,  9, 10, 11, 12, 13,
          12, 13, 14, 15, 16, 17,
          16, 17, 18, 19, 20, 21,
          20, 21, 22, 23, 24, 25,
          24, 25, 26, 27, 28, 29,
          28, 29, 30, 31, 32,  1
         )
      );

   P_Table : constant Permutation_Table_T :=
     ( In_Bits => 32,
       Out_Bits => 32,
       Table =>
         (
          16,  7, 20, 21,
          29, 12, 28, 17,
           1, 15, 23, 26,
           5, 18, 31, 10,
           2,  8, 24, 14,
          32, 27,  3,  9,
          19, 13, 30,  6,
          22, 11,  4, 25
         )
      );

   IP_Table : constant Permutation_Table_T :=
     ( In_Bits => 64,
       Out_Bits => 64,
       Table =>
         (
          58, 50, 42, 34, 26, 18, 10, 2,
          60, 52, 44, 36, 28, 20, 12, 4,
          62, 54, 46, 38, 30, 22, 14, 6,
          64, 56, 48, 40, 32, 24, 16, 8,
          57, 49, 41, 33, 25, 17,  9, 1,
          59, 51, 43, 35, 27, 19, 11, 3,
          61, 53, 45, 37, 29, 21, 13, 5,
          63, 55, 47, 39, 31, 23, 15, 7
         )
      );

   FP_Table : constant Permutation_Table_T :=
     ( In_Bits => 64,
       Out_Bits => 64,
       Table =>
         (
          40, 8, 48, 16, 56, 24, 64, 32,
          39, 7, 47, 15, 55, 23, 63, 31,
          38, 6, 46, 14, 54, 22, 62, 30,
          37, 5, 45, 13, 53, 21, 61, 29,
          36, 4, 44, 12, 52, 20, 60, 28,
          35, 3, 43, 11, 51, 19, 59, 27,
          34, 2, 42, 10, 50, 18, 58, 26,
          33, 1, 41,  9, 49, 17, 57, 25
         )
      );

   PC1_Table : constant Permutation_Table_T :=
     ( In_Bits => 64,
       Out_Bits => 56,
       Table =>
         (
          57, 49, 41, 33, 25, 17,  9,
           1, 58, 50, 42, 34, 26, 18,
          10,  2, 59, 51, 43, 35, 27,
          19, 11,  3, 60, 52, 44, 36,
          63, 55, 47, 39, 31, 23, 15,
           7, 62, 54, 46, 38, 30, 22,
          14,  6, 61, 53, 45, 37, 29,
          21, 13,  5, 28, 20, 12,  4
         )
      );

   PC2_Table : constant Permutation_Table_T :=
     ( In_Bits => 56,
       Out_Bits => 48,
       Table =>
         (
          14, 17, 11, 24,  1,  5,
           3, 28, 15,  6, 21, 10,
          23, 19, 12,  4, 26,  8,
          16,  7, 27, 20, 13,  2,
          41, 52, 31, 37, 47, 55,
          30, 40, 51, 45, 33, 48,
          44, 49, 39, 56, 34, 53,
          46, 42, 50, 36, 29, 32
         )
      );

   Round_Shifts : constant array(1 .. 16) of u8 :=
     ( 1, 1, 2, 2, 2, 2, 2, 2,
       1, 2, 2, 2, 2, 2, 2, 1 );

   procedure Permute(Out_Buffer : out u8_Array; In_Buffer : in u8_Array; Table : in Permutation_Table_T) is
      use system;
   begin
      if Out_Buffer'Length /= Table.Out_Bits / 8 then
         raise Constraint_Error;
      end if;
      if In_Buffer'Length /= Table.In_Bits / 8 then
         raise Constraint_Error;
      end if;
      for i in Table.Table'Range loop
         Bit_Set(Out_Buffer, i - 1, Bit_Get(In_Buffer, Table.Table(i) - 1, High_Order_First), High_Order_First);
      end loop;
   end Permute;

   function F(X : Block_32_Bit; Round_Key : Block_48_Bit) return Block_32_Bit is
      temp_48 : Block_48_Bit;
      temp_32, ret : Block_32_Bit;
   begin
      Permute(temp_48, X, E_Table);
      temp_48 := temp_48 xor Round_Key;
      temp_32 := SBox(temp_48);
      Permute(ret, temp_32, P_Table);
      return ret;
   end F;

   procedure Initialize(Context : out Context_T; Key : in Key_T) is
   begin
      Permute(Context.Key, u8_Array(Key), PC1_Table);
   end;

   procedure RoundKey_Step(Key : in out Block_56_Bit; Steps : in Positive; Backwards : Boolean := False) is
      C, D : u32;
   begin
      C := Shift_Left(
                      Shift_Left(
                        Shift_Left(
                          u32(Key(1)), 8)
                        or u32(Key(2)), 8)
                      or u32(Key(3)), 4)
        or u32(Shift_Right(Key(4), 4));
      D := Shift_Left(
                      Shift_Left(
                        Shift_Left(
                          u32(Key(4) and 16#0F#), 8)
                        or u32(Key(5)), 8)
                      or u32(Key(6)), 8)
        or u32(Key(7));
      if Backwards = False then
         C := Shift_Left(C, Steps);
         D := Shift_Left(D, Steps);
         C := ( C or Shift_Right(C, 28)) and 16#0FFF_FFFF#;
         D := ( D or Shift_Right(D, 28)) and 16#0FFF_FFFF#;
      else
         C := Rotate_Right(C, Steps);
         D := Rotate_Right(D, Steps);
         C :=  C or (Shift_Right((C and 16#C0000000#), 4) and not 16#F000_0000#);
         D :=  D or (Shift_Right((D and 16#C0000000#), 4) and not 16#F000_0000#);
      end if;
      Key(1) := u8(Shift_Right(C, 20) and 16#FF#);
      Key(2) := u8(Shift_Right(C, 12) and 16#FF#);
      Key(3) := u8(Shift_Right(C,  4) and 16#FF#);
      Key(4) := u8(Shift_Left(C,  4) and 16#F0#) or u8(Shift_Right(D,  24) and 16#0F#);
      Key(5) := u8(Shift_Right(D, 16) and 16#FF#);
      Key(6) := u8(Shift_Right(D,  8) and 16#FF#);
      Key(7) := u8(D and 16#FF#);
   end;


   procedure Encrypt(Context : in Context_T; Block: in out Block_64_Bit) is
      Key : Block_56_Bit := Context.Key;
      Round_Key : Block_48_Bit;
      Data : Block_64_Bit;
   begin
      Permute(Data, Block, IP_Table);
      for Round in 0 .. 7 loop
         RoundKey_Step(Key, Integer(Round_Shifts(1 + 2 * Round)));
         Permute(Round_Key, Key, PC2_Table);
         Data(1 .. 4) := Data(1 .. 4) xor F(Data(5 .. 8), Round_Key);
         RoundKey_Step(Key, Integer(Round_Shifts(2 + 2 * Round)));
         Permute(Round_Key, Key, PC2_Table);
         Data(5 .. 8) := Data(5 .. 8) xor F(Data(1 .. 4), Round_Key);
      end loop;
      Swap(Data(1 .. 4), Data(5 .. 8));
      Permute(Block, Data, FP_Table);
   end;


   procedure Decrypt(Context : in Context_T; Block: in out Block_64_Bit) is
      Key : Block_56_Bit := Context.Key;
      Round_Key : Block_48_Bit;
      Data : Block_64_Bit;
   begin
      Permute(Data, Block, IP_Table);
      for Round in reverse 0 .. 7 loop
         Permute(Round_Key, Key, PC2_Table);
         Data(1 .. 4) := Data(1 .. 4) xor F(Data(5 .. 8), Round_Key);
         RoundKey_Step(Key, Integer(Round_Shifts(2 + 2 * Round)), Backwards => True);
         Permute(Round_Key, Key, PC2_Table);
         Data(5 .. 8) := Data(5 .. 8) xor F(Data(1 .. 4), Round_Key);
         RoundKey_Step(Key, Integer(Round_Shifts(1 + 2 * Round)), Backwards => True);
      end loop;
      Swap(Data(1 .. 4), Data(5 .. 8));
      Permute(Block, Data, FP_Table);
   end;


end DES_Spec;
