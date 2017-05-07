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

with Ada.Text_IO; use Ada.Text_IO;
--  with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Crypto_Core_Types; use Crypto_Core_Types;
with Crypto_Types;      use Crypto_Types;

with Skinny128_128;
with Skinny128_256;
with Skinny128_384;

use Crypto_Types.Crypto_Utils_u8;

procedure Test_Skinny is
   --   package u8_IO is new Crypto_Types.u8_Sequential_IO;

   procedure Print_Hex (value : in u8) is
      hex_table : constant array (0 .. 15) of Character :=
        ('0',
         '1',
         '2',
         '3',
         '4',
         '5',
         '6',
         '7',
         '8',
         '9',
         'a',
         'b',
         'c',
         'd',
         'e',
         'f');
   begin
      Put (hex_table (Integer (Shift_Right (value, 4))));
      Put (hex_table (Integer (value and 16#F#)));
   end Print_Hex;

   procedure Print_Hex (value : in u8_Array) is
   begin
      for i in value'Range loop
         Print_Hex (value (i));
         Put (" ");
      end loop;
   end Print_Hex;

   procedure test_Skinny128_128 (Data : in u8_Array; Key : in u8_Array) is
      use Skinny128_128;
      Context : Context_T;
      Enc : Block_T;
      Dec : Block_T;
   begin
      Put("Data (" & Integer'Image(Data'Length * 8) &"): ");
      Print_Hex(Data);
      New_Line;
      Put("Key  (" & Integer'Image(Key'Length * 8) &"): ");
      Print_Hex(Key);
      New_Line;
      Context := Initialize(Key);
      Enc := Encrypt(Context, Data);
      Dec := Decrypt(Context, Enc);
      Put("Enc  (" & Integer'Image(Enc'Length * 8) &"): ");
      Print_Hex(Enc);
      New_Line;
      Put("Dec  (" & Integer'Image(Dec'Length * 8) &"): ");
      Print_Hex(Dec);
      New_Line;
   end test_Skinny128_128;

   procedure test_Skinny128_256 (Data : in u8_Array; Key : in u8_Array) is
      use Skinny128_256;
      Context : Context_T;
      Enc : Block_T;
      Dec : Block_T;
   begin
      Put("Data (" & Integer'Image(Data'Length * 8) &"): ");
      Print_Hex(Data);
      New_Line;
      Put("Key  (" & Integer'Image(Key'Length * 8) &"): ");
      Print_Hex(Key);
      New_Line;
      Context := Initialize(Key);
      Enc := Encrypt(Context, Data);
      Dec := Decrypt(Context, Enc);
      Put("Enc  (" & Integer'Image(Enc'Length * 8) &"): ");
      Print_Hex(Enc);
      New_Line;
      Put("Dec  (" & Integer'Image(Dec'Length * 8) &"): ");
      Print_Hex(Dec);
      New_Line;
   end test_Skinny128_256;

   procedure test_Skinny128_384 (Data : in u8_Array; Key : in u8_Array) is
      use Skinny128_384;
      Context : Context_T;
      Enc : Block_T;
      Dec : Block_T;
   begin
      Put("Data (" & Integer'Image(Data'Length * 8) &"): ");
      Print_Hex(Data);
      New_Line;
      Put("Key  (" & Integer'Image(Key'Length * 8) &"): ");
      Print_Hex(Key);
      New_Line;
      Context := Initialize(Key);
      Enc := Encrypt(Context, Data);
      Dec := Decrypt(Context, Enc);
      Put("Enc  (" & Integer'Image(Enc'Length * 8) &"): ");
      Print_Hex(Enc);
      New_Line;
      Put("Dec  (" & Integer'Image(Dec'Length * 8) &"): ");
      Print_Hex(Dec);
      New_Line;
   end test_Skinny128_384;


   Key_128 : constant u8_Array :=
     (16#4f#, 16#55#, 16#cf#, 16#b0#, 16#52#, 16#0c#, 16#ac#, 16#52#,
      16#fd#, 16#92#, 16#c1#, 16#5f#, 16#37#, 16#07#, 16#3e#, 16#93# );

   Plain_128 : constant u8_Array :=
     (16#f2#, 16#0a#, 16#db#, 16#0e#, 16#b0#, 16#8b#, 16#64#, 16#8a#,
      16#3b#, 16#2e#, 16#ee#, 16#d1#, 16#f0#, 16#ad#, 16#da#, 16#14# );

   Key_256 : constant u8_Array :=
     (
      16#00#, 16#9c#, 16#ec#, 16#81#, 16#60#, 16#5d#, 16#4a#, 16#c1#,
      16#d2#, 16#ae#, 16#9e#, 16#30#, 16#85#, 16#d7#, 16#a1#, 16#f3#,
      16#1a#, 16#c1#, 16#23#, 16#eb#, 16#fc#, 16#00#, 16#fd#, 16#dc#,
      16#f0#, 16#10#, 16#46#, 16#ce#, 16#ed#, 16#df#, 16#ca#, 16#b3# );

   Plain_256 : constant u8_Array :=
     ( 16#3a#, 16#0c#, 16#47#, 16#76#, 16#7a#, 16#26#, 16#a6#, 16#8d#,
       16#d3#, 16#82#, 16#a6#, 16#95#, 16#e7#, 16#02#, 16#2e#, 16#25# );

   -- 16#b7#, 16#31#, 16#d9#, 16#8a#, 16#4b#, 16#de#, 16#14#, 16#7a#,
   -- 16#7e#, 16#d4#, 16#a6#, 16#f1#, 16#6b#, 16#9b#, 16#58#, 16#7f#,

   Key_384 : constant u8_Array :=
     (16#df#, 16#88#, 16#95#, 16#48#, 16#cf#, 16#c7#, 16#ea#, 16#52#,
      16#d2#, 16#96#, 16#33#, 16#93#, 16#01#, 16#79#, 16#74#, 16#49#,
      16#ab#, 16#58#, 16#8a#, 16#34#, 16#a4#, 16#7f#, 16#1a#, 16#b2#,
      16#df#, 16#e9#, 16#c8#, 16#29#, 16#3f#, 16#be#, 16#a9#, 16#a5#,
      16#ab#, 16#1a#, 16#fa#, 16#c2#, 16#61#, 16#10#, 16#12#, 16#cd#,
      16#8c#, 16#ef#, 16#95#, 16#26#, 16#18#, 16#c3#, 16#eb#, 16#e8# );
   Plain_384 : constant u8_Array :=
     (
      16#a3#, 16#99#, 16#4b#, 16#66#, 16#ad#, 16#85#, 16#a3#, 16#45#,
      16#9f#, 16#44#, 16#e9#, 16#2b#, 16#08#, 16#f5#, 16#50#, 16#cb# );

   --16#94#, 16#ec#, 16#f5#, 16#89#, 16#e2#, 16#01#, 16#7c#, 16#60#, 16#1b#, 16#38#, 16#c6#, 16#34#, 16#6a#, 16#10#, 16#dc#, 16#fa#


begin
   New_Line;
   test_Skinny128_128(Data => Plain_128, Key => Key_128);
   New_Line;
   test_Skinny128_256(Data => Plain_256, Key => Key_256);
   New_Line;
   test_Skinny128_384(Data => Plain_384, Key => Key_384);
   New_Line;
end Test_Skinny;
