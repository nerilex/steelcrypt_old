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
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Crypto_Core_Types; use Crypto_Core_Types;
with Crypto_Types; use Crypto_Types;

with Spritz_Stream;
with Spritz_Hash;

with AES;

use Crypto_Types.Crypto_Types_u8;

procedure main is
--   package u8_IO is new Crypto_Types.u8_Sequential_IO;

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

   procedure test_spritz_stream(s : in String) is
      ctx : Spritz_Stream.Context;
      z : u8_Array(1 .. 8) := (others => 0);
   begin
      Spritz_Stream.Initialize(ctx, key => s);
      Put(s);
      for i in 0 .. 6 - s'Length loop
         Put(" ");
      end loop;
      Put(": ");
      Spritz_Stream.Encrypt(ctx, z);
      print_hex(z);
      New_Line;
   end test_spritz_stream;

   procedure test_spritz_hash(s : in String) is
      ctx : Spritz_Hash.Context;
      hash : u8_Array(1 .. 32);
   begin
      Spritz_Hash.Initialize(ctx);
      Spritz_Hash.Add_Data(ctx, s);
      Spritz_Hash.Extract_Hash(ctx => ctx, Hash => hash);
      Put(s);
      for i in 0 .. 6 - s'Length loop
         Put(" ");
      end loop;
      Put(": ");
      print_hex(hash(1 .. 8));
      New_Line;
   end test_spritz_hash;

   procedure test_aes_schedule(key : u8_Array) is
      Ctx : AES.Context_128;
   begin
      Ctx := AES.Initialize(AES.Key_128(key));
      for i in Ctx.RoundKeys'Range loop
         Put("RoundKey ");
         Put(i);
         Put(": ");
         print_hex(u8_Array(Ctx.RoundKeys(i)));
         New_Line;
      end loop;
      New_Line;
   end test_aes_schedule;

   procedure test_aes_schedule192(key : u8_Array) is
      Ctx : AES.Context_192;
   begin
      Ctx := AES.Initialize(AES.Key_192(key));
      for i in Ctx.RoundKeys'Range loop
         Put("RoundKey ");
         Put(i);
         Put(": ");
         print_hex(u8_Array(Ctx.RoundKeys(i)));
         New_Line;
      end loop;
      New_Line;
   end test_aes_schedule192;

   procedure test_aes_schedule256(key : u8_Array) is
      Ctx : AES.Context_256;
   begin
      Ctx := AES.Initialize(AES.Key_256(key));
      for i in Ctx.RoundKeys'Range loop
         Put("RoundKey ");
         Put(i);
         Put(": ");
         print_hex(u8_Array(Ctx.RoundKeys(i)));
         New_Line;
      end loop;
      New_Line;
   end test_aes_schedule256;


--   Random_File : File_Type;
begin
   test_spritz_stream("ABC");
   test_spritz_stream("spam");
   test_spritz_stream("arcfour");
   New_Line;

   test_spritz_hash("ABC");
   test_spritz_hash("spam");
   test_spritz_hash("arcfour");
   New_Line;

   test_aes_schedule(u8_Array'(
                     16#2b#, 16#7e#, 16#15#, 16#16#,
                     16#28#, 16#ae#, 16#d2#, 16#a6#,
                     16#ab#, 16#f7#, 16#15#, 16#88#,
                     16#09#, 16#cf#, 16#4f#, 16#3c# ));

   test_aes_schedule192(u8_Array'(
                        16#8e#, 16#73#, 16#b0#, 16#f7#,
                        16#da#, 16#0e#, 16#64#, 16#52#,
                        16#c8#, 16#10#, 16#f3#, 16#2b#,
                        16#80#, 16#90#, 16#79#, 16#e5#,
                        16#62#, 16#f8#, 16#ea#, 16#d2#,
                        16#52#, 16#2c#, 16#6b#, 16#7b# ));

   test_aes_schedule256(u8_Array'(
                        16#60#, 16#3d#, 16#eb#, 16#10#,
                        16#15#, 16#ca#, 16#71#, 16#be#,
                        16#2b#, 16#73#, 16#ae#, 16#f0#,
                        16#85#, 16#7d#, 16#77#, 16#81#,
                        16#1f#, 16#35#, 16#2c#, 16#07#,
                        16#3b#, 16#61#, 16#08#, 16#d7#,
                        16#2d#, 16#98#, 16#10#, 16#a3#,
                        16#09#, 16#14#, 16#df#, 16#f4# ));

end main;
