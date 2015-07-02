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
--  with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Crypto_Core_Types; use Crypto_Core_Types;
with Crypto_Types; use Crypto_Types;

with SHA2_256;

use Crypto_Types.Crypto_Types_u8;

procedure main is
   --   package u8_IO is new Crypto_Types.u8_Sequential_IO;

   procedure Print_Hex(value : in u8) is
      hex_table : constant array (0 .. 15) of Character :=
        ( '0', '1', '2', '3',
          '4', '5', '6', '7',
          '8', '9', 'a', 'b',
          'c', 'd', 'e', 'f');
   begin
      Put(hex_table(Integer(Shift_Right(value, 4))));
      Put(hex_table(Integer(value and 16#F#)));
   end;

   procedure Print_Hex(value : in u8_Array) is
   begin
      for i in value'Range loop
         print_hex(value(i));
         Put(" ");
      end loop;
   end;


   procedure test_sha256(Data : in u8_Array; Bits : in Integer := -1) is
      Digest : Block_256_Bit;
      q : Integer := Bits;
   begin
      if q < 0 then
         q := Data'Length * 8;
      end if;
      Print_Hex(Data);
      Put(" (" & Integer'Image(q) & "): ");
      Sha2_256.Hash(Data, Digest, Bits);
      Print_Hex(Digest);
      New_Line;
   end test_sha256;

   procedure test_sha256(Msg : in String) is
      Data : u8_Array(1 .. Msg'Length);
   begin
      Put("""" & Msg & """: ");
      for i in data'Range loop
         Data(i) := u8(Character'Pos(Msg(Msg'First + i - Data'First)));
      end loop;
      test_sha256(Data);
      New_Line;
   end test_sha256;

   a : u8_Array(1 .. 12);
begin
   Put_Line("SHA2_256.Context_T'Size: " & Integer'Image(SHA2_256.Context_T'Size / 8));
   test_sha256("");
   a(1) := 16#80#;
   test_sha256(a(1..1), 2);
   test_sha256("abc");
   test_sha256("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
   New_Line;
end main;
