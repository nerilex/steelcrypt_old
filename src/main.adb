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

with Spritz_Stream;
with Spritz_Hash;

with AES;

with ARIA;

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

   procedure test_aes is
      key : AES.Key_256;
      block : Block_128_bit;
      ctx128 : AES.Context_128;
      ctx192 : AES.Context_192;
      ctx256 : AES.Context_256;
   begin
      for i in key'Range loop
         key(i) := u8(i - 1);
      end loop;
      block(block'First) := 0;
      for i in block'First + 1 .. block'Last loop
         block(i) := u8(block(i - 1) + 16#11#);
      end loop;
      AES.Initialize(ctx128, AES.Key_128(key(1 .. 16)));
      AES.Encrypt(ctx128, block);
      print_hex(block); New_Line;
      AES.Decrypt(ctx128, block);
      print_hex(block); New_Line;

      AES.Initialize(ctx192, AES.Key_192(key(1 .. 24)));
      AES.Encrypt(ctx192, block);
      print_hex(block); New_Line;
      AES.Decrypt(ctx192, block);
      print_hex(block); New_Line;

      AES.Initialize(ctx256, key);
      AES.Encrypt(ctx256, block);
      print_hex(block); New_Line;
      AES.Decrypt(ctx256, block);
      print_hex(block); New_Line;
   end test_aes;

--
--     procedure test_aes_128b is
--        key : constant AES.Key_128 := (
--                                       16#2b#, 16#7e#, 16#15#, 16#16#,
--                                       16#28#, 16#ae#, 16#d2#, 16#a6#,
--                                       16#ab#, 16#f7#, 16#15#, 16#88#,
--                                       16#09#, 16#cf#, 16#4f#, 16#3c# );
--        block : Block_128_bit := (
--                                  16#32#, 16#43#, 16#f6#, 16#a8#,
--                                  16#88#, 16#5a#, 16#30#, 16#8d#,
--                                  16#31#, 16#31#, 16#98#, 16#a2#,
--                                  16#e0#, 16#37#, 16#07#, 16#34# );
--        ctx : AES.Context_128;
--     begin
--        AES.Initialize(key, ctx);
--        AES.Encrypt(ctx, block);
--        print_hex(block);
--     end test_aes_128b;

   procedure test_aria is
      Context : Aria.Context_T;
      key : Aria.Key_256;
      Block : Block_128_Bit := (others => 0);
   begin
      for i in Key'Range loop
         Key(i) := u8(i - 1);
      end loop;

      for i in 2 .. Block'Last loop
         Block(i) := Block(i - 1) + 16#11#;
      end loop;
      Aria.Initialize(Key => ARIA.Key_128(key(1 .. 16)), Context => Context);
      Put("Plaintext:  "); print_hex(Block); New_Line;
      Aria.Encrypt(Block => Block, Context => Context);
      Put("Ciphertext: "); print_hex(Block); New_Line;
      Aria.Decrypt(Block => Block, Context => Context);
      Put("Plaintext:  "); print_hex(Block); New_Line;
      New_Line;

      Block(1) := 0;
      for i in 2 .. Block'Last loop
         Block(i) := Block(i - 1) + 16#11#;
      end loop;
      Aria.Initialize(Key => ARIA.Key_192(key(1 .. 24)), Context => Context);
      Put("Plaintext:  "); print_hex(Block); New_Line;
      Aria.Encrypt(Block => Block, Context => Context);
      Put("Ciphertext: "); print_hex(Block); New_Line;
      Aria.Decrypt(Block => Block, Context => Context);
      Put("Plaintext:  "); print_hex(Block); New_Line;
      New_Line;

      Block(1) := 0;
      for i in 2 .. Block'Last loop
         Block(i) := Block(i - 1) + 16#11#;
      end loop;
      Aria.Initialize(Key => key, Context => Context);
      Put("Plaintext:  "); print_hex(Block); New_Line;
      Aria.Encrypt(Block => Block, Context => Context);
      Put("Ciphertext: "); print_hex(Block); New_Line;
      Aria.Decrypt(Block => Block, Context => Context);
      Put("Plaintext:  "); print_hex(Block); New_Line;
      New_Line;
   end;

   procedure test_sha256(Msg : String) is
      Data : u8_Array(1 .. Msg'Length);
      Digest : Block_256_Bit;
   begin
      Put("""" & Msg & """: ");
      for i in data'Range loop
         Data(i) := u8(Character'Pos(Msg(Msg'First + i - Data'First)));
      end loop;
      Sha2_256.Hash(Data, Digest);
      Print_Hex(Digest);
      New_Line;
   end test_sha256;


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

   Put_Line("AES.Context_128'Size: " & Integer'Image(AES.Context_128'Size / 8));
   Put_Line("AES.Context_192'Size: " & Integer'Image(AES.Context_192'Size / 8));
   Put_Line("AES.Context_256'Size: " & Integer'Image(AES.Context_256'Size / 8));
   test_aes;
   New_Line;

   Put_Line("ARIA.Context_T'Size: " & Integer'Image(ARIA.Context_T'Size / 8));
   test_aria;
   New_Line;

   Put_Line("SHA2_256.Context_T'Size: " & Integer'Image(SHA2_256.Context_T'Size / 8));
   test_sha256("abc");
   test_sha256("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
   New_Line;
end main;
