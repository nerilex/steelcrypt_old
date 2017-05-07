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

with Spritz_Stream;
with Spritz_Hash;

with AES; use AES;

with ARIA; use ARIA;

with SHA2_256;

with GCM128_Spec;

procedure main is
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

   procedure test_spritz_stream (s : in String) is
      ctx : Spritz_Stream.Context;
      z   : u8_Array (1 .. 8) := (others => 0);
   begin
      Spritz_Stream.Initialize (ctx, key => s);
      Put (s);
      for i in 0 .. 6 - s'Length loop
         Put (" ");
      end loop;
      Put (": ");
      Spritz_Stream.Encrypt (ctx, z);
      Print_Hex (z);
      New_Line;
   end test_spritz_stream;

   procedure test_spritz_hash (s : in String) is
      ctx  : Spritz_Hash.Context;
      hash : u8_Array (1 .. 32);
   begin
      Spritz_Hash.Initialize (ctx);
      Spritz_Hash.Add_Data (ctx, s);
      Spritz_Hash.Extract_Hash (ctx => ctx, Hash => hash);
      Put (s);
      for i in 0 .. 6 - s'Length loop
         Put (" ");
      end loop;
      Put (": ");
      Print_Hex (hash (1 .. 8));
      New_Line;
   end test_spritz_hash;

   procedure test_aes is
      key    : Block_256_Bit;
      block  : Block_128_Bit;
      ctx128 : AES_128.Context_T;
      ctx192 : AES_192.Context_T;
      ctx256 : AES_256.Context_T;
   begin
      for i in key'Range loop
         key (i) := u8 (i - 1);
      end loop;
      block (block'First) := 0;
      for i in block'First + 1 .. block'Last loop
         block (i) := u8 (block (i - 1) + 16#11#);
      end loop;
      AES_128.Initialize (ctx128, key (1 .. 16));
      AES_128.Encrypt (ctx128, block);
      Print_Hex (block);
      New_Line;
      AES_128.Decrypt (ctx128, block);
      Print_Hex (block);
      New_Line;

      AES_192.Initialize (ctx192, key (1 .. 24));
      AES_192.Encrypt (ctx192, block);
      Print_Hex (block);
      New_Line;
      AES_192.Decrypt (ctx192, block);
      Print_Hex (block);
      New_Line;

      AES_256.Initialize (ctx256, key);
      AES_256.Encrypt (ctx256, block);
      Print_Hex (block);
      New_Line;
      AES_256.Decrypt (ctx256, block);
      Print_Hex (block);
      New_Line;
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
      Context : ARIA_256.Context_T;
      key     : Block_256_Bit;
      Block   : Block_128_Bit := (others => 0);
   begin
      for i in key'Range loop
         key (i) := u8 (i - 1);
      end loop;

      for i in 2 .. Block'Last loop
         Block (i) := Block (i - 1) + 16#11#;
      end loop;
      ARIA_128.Initialize (Key => key (1 .. 16), Context => Context);
      Put ("Plaintext:  ");
      Print_Hex (Block);
      New_Line;
      ARIA_128.Encrypt (Block => Block, Context => Context);
      Put ("Ciphertext: ");
      Print_Hex (Block);
      New_Line;
      ARIA_128.Decrypt (Block => Block, Context => Context);
      Put ("Plaintext:  ");
      Print_Hex (Block);
      New_Line;
      New_Line;

      Block (1) := 0;
      for i in 2 .. Block'Last loop
         Block (i) := Block (i - 1) + 16#11#;
      end loop;
      ARIA_192.Initialize (Key => key (1 .. 24), Context => Context);
      Put ("Plaintext:  ");
      Print_Hex (Block);
      New_Line;
      ARIA_192.Encrypt (Block => Block, Context => Context);
      Put ("Ciphertext: ");
      Print_Hex (Block);
      New_Line;
      ARIA_192.Decrypt (Block => Block, Context => Context);
      Put ("Plaintext:  ");
      Print_Hex (Block);
      New_Line;
      New_Line;

      Block (1) := 0;
      for i in 2 .. Block'Last loop
         Block (i) := Block (i - 1) + 16#11#;
      end loop;
      ARIA_256.Initialize (Key => key, Context => Context);
      Put ("Plaintext:  ");
      Print_Hex (Block);
      New_Line;
      ARIA_256.Encrypt (Block => Block, Context => Context);
      Put ("Ciphertext: ");
      Print_Hex (Block);
      New_Line;
      ARIA_256.Decrypt (Block => Block, Context => Context);
      Put ("Plaintext:  ");
      Print_Hex (Block);
      New_Line;
      New_Line;
   end test_aria;

   procedure test_sha256 (Msg : String) is
      Data   : u8_Array (1 .. Msg'Length);
      Digest : Block_256_Bit;
   begin
      Put ("""" & Msg & """: ");
      for i in Data'Range loop
         Data (i) := u8 (Character'Pos (Msg (Msg'First + i - Data'First)));
      end loop;
      SHA2_256.Hash (Data, Digest);
      Print_Hex (Digest);
      New_Line;
   end test_sha256;

   procedure test_gcm
     (pKey    : String;
      pIV     : String;
      pHeader : String;
      pMsg    : String)
   is
      Key    : constant Block_128_Bit := From_Hex (pKey);
      IV     : constant u8_Array      := From_Hex (pIV);
      Header : constant u8_Array      := From_Hex (pHeader);
      Msg    : u8_Array               := From_Hex (pMsg);
      package gcm is new GCM128_Spec (AES_128);
      ctx : gcm.Context_T;
      Tag : Block_128_Bit;
   begin
      Put_Line ("Key: " & To_Hex (Key));
      Put_Line ("IV:  " & To_Hex (IV));
      Put_Line ("Header: " & To_Hex (Header));
      Put_Line ("Plaintext: " & To_Hex (Msg));
      gcm.Initialize (ctx, Key, IV);
      gcm.Header_Last_Block (ctx, Header);
      gcm.Encrypt_Last_Block (ctx, Msg);
      Put_Line ("Ciphertext: " & To_Hex (Msg));
      gcm.Get_Tag (ctx, Tag);
      Put_Line ("Tag (A): " & To_Hex (Tag));
      gcm.Initialize (ctx, Key, IV);
      gcm.Header_Last_Block (ctx, Header);
      gcm.Decrypt_Last_Block (ctx, Msg);
      Put_Line ("Plaintext: " & To_Hex (Msg));
      gcm.Get_Tag (ctx, Tag);
      Put_Line ("Tag (B): " & To_Hex (Tag));
      New_Line;
   end test_gcm;

--   Random_File : File_Type;
begin
   test_spritz_stream ("ABC");
   test_spritz_stream ("spam");
   test_spritz_stream ("arcfour");
   New_Line;

   test_spritz_hash ("ABC");
   test_spritz_hash ("spam");
   test_spritz_hash ("arcfour");
   New_Line;

   Put_Line
     ("AES_128.Context_T'Size / 8: " &
      Integer'Image (AES_128.Context_T'Size / 8));
   Put_Line
     ("AES_192.Context_T'Size / 8: " &
      Integer'Image (AES_192.Context_T'Size / 8));
   Put_Line
     ("AES_256.Context_T'Size / 8: " &
      Integer'Image (AES_256.Context_T'Size / 8));
   test_aes;
   New_Line;

   Put_Line
     ("ARIA_128.Context_T'Size / 8: " &
      Integer'Image (ARIA_128.Context_T'Size / 8));
   test_aria;
   New_Line;

   Put_Line
     ("SHA2_256.Context_T'Size: " &
      Integer'Image (SHA2_256.Context_T'Size / 8));
   test_sha256 ("abc");
   test_sha256 ("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
   New_Line;

   test_gcm
     ("11754cd72aec309bf52f7687212e8957",
      "3c819d9a9bed087615030b65",
      "",
      "");
   --Count = 0
--     Key = da0b615656135194ba6d3c851099bc48
--       IV = d39d4b4d3cc927885090e6c3
--         PT =
--           AAD = e7e5e6f8dac913036cb2ff29e8625e0e
--             CT =
--               Tag = ab967711a5770461724460b07237e2
   test_gcm
     (pKey    => "da0b615656135194ba6d3c851099bc48",
      pIV     => "d39d4b4d3cc927885090e6c3",
      pHeader => "e7e5e6f8dac913036cb2ff29e8625e0e",
      pMsg    => "");

--     Count = 0
--       Key = 9bf406339fcef9675bbcf156aa1a0661
--         IV = 8be4a9543d40f542abacac95
--           PT =
--             AAD = 7167cbf56971793186333a6685bbd58d47d379b3
--               CT =
--                 Tag = 5e7968d7bbd5ba58cfcc750e2ef8f1
   test_gcm
     (pKey    => "9bf406339fcef9675bbcf156aa1a0661",
      pIV     => "8be4a9543d40f542abacac95",
      pHeader => "7167cbf56971793186333a6685bbd58d47d379b3",
      pMsg    => "");

--     Count = 0
--       Key = 7fddb57453c241d03efbed3ac44e371c
--         IV = ee283a3fc75575e33efd4887
--           PT = d5de42b461646c255c87bd2962d3b9a2
--             AAD =
--               CT = 2ccda4a5415cb91e135c2a0f78c9b2fd
--                 Tag = b36d1df9b9d5e596f83e8b7f52971cb3
   test_gcm
     (pKey    => "7fddb57453c241d03efbed3ac44e371c",
      pIV     => "ee283a3fc75575e33efd4887",
      pHeader => "",
      pMsg    => "d5de42b461646c255c87bd2962d3b9a2");

--     Count = 14
--       Key = 0e00c76561d2bd9b40c3c15427e2b08f
--         IV = 492cadaccd3ca3fbc9cf9f06eb3325c4e159850b0dbe98199b89b7af528806610b6f63998e1eae80c348e74cbb921d8326631631fc6a5d304f39166daf7ea15fa1977f101819adb510b50fe9932e12c5a85aa3fd1e73d8d760af218be829903a77c63359d75edd91b4f6ed5465a72662f5055999e059e7654a8edc921aa0d496
--           PT = fef03c2d7fb15bf0d2df18007d99f967c878ad59359034f7bb2c19af120685d78e32f6b8b83b032019956ca9c0195721476b85
--             AAD = d8f1163d8c840292a2b2dacf4ac7c36aff8733f18fabb4fa5594544125e03d1e6e5d6d0fd61656c8d8f327c92839ae5539bb469c9257f109ebff85aad7bd220fdaa95c022dbd0c7bb2d878ad504122c943045d3c5eba8f1f56c0
--               CT = 4f6cf471be7cbd2575cd5a1747aea8fe9dea83e51936beac3e68f66206922060c697ffa7af80ad6bb68f2cf4fc97416ee52abe
--                 Tag = e20b6655
--
   test_gcm
     (pKey => "0e00c76561d2bd9b40c3c15427e2b08f",
      pIV  =>
        "492cadaccd3ca3fbc9cf9f06eb3325c4e159850b0dbe98199b89b7af528806610b6f63998e1eae80c348e74cbb921d8326631631fc6a5d304f39166daf7ea15fa1977f101819adb510b50fe9932e12c5a85aa3fd1e73d8d760af218be829903a77c63359d75edd91b4f6ed5465a72662f5055999e059e7654a8edc921aa0d496",
      pHeader =>
        "d8f1163d8c840292a2b2dacf4ac7c36aff8733f18fabb4fa5594544125e03d1e6e5d6d0fd61656c8d8f327c92839ae5539bb469c9257f109ebff85aad7bd220fdaa95c022dbd0c7bb2d878ad504122c943045d3c5eba8f1f56c0",
      pMsg =>
        "fef03c2d7fb15bf0d2df18007d99f967c878ad59359034f7bb2c19af120685d78e32f6b8b83b032019956ca9c0195721476b85");
   New_Line;
end main;
