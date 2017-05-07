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

with Ada.Text_IO;       use Ada.Text_IO;
with Crypto_Core_Types; use Crypto_Core_Types;
with Crypto_Types;      use Crypto_Types;

with Sha_Test_IO;
with SHA2_256;

use Crypto_Types.Crypto_Utils_u8;

procedure Test_SHA256 is

--     procedure test_sha256(Data : in u8_Array; Bits : in Integer := -1) is
--        Digest : Block_256_Bit;
--        q : Integer := Bits;
--     begin
--        if q < 0 then
--           q := Data'Length * 8;
--        end if;
--        Print_Hex(Data);
--        Put(" (" & Integer'Image(q) & "): ");
--        Sha2_256.Hash(Data, Digest, Bits);
--        Print_Hex(Digest);
--        New_Line;
--     end test_sha256;

--     procedure test_sha256(Msg : in String) is
--        Data : u8_Array(1 .. Msg'Length);
--     begin
--        Put("""" & Msg & """: ");
--        for i in data'Range loop
--           Data(i) := u8(Character'Pos(Msg(Msg'First + i - Data'First)));
--        end loop;
--        test_sha256(Data);
--        New_Line;
--     end test_sha256;

   procedure test_sha256_with_File is new Sha_Test_IO.Test_With_File
     (Digest_Size_Bits => SHA2_256.Digest_Size_Bits,
      Hash             => SHA2_256.Hash);

begin
   Put_Line
     ("SHA2_256.Context_T'Size: " &
      Integer'Image (SHA2_256.Context_T'Size / 8));
--     test_sha256("");
--     a(1) := 16#80#;
--     test_sha256(a(1..1), 2);
--     test_sha256("abc");
--     test_sha256("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
   New_Line;
   test_sha256_with_File ("testvectors/sha2/bit/SHA256ShortMsg.rsp");
   test_sha256_with_File ("testvectors/sha2/bit/SHA256LongMsg.rsp");
   test_sha256_with_File ("testvectors/sha2/bit/SHA256Monte.rsp");
   test_sha256_with_File ("testvectors/sha2/byte/SHA256ShortMsg.rsp");
   test_sha256_with_File ("testvectors/sha2/byte/SHA256LongMsg.rsp");
   test_sha256_with_File ("testvectors/sha2/byte/SHA256Monte.rsp");
end Test_SHA256;
