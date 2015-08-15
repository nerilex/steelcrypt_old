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
with Crypto_Core_Types; use Crypto_Core_Types;
with Crypto_Types; use Crypto_Types;

with Sha_Test_IO;

with SHA2_224;
with SHA2_256;
with SHA2_384;
with SHA2_512;

use Crypto_Types.Crypto_Types_u8;

procedure Test_SHA2 is

   procedure test_sha224_with_File is new Sha_Test_IO.Test_With_File(DigestSize_Bits => SHA2_224.DigestSize_Bits, Hash => SHA2_224.Hash);
   procedure test_sha256_with_File is new Sha_Test_IO.Test_With_File(DigestSize_Bits => SHA2_256.DigestSize_Bits, Hash => SHA2_256.Hash);
   procedure test_sha384_with_File is new Sha_Test_IO.Test_With_File(DigestSize_Bits => SHA2_384.DigestSize_Bits, Hash => SHA2_384.Hash);
   procedure test_sha512_with_File is new Sha_Test_IO.Test_With_File(DigestSize_Bits => SHA2_512.DigestSize_Bits, Hash => SHA2_512.Hash);

begin
   New_Line;
   test_sha224_with_File("testvectors/sha2/bit/SHA224ShortMsg.rsp");
   test_sha224_with_File("testvectors/sha2/bit/SHA224LongMsg.rsp");
   test_sha224_with_File("testvectors/sha2/bit/SHA224Monte.rsp");
   test_sha224_with_File("testvectors/sha2/byte/SHA224ShortMsg.rsp");
   test_sha224_with_File("testvectors/sha2/byte/SHA224LongMsg.rsp");
   test_sha224_with_File("testvectors/sha2/byte/SHA224Monte.rsp");

   New_Line;
   test_sha256_with_File("testvectors/sha2/bit/SHA256ShortMsg.rsp");
   test_sha256_with_File("testvectors/sha2/bit/SHA256LongMsg.rsp");
   test_sha256_with_File("testvectors/sha2/bit/SHA256Monte.rsp");
   test_sha256_with_File("testvectors/sha2/byte/SHA256ShortMsg.rsp");
   test_sha256_with_File("testvectors/sha2/byte/SHA256LongMsg.rsp");
   test_sha256_with_File("testvectors/sha2/byte/SHA256Monte.rsp");

   New_Line;
   test_sha384_with_File("testvectors/sha2/bit/SHA384ShortMsg.rsp");
   test_sha384_with_File("testvectors/sha2/bit/SHA384LongMsg.rsp");
   test_sha384_with_File("testvectors/sha2/bit/SHA384Monte.rsp");
   test_sha384_with_File("testvectors/sha2/byte/SHA384ShortMsg.rsp");
   test_sha384_with_File("testvectors/sha2/byte/SHA384LongMsg.rsp");
   test_sha384_with_File("testvectors/sha2/byte/SHA384Monte.rsp");

   New_Line;
   test_sha512_with_File("testvectors/sha2/bit/SHA512ShortMsg.rsp");
   test_sha512_with_File("testvectors/sha2/bit/SHA512LongMsg.rsp");
   test_sha512_with_File("testvectors/sha2/bit/SHA512Monte.rsp");
   test_sha512_with_File("testvectors/sha2/byte/SHA512ShortMsg.rsp");
   test_sha512_with_File("testvectors/sha2/byte/SHA512LongMsg.rsp");
   test_sha512_with_File("testvectors/sha2/byte/SHA512Monte.rsp");

   New_Line;

end Test_SHA2;
