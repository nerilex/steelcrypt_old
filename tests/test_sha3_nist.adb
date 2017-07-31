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
with Crypto.Types;      use Crypto.Types;

with Sha_Test_IO;

with SHA3; use SHA3;

with Crypto.Types.X;
use Crypto.Types.X.Utils_u8;

procedure Test_SHA3_Nist is

   procedure test_sha3_224_with_File is new Sha_Test_IO.Test_With_File
     (Digest_Size_Bits => SHA3_224.Digest_Size_Bits,
      Hash             => SHA3_224.Hash);
   procedure test_sha3_256_with_File is new Sha_Test_IO.Test_With_File
     (Digest_Size_Bits => SHA3_256.Digest_Size_Bits,
      Hash             => SHA3_256.Hash);
   procedure test_sha3_384_with_File is new Sha_Test_IO.Test_With_File
     (Digest_Size_Bits => SHA3_384.Digest_Size_Bits,
      Hash             => SHA3_384.Hash);
   procedure test_sha3_512_with_File is new Sha_Test_IO.Test_With_File
     (Digest_Size_Bits => SHA3_512.Digest_Size_Bits,
      Hash             => SHA3_512.Hash);

begin
   New_Line;

   test_sha3_224_with_File ("testvectors/sha3/pre/ShortMsgKAT_SHA3-224.txt");
   test_sha3_256_with_File ("testvectors/sha3/pre/ShortMsgKAT_SHA3-256.txt");
   test_sha3_384_with_File ("testvectors/sha3/pre/ShortMsgKAT_SHA3-384.txt");
   test_sha3_512_with_File ("testvectors/sha3/pre/ShortMsgKAT_SHA3-512.txt");

--     New_Line;
--     test_sha3_224_with_File("testvectors/sha3/bit/sha3_224ShortMsg.rsp");
--     test_sha3_224_with_File("testvectors/sha3/bit/sha3_224LongMsg.rsp");
--     test_sha3_224_with_File("testvectors/sha3/bit/sha3_224Monte.rsp");
--     test_sha3_224_with_File("testvectors/sha3/byte/sha3_224ShortMsg.rsp");
--     test_sha3_224_with_File("testvectors/sha3/byte/sha3_224LongMsg.rsp");
--     test_sha3_224_with_File("testvectors/sha3/byte/sha3_224Monte.rsp");
--
--     New_Line;
--     test_sha3_256_with_File("testvectors/sha3/bit/sha3_256ShortMsg.rsp");
--     test_sha3_256_with_File("testvectors/sha3/bit/sha3_256LongMsg.rsp");
--     test_sha3_256_with_File("testvectors/sha3/bit/sha3_256Monte.rsp");
--     test_sha3_256_with_File("testvectors/sha3/byte/sha3_256ShortMsg.rsp");
--     test_sha3_256_with_File("testvectors/sha3/byte/sha3_256LongMsg.rsp");
--     test_sha3_256_with_File("testvectors/sha3/byte/sha3_256Monte.rsp");
--
--     New_Line;
--     test_sha3_384_with_File("testvectors/sha3/bit/sha3_284ShortMsg.rsp");
--     test_sha3_384_with_File("testvectors/sha3/bit/sha3_284LongMsg.rsp");
--     test_sha3_384_with_File("testvectors/sha3/bit/sha3_284Monte.rsp");
--     test_sha3_384_with_File("testvectors/sha3/byte/sha3_284ShortMsg.rsp");
--     test_sha3_384_with_File("testvectors/sha3/byte/sha3_284LongMsg.rsp");
--     test_sha3_384_with_File("testvectors/sha3/byte/sha3_284Monte.rsp");
--
--     New_Line;
--     test_sha3_512_with_File("testvectors/sha3/bit/SHA512ShortMsg.rsp");
--     test_sha3_512_with_File("testvectors/sha3/bit/SHA512LongMsg.rsp");
--     test_sha3_512_with_File("testvectors/sha3/bit/SHA512Monte.rsp");
--     test_sha3_512_with_File("testvectors/sha3/byte/SHA512ShortMsg.rsp");
--     test_sha3_512_with_File("testvectors/sha3/byte/SHA512LongMsg.rsp");
--     test_sha3_512_with_File("testvectors/sha3/byte/SHA512Monte.rsp");

   New_Line;

end Test_SHA3_Nist;
