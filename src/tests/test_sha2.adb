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

with Nessie_Hash_Test_Generator;

with SHA2_224;
with SHA2_256;
with SHA2_384;
with SHA2_512;

use Crypto_Types.Crypto_Types_u8;

procedure Test_SHA2 is

   procedure test_sha224_with_File is new Sha_Test_IO.Test_With_File(Digest_Size_Bits => SHA2_224.Digest_Size_Bits, Hash => SHA2_224.Hash);
   procedure test_sha256_with_File is new Sha_Test_IO.Test_With_File(Digest_Size_Bits => SHA2_256.Digest_Size_Bits, Hash => SHA2_256.Hash);
   procedure test_sha384_with_File is new Sha_Test_IO.Test_With_File(Digest_Size_Bits => SHA2_384.Digest_Size_Bits, Hash => SHA2_384.Hash);
   procedure test_sha512_with_File is new Sha_Test_IO.Test_With_File(Digest_Size_Bits => SHA2_512.Digest_Size_Bits, Hash => SHA2_512.Hash);

   package Nessie_Test_Sha224 is new Nessie_Hash_Test_Generator(
                                                                Name             => "SHA-224",
                                                                Digest_Size_Bits => SHA2_224.Digest_Size_Bits,
                                                                Block_Size_Bits  => SHA2_224.Block_Size_Bits,
                                                                Context_T        => SHA2_224.Context_T,
                                                                Initialize       => SHA2_224.Initialize,
                                                                Next_Block       => SHA2_224.Next_Block,
                                                                Last_Block       => SHA2_224.Last_Block,
                                                                Get_Digest       => SHA2_224.Get_Digest,
                                                                Hash             => SHA2_224.Hash);
   package Nessie_Test_Sha256 is new Nessie_Hash_Test_Generator(
                                                                Name             => "SHA-256",
                                                                Digest_Size_Bits => SHA2_256.Digest_Size_Bits,
                                                                Block_Size_Bits  => SHA2_256.Block_Size_Bits,
                                                                Context_T        => SHA2_256.Context_T,
                                                                Initialize       => SHA2_256.Initialize,
                                                                Next_Block       => SHA2_256.Next_Block,
                                                                Last_Block       => SHA2_256.Last_Block,
                                                                Get_Digest       => SHA2_256.Get_Digest,
                                                                Hash             => SHA2_256.Hash);
   package Nessie_Test_Sha384 is new Nessie_Hash_Test_Generator(
                                                                Name             => "SHA-384",
                                                                Digest_Size_Bits => SHA2_384.Digest_Size_Bits,
                                                                Block_Size_Bits  => SHA2_384.Block_Size_Bits,
                                                                Context_T        => SHA2_384.Context_T,
                                                                Initialize       => SHA2_384.Initialize,
                                                                Next_Block       => SHA2_384.Next_Block,
                                                                Last_Block       => SHA2_384.Last_Block,
                                                                Get_Digest       => SHA2_384.Get_Digest,
                                                                Hash             => SHA2_384.Hash);
   package Nessie_Test_Sha512 is new Nessie_Hash_Test_Generator(
                                                                Name             => "SHA-512",
                                                                Digest_Size_Bits => SHA2_512.Digest_Size_Bits,
                                                                Block_Size_Bits  => SHA2_512.Block_Size_Bits,
                                                                Context_T        => SHA2_512.Context_T,
                                                                Initialize       => SHA2_512.Initialize,
                                                                Next_Block       => SHA2_512.Next_Block,
                                                                Last_Block       => SHA2_512.Last_Block,
                                                                Get_Digest       => SHA2_512.Get_Digest,
                                                                Hash             => SHA2_512.Hash);

begin

   Nessie_Test_Sha224.Run_File;
   Nessie_Test_Sha256.Run_File;
   Nessie_Test_Sha384.Run_File;
   Nessie_Test_Sha512.Run_File;

--     New_Line;
--     test_sha224_with_File("testvectors/sha2/bit/SHA224ShortMsg.rsp");
--     test_sha224_with_File("testvectors/sha2/bit/SHA224LongMsg.rsp");
--     test_sha224_with_File("testvectors/sha2/bit/SHA224Monte.rsp");
--     test_sha224_with_File("testvectors/sha2/byte/SHA224ShortMsg.rsp");
--     test_sha224_with_File("testvectors/sha2/byte/SHA224LongMsg.rsp");
--     test_sha224_with_File("testvectors/sha2/byte/SHA224Monte.rsp");
--
--     New_Line;
--     test_sha256_with_File("testvectors/sha2/bit/SHA256ShortMsg.rsp");
--     test_sha256_with_File("testvectors/sha2/bit/SHA256LongMsg.rsp");
--     test_sha256_with_File("testvectors/sha2/bit/SHA256Monte.rsp");
--     test_sha256_with_File("testvectors/sha2/byte/SHA256ShortMsg.rsp");
--     test_sha256_with_File("testvectors/sha2/byte/SHA256LongMsg.rsp");
--     test_sha256_with_File("testvectors/sha2/byte/SHA256Monte.rsp");
--
--     New_Line;
--     test_sha384_with_File("testvectors/sha2/bit/SHA384ShortMsg.rsp");
--     test_sha384_with_File("testvectors/sha2/bit/SHA384LongMsg.rsp");
--     test_sha384_with_File("testvectors/sha2/bit/SHA384Monte.rsp");
--     test_sha384_with_File("testvectors/sha2/byte/SHA384ShortMsg.rsp");
--     test_sha384_with_File("testvectors/sha2/byte/SHA384LongMsg.rsp");
--     test_sha384_with_File("testvectors/sha2/byte/SHA384Monte.rsp");
--
--     New_Line;
--     test_sha512_with_File("testvectors/sha2/bit/SHA512ShortMsg.rsp");
--     test_sha512_with_File("testvectors/sha2/bit/SHA512LongMsg.rsp");
--     test_sha512_with_File("testvectors/sha2/bit/SHA512Monte.rsp");
--     test_sha512_with_File("testvectors/sha2/byte/SHA512ShortMsg.rsp");
--     test_sha512_with_File("testvectors/sha2/byte/SHA512LongMsg.rsp");
--     test_sha512_with_File("testvectors/sha2/byte/SHA512Monte.rsp");

   New_Line;

end Test_SHA2;
