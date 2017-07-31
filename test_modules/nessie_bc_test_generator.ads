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

with Crypto.Types; use Crypto.Types;
with Block_Cipher_Generic;

--
--  ********************************************************************************
--  *Project NESSIE - New European Schemes for Signature, Integrity, and Encryption*
--  ********************************************************************************
--
--  Primitive Name: Des
--  ===================
--  Key size: 64 bits
--  Block size: 64 bits
--
--  Test vectors -- set 1
--  =====================
--
--  Set 1, vector#  0:
--  key=8000000000000000
--  Plain=0000000000000000
--  cipher=95A8D72813DAA94D
--  decrypted=0000000000000000
--  Iterated 100 times=F749E1F8DEFAF605
--  Iterated 1000 times=F396DD0B33D04244
--
--  Set 1, vector#  1:
--  key=4000000000000000
--  plain=0000000000000000
--  cipher=0EEC1487DD8C26D5
--  decrypted=0000000000000000
--  Iterated 100 times=E5BEE86B600F3B48
--  Iterated 1000 times=1D5931D700EF4E15

generic
   with package Cipher is new Block_Cipher_Generic (<>);

package Nessie_BC_Test_Generator is

   Verbose : Boolean := True;
   procedure Run (FileName : String := "");
   procedure Run_File;

   Default_Suffix : constant String := ".test-vectors";

private

   subtype Key_T is u8_Array (1 .. Cipher.Key_Size_Bytes);
   subtype Block_T is u8_Array (1 .. Cipher.Block_Size_Bytes);

end Nessie_BC_Test_Generator;
