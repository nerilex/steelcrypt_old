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

with Nessie_Hash_Test_Generator;

with SHA3;

use Crypto_Types.Crypto_Utils_u8;

procedure Test_SHA3_Nessie is

   package Nessie_Test_Sha224 is new Nessie_Hash_Test_Generator(SHA3.SHA3_224);
   package Nessie_Test_Sha256 is new Nessie_Hash_Test_Generator(SHA3.SHA3_256);
   package Nessie_Test_Sha384 is new Nessie_Hash_Test_Generator(SHA3.SHA3_384);
   package Nessie_Test_Sha512 is new Nessie_Hash_Test_Generator(SHA3.SHA3_512);

begin

   Nessie_Test_Sha224.Run_File;
   Nessie_Test_Sha256.Run_File;
   Nessie_Test_Sha384.Run_File;
   Nessie_Test_Sha512.Run_File;

   New_Line;

end Test_SHA3_Nessie;
