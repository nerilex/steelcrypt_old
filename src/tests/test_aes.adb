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

with Crypto_Core_Types; use Crypto_Core_Types;
with Crypto_Types; use Crypto_Types;
use Crypto_Types.Crypto_Types_u8;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Nessie_BC_Test_Generator;
with AES;

procedure Test_AES is

   package Nessie_Test_128 is new Nessie_BC_Test_Generator(
                                                           Name => "AES-128",
                                                           Key_Size_Bits => AES.Key_128_T'Length * 8,
                                                           Block_Size_Bits => AES.Block_T'Length * 8,
                                                           Context_T => AES.Context_128_T,
                                                           Initialize => AES.Initialize,
                                                           Encrypt => AES.Encrypt,
                                                           Decrypt => AES.Decrypt );

   package Nessie_Test_192 is new Nessie_BC_Test_Generator(
                                                           Name => "AES-192",
                                                           Key_Size_Bits => AES.Key_192_T'Length * 8,
                                                           Block_Size_Bits => AES.Block_T'Length * 8,
                                                           Context_T => AES.Context_192_T,
                                                           Initialize => AES.Initialize,
                                                           Encrypt => AES.Encrypt,
                                                           Decrypt => AES.Decrypt );

   package Nessie_Test_256 is new Nessie_BC_Test_Generator(
                                                           Name => "AES-256",
                                                           Key_Size_Bits => AES.Key_256_T'Length * 8,
                                                           Block_Size_Bits => AES.Block_T'Length * 8,
                                                           Context_T => AES.Context_256_T,
                                                           Initialize => AES.Initialize,
                                                           Encrypt => AES.Encrypt,
                                                           Decrypt => AES.Decrypt );

begin

   Nessie_Test_128.Run_File;
   Nessie_Test_192.Run_File;
   Nessie_Test_256.Run_File;

end Test_AES;
