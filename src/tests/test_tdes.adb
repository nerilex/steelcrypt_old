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

--  with Ada.Text_IO; use Ada.Text_IO;
--  with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Crypto_Core_Types; use Crypto_Core_Types;
with Crypto_Types; use Crypto_Types;
use Crypto_Types.Crypto_Types_u8;

with Nessie_BC_Test_Generator;
with TDES;

procedure Test_TDES is

   package Nessie_Test_2 is new Nessie_BC_Test_Generator(
                                                         Name => "Triple-DES (two keys)",
                                                         Key_Size_Bits => TDES.Key_128_T'Length * 8,
                                                         Block_Size_Bits => TDES.Block_T'Length * 8,
                                                         Context_T => TDES.Context_T,
                                                         Initialize => TDES.Initialize,
                                                         Encrypt => TDES.Encrypt,
                                                         Decrypt => TDES.Decrypt );

   package Nessie_Test_3 is new Nessie_BC_Test_Generator(
                                                         Name => "Triple-DES (three keys)",
                                                         Key_Size_Bits => TDES.Key_192_T'Length * 8,
                                                         Block_Size_Bits => TDES.Block_T'Length * 8,
                                                         Context_T => TDES.Context_T,
                                                         Initialize => TDES.Initialize,
                                                         Encrypt => TDES.Encrypt,
                                                         Decrypt => TDES.Decrypt );

begin

   Nessie_Test_2.Verbose := True;
   Nessie_Test_2.Run_File;
   Nessie_Test_3.Verbose := True;
   Nessie_Test_3.Run_File;

end Test_TDES;
