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
with DES;

procedure Test_DES is

   package Nessie_Test is new Nessie_BC_Test_Generator(
                                                       Name => "DES",
                                                       Key_Size_Bits => DES.Key_T'Length * 8,
                                                       Block_Size_Bits => DES.Block_T'Length * 8,
                                                       Context_T => DES.Context_T,
                                                       Initialize => DES.Initialize,
                                                       Encrypt => DES.Encrypt,
                                                       Decrypt => DES.Decrypt );

begin

   Nessie_Test.Run_File;

end Test_DES;
