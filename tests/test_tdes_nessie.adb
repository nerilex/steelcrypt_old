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

--  with Ada.Text_IO; use Ada.Text_IO;
--  with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Crypto_Core_Types; use Crypto_Core_Types;
with Crypto_Types;      use Crypto_Types;
use Crypto_Types.Crypto_Utils_u8;

with Nessie_BC_Test_Generator;
with TDES;

procedure Test_TDES_Nessie is

   package Nessie_Test_2 is new Nessie_BC_Test_Generator (TDES.TDES_Two_Key);

   package Nessie_Test_3 is new Nessie_BC_Test_Generator (TDES.TDES_Three_Key);

begin

   Nessie_Test_2.Verbose := True;
   Nessie_Test_2.Run_File;
   Nessie_Test_3.Verbose := True;
   Nessie_Test_3.Run_File;

end Test_TDES_Nessie;
