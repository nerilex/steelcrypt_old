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

with Ada.Text_IO; use Ada.Text_IO;
--  with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Crypto_Core_Types; use Crypto_Core_Types;
with Crypto_Types;      use Crypto_Types;

with Sha3_Generic;

use Crypto_Types.Crypto_Utils_u8;
use Crypto_Types.Crypto_Utils_u64;

procedure Test_Keccak is
--     procedure Print_State(A : State_T) is
--        c : Natural := 1;
--        l : u8_Array(1 .. 8);
--     begin
--        for i in y_T'Range loop
--           for j in x_T'Range loop
--              Store_le(A     => l,
--                       value => A(j, i));
--              for z in l'Range loop
--                 Put(To_Hex(l(z)));
--                 Put(' ');
--                 if c mod 16 = 0 then
--                    New_Line;
--                 end if;
--                 c := c + 1;
--              end loop;
--           end loop;
--        end loop;
--        New_Line;
--     end;

--     A : State_T := ( 0 => ( 0 => 6, others => 0 ), 1 => ( 3 => 16#8000000000000000#, others => 0 ), others => ( others => 0 ));

   package Sha3_224 is new Sha3_Generic (Capacity_Bits => 448);
   Digest : Sha3_224.Digest_T;
begin
--     Print_State(A);
--     Permute(A);
--     New_Line;
--     Print_State(A);
   Sha3_224.Hash (u8_Array'(1 => 16#13#), Digest, 5);
   Put_Line (To_Hex (Digest));
end Test_Keccak;
