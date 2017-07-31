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
with Crypto.Types.X;

use Crypto.Types.X.Utils_u8;
use Crypto.Types.X.Utils_u64;

package Keccak_Parameters is

   subtype Word_T is u64;
   w : constant := 64;
   l : constant := 6;
   b : constant := 25 * w;
   subtype Capacity_T is Natural range 0 .. b;
   subtype Capacity_Bytes_T is Natural range 0 .. b / 8;
   type x_T is mod 5;
   type y_T is mod 5;
   type z_T is mod w;

end Keccak_Parameters;
