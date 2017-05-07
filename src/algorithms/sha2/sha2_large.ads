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

with Crypto_Core_Types; use Crypto_Core_Types;
with Crypto_Types;      use Crypto_Types;

use Crypto_Types.Crypto_Utils_u8;
use Crypto_Types.Crypto_Utils_u64;

package SHA2_Large is

   type Context_T is record
      H       : u64_Array (1 .. 8);
      counter : Natural;
   end record;

   procedure Next_Block
     (Context : in out Context_T;
      Block   : in     Block_1024_Bit);
   procedure Last_Block
     (Context : in out Context_T;
      Block   : in     u8_Array;
      Bits    : in     Integer := -1);

private

end SHA2_Large;
