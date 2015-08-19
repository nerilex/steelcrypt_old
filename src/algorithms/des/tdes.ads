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

with DES;

use Crypto_Types.Crypto_Types_u8;
use Crypto_Types.Crypto_Types_u32;

package TDES is

   type Context_T is private;

   subtype Key_128_T is Block_128_Bit;
   subtype Key_192_T is Block_192_Bit;
   subtype Block_T is Block_64_Bit;

   procedure Initialize(Context : out Context_T; Key : in u8_Array);
   procedure Encrypt(Context : in Context_T; Block: in out Block_64_Bit);
   procedure Decrypt(Context : in Context_T; Block: in out Block_64_Bit);

private
   type Context_T is record
      Ctx1 : DES.Context_T;
      Ctx2 : DES.Context_T;
      Ctx3 : DES.Context_T;
   end record;

end TDES;
