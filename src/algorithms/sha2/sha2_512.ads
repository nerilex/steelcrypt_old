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
with Sha2_Large;

use Crypto_Types.Crypto_Types_u8;
use Crypto_Types.Crypto_Types_u64;

package SHA2_512 is

   type Context_T is private;

   BlockSize_Bits : constant := 1024;
   DigestSize_Bits : constant := 512;

   BlockSize_Bytes : constant := (BlockSize_Bits) / 8;
   DigestSize_Bytes : constant := (DigestSize_Bits) / 8;

   procedure Initialize(Context : out Context_T);
   procedure Next_Block(Context : in out Context_T; Block : in Block_1024_Bit);
   procedure Last_Block(Context : in out Context_T; Block : in u8_Array; Bits : in Integer := -1);
   procedure Get_Digest(Context : in out Context_T; Digest : out Block_512_Bit);

   procedure Hash(Data : in u8_array; Digest : out Block_512_Bit; Bits : in Integer := -1);

private

   type Context_T is new Sha2_Large.Context_T;

end SHA2_512;
