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
with Sha2_Small;

package SHA2_256_Spec is

   Name : constant String := "SHA2-256";

   type Context_T is private;

   Block_Size_Bits : constant := 512;
   Digest_Size_Bits : constant := 256;

   Block_Size_Bytes : constant := (Block_Size_Bits) / 8;
   Digest_Size_Bytes : constant := (Digest_Size_Bits) / 8;

   subtype Block_T is u8_Array(1 .. Block_Size_Bytes);
   subtype Digest_T is u8_Array(1 .. Digest_Size_Bytes);

   procedure Initialize(Context : out Context_T);
   procedure Next_Block(Context : in out Context_T; Block : in Block_T);
   procedure Last_Block(Context : in out Context_T; Block : in u8_Array; Bits : in Integer := -1);
   procedure Get_Digest(Context : in out Context_T; Digest : out Digest_T);


private

   type Context_T is new Sha2_Small.Context_T;

end SHA2_256_Spec;
