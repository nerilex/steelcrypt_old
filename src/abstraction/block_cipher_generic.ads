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

generic

   Name_Intern : String;

   type Context_T_Intern is private;

   Block_Size_Bits_Intern : Natural;
   Key_Size_Bits_Intern   : Natural;

   with procedure Initialize_Intern
     (Context :    out Context_T_Intern;
      Key     : in     u8_Array);
   with procedure Encrypt_Intern
     (Context : in     Context_T_Intern;
      Block   : in out u8_Array);
   with procedure Decrypt_Intern
     (Context : in     Context_T_Intern;
      Block   : in out u8_Array);

package Block_Cipher_Generic is

   Name : String renames Name_Intern;

   subtype Context_T is Context_T_Intern;

   Block_Size_Bits : Natural renames Block_Size_Bits_Intern;
   Key_Size_Bits : Natural renames Key_Size_Bits_Intern;

   Block_Size_Bytes : Natural := (Block_Size_Bits) / 8;
   Key_Size_Bytes   : Natural := (Key_Size_Bits) / 8;

   procedure Initialize
     (Context :    out Context_T_Intern;
      Key     : in     u8_Array) renames
     Initialize_Intern;
   procedure Encrypt
     (Context : in     Context_T_Intern;
      Block   : in out u8_Array) renames
     Encrypt_Intern;
   procedure Decrypt
     (Context : in     Context_T_Intern;
      Block   : in out u8_Array) renames
     Decrypt_Intern;

private

end Block_Cipher_Generic;
