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

with Block_Cipher_Generic;
with Crypto_Core_Types; use Crypto_Core_Types;

generic
   with package Cipher is new Block_Cipher_Generic (<>);

package GCM128_Spec is
   type Context_T is private;

   Key_Size_Bits    : Natural := Cipher.Key_Size_Bits;
   Block_Size_Bits  : Natural := Cipher.Block_Size_Bits;
   Key_Size_Bytes   : Natural := Cipher.Key_Size_Bytes;
   Block_Size_Bytes : Natural := Cipher.Block_Size_Bytes;

   procedure Initialize
     (Context :    out Context_T;
      Key     : in     u8_Array;
      IV      : in     u8_Array);
   procedure Header_Next_Block
     (Context : in out Context_T;
      Header  : in     u8_Array);
   procedure Header_Last_Block
     (Context : in out Context_T;
      Header  : in     u8_Array);
   procedure Encrypt_Next_Block
     (Context : in out Context_T;
      Block   : in out u8_Array);
   procedure Encrypt_Last_Block
     (Context : in out Context_T;
      Block   : in out u8_Array);
   procedure Decrypt_Next_Block
     (Context : in out Context_T;
      Block   : in out u8_Array);
   procedure Decrypt_Last_Block
     (Context : in out Context_T;
      Block   : in out u8_Array);
   procedure Get_Tag (Context : in Context_T; Tag : out u8_Array);
   function Is_Valid
     (Context : in Context_T;
      Tag     : in u8_Array) return Boolean;

private

   type Context_T is record
      ICB, J0, H, Y    : Block_128_Bit;
      Header_Length    : u64;
      Plaintext_Length : u64;
      Cipher_Ctx       : Cipher.Context_T;
   end record;

end GCM128_Spec;
