--  Copyright (C) 2017  bg nerilex <bg@nerilex.org>
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

with Block_Cipher_Generic;

generic

   with package Cipher is new Block_Cipher_Generic(<>);
   Width_Bits : Natural;

package Block_Cipher_CFB is

     type Context_T is private;

   subtype Initialization_Vector_T is u8_Array(1 .. Cipher.Block_Size_Bytes);

   procedure Initialize
     (Context :    out Context_T;
      Key     : in     u8_Array);

   procedure Set_Initialization_Vector
     (Context : in out Context_T;
      IV      : in     Initialization_Vector_T);

   procedure Encrypt
     (Context : in out Context_T;
      Block   : in out u8_Array);

   procedure Decrypt
     (Context : in out Context_T;
      Block   : in out u8_Array);

   procedure Encrypt
     (Context : in out Context_T;
      Block   : in out Bit_Array);

   procedure Decrypt
     (Context : in out Context_T;
      Block   : in out Bit_Array);

private

   type Context_T is record
      Cipher_Context : Cipher.Context_T;
      Block : u8_Array(1 .. Cipher.Block_Size_Bytes);
      Buffer_Fill : Natural;
   end record;


end Block_Cipher_CFB;
