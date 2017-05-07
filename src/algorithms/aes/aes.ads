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
with Crypto_Types;      use Crypto_Types;
with AES_Spec;

use Crypto_Types.Crypto_Utils_u8;

package AES is

   package AES_128 is new Block_Cipher_Generic
     (Name_Intern            => "AES-128",
      Context_T_Intern       => AES_Spec.Context_128_T,
      Block_Size_Bits_Intern => 128,
      Key_Size_Bits_Intern   => 128,
      Initialize_Intern      => AES_Spec.Initialize,
      Encrypt_Intern         => AES_Spec.Encrypt,
      Decrypt_Intern         => AES_Spec.Decrypt);

   package AES_192 is new Block_Cipher_Generic
     (Name_Intern            => "AES-192",
      Context_T_Intern       => AES_Spec.Context_192_T,
      Block_Size_Bits_Intern => 128,
      Key_Size_Bits_Intern   => 192,
      Initialize_Intern      => AES_Spec.Initialize,
      Encrypt_Intern         => AES_Spec.Encrypt,
      Decrypt_Intern         => AES_Spec.Decrypt);

   package AES_256 is new Block_Cipher_Generic
     (Name_Intern            => "AES-256",
      Context_T_Intern       => AES_Spec.Context_256_T,
      Block_Size_Bits_Intern => 128,
      Key_Size_Bits_Intern   => 256,
      Initialize_Intern      => AES_Spec.Initialize,
      Encrypt_Intern         => AES_Spec.Encrypt,
      Decrypt_Intern         => AES_Spec.Decrypt);

end AES;
