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
with Crypto.Types; use Crypto.Types;
with Crypto.Types;      use Crypto.Types;
with DES_Spec;

with Crypto.Types.X;
use Crypto.Types.X.Utils_u8;

package DES is new Block_Cipher_Generic
  (Name_Intern => "DES",
   Context_T_Intern => DES_Spec.Context_T,
   Block_Size_Bits_Intern => 64,
   Key_Size_Bits_Intern => 64,
   Initialize_Intern => DES_Spec.Initialize,
   Encrypt_Intern => DES_Spec.Encrypt,
   Decrypt_Intern => DES_Spec.Decrypt);
