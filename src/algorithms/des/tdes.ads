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

with Block_Cipher_Generic;
with Crypto_Core_Types; use Crypto_Core_Types;
with Crypto_Types; use Crypto_Types;
with TDES_Spec;

use Crypto_Types.Crypto_Utils_u8;

package TDES is

   package TDES_Two_Key is new Block_Cipher_Generic( Name_Intern            => "Triple-DES (two keys)",
                                                     Context_T_Intern       => TDES_Spec.Context_T,
                                                     Block_Size_Bits_Intern => 64,
                                                     Key_Size_Bits_Intern   => 128,
                                                     Initialize_Intern      => TDES_Spec.Initialize,
                                                     Encrypt_Intern         => TDES_Spec.Encrypt,
                                                     Decrypt_Intern         => TDES_Spec.Decrypt );

   package TDES_Three_Key is new Block_Cipher_Generic( Name_Intern            => "Triple-DES (three keys)",
                                                       Context_T_Intern       => TDES_Spec.Context_T,
                                                       Block_Size_Bits_Intern => 64,
                                                       Key_Size_Bits_Intern   => 192,
                                                       Initialize_Intern      => TDES_Spec.Initialize,
                                                       Encrypt_Intern         => TDES_Spec.Encrypt,
                                                       Decrypt_Intern         => TDES_Spec.Decrypt );

end TDES;
