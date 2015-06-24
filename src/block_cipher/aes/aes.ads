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

use Crypto_Types.Crypto_Types_u8;

package AES is

   type Key_128 is new Block_128_Bit;
   type Key_192 is new Block_192_Bit;
   type Key_256 is new Block_256_Bit;

 --  type Context_128 is private;
 --  type Context_192 is private;
 --  type Context_256 is private;

   type Plaintext is new Block_128_Bit;
   type Ciphertext is new Block_128_Bit;


   type RoundKey_T is new Block_128_Bit;
   type RoundKeys_T is Array (Integer range <>) of RoundKey_T;

   type Context_128 is record
      RoundKeys : RoundKeys_T(1 .. 11);
   end record;

   type Context_192 is record
      RoundKeys : RoundKeys_T(1 .. 13);
   end record;

   type Context_256 is record
      RoundKeys : RoundKeys_T(1 .. 15);
   end record;

   function Initialize(Key : Key_128) return Context_128;
--     function Encrypt(Ctx : Context_128; Source : Plaintext) return Ciphertext;
--     function Decrypt(Ctx : Context_128; Source : Ciphertext) return Plaintext;
--
   function Initialize(Key : Key_192) return Context_192;
--     function Encrypt(Ctx : Context_192; Source : Plaintext) return Ciphertext;
--     function Decrypt(Ctx : Context_192; Source : Ciphertext) return Plaintext;
--
   function Initialize(Key : Key_256) return Context_256;
--     function Encrypt(Ctx : Context_256; Source : Plaintext) return Ciphertext;
--     function Decrypt(Ctx : Context_256; Source : Ciphertext) return Plaintext;

private

   Nb : constant Integer := 4;
   polynom : constant u8 := 16#1B#;
   function gf256mul(a, b : u8) return u8;
end AES;
