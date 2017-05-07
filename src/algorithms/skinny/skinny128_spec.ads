--  Copyright (C) 2016  bg nerilex <bg@nerilex.org>
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
with Crypto_Types;      use Crypto_Types;

use Crypto_Types.Crypto_Utils_u8;

package Skinny128_Spec is

--     subtype Tweakey_128_T is Block_128_Bit;
--     subtype Key_256_T is Block_256_Bit;
--     subtype Key_384_T is Block_384_Bit;
--
--     type Context_128_T is private;
--     type Context_256_T is private;
--     type Context_384_T is private;
--
--     subtype Block_T is Block_128_Bit;
--
--     procedure Initialize(Context : out Context_128_T; Key : in Tweakey_128_T);
--     procedure Encrypt(Context : in Context_128_T; Block: in out Block_T);
--     procedure Decrypt(Context : in Context_128_T; Block: in out Block_T);
--
--     procedure Initialize(Context : out Context_256_T; Key : in Key_256_T);
--     procedure Encrypt(Context : in Context_256_T; Block: in out Block_T);
--     procedure Decrypt(Context : in Context_256_T; Block: in out Block_T);
--
--     procedure Initialize(Context : out Context_384_T; Key : in Key_384_T);
--     procedure Encrypt(Context : in Context_384_T; Block: in out Block_T);
--     procedure Decrypt(Context : in Context_384_T; Block: in out Block_T);
--
--  private
--
--     subtype RoundKey_T is Block_128_Bit;
--     type RoundKeys_T is Array (Integer range <>) of RoundKey_T;
--
--     subtype Num_RoundKeys_T is Integer range 11 .. 15;
--
--     type Context_T(Num_RoundKeys : Num_RoundKeys_T := 15) is record
--        RoundKeys : RoundKeys_T(1 .. Num_RoundKeys);
--     end record;
--
--     type Context_128_T is new Context_T(11);
--     type Context_192_T is new Context_T(13);
--     type Context_256_T is new Context_T(15);
--
--     Nb : constant Integer := 4;
--     polynom : constant u8 := 16#1B#;
--     function gf256mul(a, b : u8) return u8;
--
--     generic
--        type T_In(<>) is new u8_Array;
--        type T_Out(<>) is new Context_T;
--     procedure Initialize_Generic(Key : T_In; Context : out T_Out);

end Skinny128_Spec;
