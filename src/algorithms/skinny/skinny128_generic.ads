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

with Skinny128_Types; use Skinny128_Types;

generic
   Rounds       : Natural;
   Tweakey_Size : Natural;
   type Tweakey_State_T is private;
   with function TK_Update
     (Tweakey_State : Tweakey_State_T) return Tweakey_State_T;
   with function Get_Round_Tweakey
     (Tweakey_State : Tweakey_State_T) return Round_Tweakey_T;
   with function Load_Tweakey
     (key_blob : u8_Array) return Tweakey_State_T;

package Skinny128_Generic is

   subtype Tweakey_T is u8_Array (1 .. Tweakey_Size);
   type Context_T is private;
   subtype Block_T is Block_128_Bit;

   function Initialize (Tweakey : in Tweakey_T) return Context_T;
   function Encrypt
     (Context : in Context_T;
      Block   : in Block_T) return Block_T;
   function Decrypt
     (Context : in Context_T;
      Block   : in Block_T) return Block_T;

private

   type Round_Tweakeys_T is array (1 .. Rounds) of Round_Tweakey_T;

   type Context_T is record
      Round_Tweakeys : Round_Tweakeys_T;
   end record;

   type Sbox_T is array (Cell_T'Range) of Cell_T;

end Skinny128_Generic;
