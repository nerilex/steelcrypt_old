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
with Skinny128_Generic;
with Skinny128_Types; use Skinny128_Types;

package Skinny128_256 is

   subtype Block_T is Block_128_Bit;
   subtype Key_T is Block_256_Bit;
   type Context_T is private;

   function Initialize (Tweakey : in Key_T) return Context_T;
   function Encrypt
     (Context : in Context_T;
      Block   : in Block_T) return Block_T;
   function Decrypt
     (Context : in Context_T;
      Block   : in Block_T) return Block_T;

private

   type Tweakey_State_T is array (1 .. 2) of State_T;

   function TK_Update(Tk : Tweakey_State_T) return Tweakey_State_T;
   function Get_Round_Tweakey(Tk : Tweakey_State_T) return Round_Tweakey_T;
   function Load_Tweakey(Key : u8_Array) return Tweakey_State_T;

   package Core is new Skinny128_Generic(Rounds            => 48,
                                                   Tweakey_Size      => 256 / 8,
                                                   Tweakey_State_T   => Tweakey_State_T,
                                                   TK_Update         => TK_Update,
                                                   Get_Round_Tweakey => Get_Round_Tweakey,
                                                   Load_Tweakey      => Load_Tweakey);

   type Context_T is new Core.Context_T;


end Skinny128_256;
