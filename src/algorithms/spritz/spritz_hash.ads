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
with Spritz;

use Crypto_Types.Crypto_Utils_u8;

package Spritz_Hash is

   type Context is private;

   procedure Initialize (ctx : out Context);
   procedure Add_Domain (ctx : in out Context; Domain : in u8_Array);
   procedure Add_Domain (ctx : in out Context; Domain : in String);
   procedure Add_Data (ctx : in out Context; data : in u8_Array);
   procedure Add_Data (ctx : in out Context; data : in String);
   procedure Extract_Hash (ctx : in out Context; Hash : out u8_Array);

private

   type Context is record
      ctx : Spritz.Context;
   end record;

end Spritz_Hash;