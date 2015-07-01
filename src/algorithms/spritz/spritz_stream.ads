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

use Crypto_Types.Crypto_Types_u8;

package Spritz_Stream is

   type Context is private;

   procedure Initialize (ctx : out Context; key : in u8_Array);
   procedure Initialize (ctx : out Context; key : in String);
   procedure Set_Iv (ctx : in out Context; iv : in u8_Array);
   procedure Encrypt (ctx : in out Context; data : in out u8_Array);
   procedure Decrypt (ctx : in out Context; data : in out u8_Array);

private

   type Context is record
      ctx : Spritz.Context;
   end record;

end Spritz_Stream;