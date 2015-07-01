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

package Spritz is

   N : constant Integer:= 256;

   type Context is private;

   procedure Initialize (ctx : out Context);
   procedure AbsorbStop (ctx : in out Context);
   procedure Absorb (ctx : in out Context; x : in u8);
   procedure Absorb (ctx : in out Context; x : in u8_Array);
   procedure Absorb (ctx : in out Context; x : in String);
   procedure Absorb (ctx : in out Context; x : in Natural);
   procedure Drip (ctx : in out Context; z : out u8);
   procedure Squeeze (ctx : in out Context; P : out u8_Array);

private


   type S_Array is Array (u8 range <>) of u8;

   type Context is record
      S : S_Array (0 .. u8(N - 1));
      i, j, k, z, w, a : u8;
   end record;

end Spritz;
