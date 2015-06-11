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

with Interfaces; use Interfaces;

package Crypto_Core_Types is

   type u8  is new Unsigned_8;
   for u8'Size use 8;

   type u16 is new Unsigned_16;
   for u16'Size use 16;

   type u32 is new Unsigned_32;
   for u32'Size use 32;

   type u64 is new Unsigned_64;
   for u64'Size use 64;

   type u8_Array  is Array (Integer range <>) of u8;
   type u16_Array is Array (Integer range <>) of u16;
   type u32_Array is Array (Integer range <>) of u32;
   type u64_Array is Array (Integer range <>) of u64;

   type u8_Array_Access  is access all u8_Array;
   type u16_Array_Access is access all u16_Array;
   type u32_Array_Access is access all u32_Array;
   type u64_Array_Access is access all u64_Array;

end Crypto_Core_Types;
