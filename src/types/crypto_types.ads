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

with Crypto_Generic_Utils;
with Crypto_Core_Types; use Crypto_Core_Types;
with System;

with Ada.Direct_IO;
with Ada.Sequential_IO;

package Crypto_Types is

   subtype Bit_Order is System.Bit_Order;

   package Crypto_Utils_u8  is new Crypto_Generic_Utils(T =>  u8, T_Array =>  u8_Array);
   package Crypto_Utils_u16 is new Crypto_Generic_Utils(T => u16, T_Array => u16_Array);
   package Crypto_Utils_u32 is new Crypto_Generic_Utils(T => u32, T_Array => u32_Array);
   package Crypto_Utils_u64 is new Crypto_Generic_Utils(T => u64, T_Array => u64_Array);

   package  u8_Direct_IO is new Ada.Direct_IO( u8);
   package u16_Direct_IO is new Ada.Direct_IO(u16);
   package u32_Direct_IO is new Ada.Direct_IO(u32);
   package u64_Direct_IO is new Ada.Direct_IO(u64);

   package  u8_Sequential_IO is new Ada.Sequential_IO( u8);
   package u16_Sequential_IO is new Ada.Sequential_IO(u16);
   package u32_Sequential_IO is new Ada.Sequential_IO(u32);
   package u64_Sequential_IO is new Ada.Sequential_IO(u64);

end Crypto_Types;
