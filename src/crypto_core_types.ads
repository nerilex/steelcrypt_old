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

   type Bit is mod 2;
   for Bit'Size use 1;

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

   subtype Block_32_Bit   is u8_Array(1 ..   32 / 8);
   subtype Block_48_Bit   is u8_Array(1 ..   48 / 8);
   subtype Block_56_Bit   is u8_Array(1 ..   56 / 8);
   subtype Block_64_Bit   is u8_Array(1 ..   64 / 8);
   subtype Block_96_Bit   is u8_Array(1 ..   96 / 8);
   subtype Block_128_Bit  is u8_Array(1 ..  128 / 8);
   subtype Block_160_Bit  is u8_Array(1 ..  160 / 8);
   subtype Block_168_Bit  is u8_Array(1 ..  168 / 8);
   subtype Block_192_Bit  is u8_Array(1 ..  192 / 8);
   subtype Block_224_Bit  is u8_Array(1 ..  224 / 8);
   subtype Block_256_Bit  is u8_Array(1 ..  256 / 8);
   subtype Block_384_Bit  is u8_Array(1 ..  384 / 8);
   subtype Block_512_Bit  is u8_Array(1 ..  512 / 8);
   subtype Block_768_Bit  is u8_Array(1 ..  768 / 8);
   subtype Block_1024_Bit is u8_Array(1 .. 1024 / 8);
   subtype Block_1536_Bit is u8_Array(1 .. 1536 / 8);
   subtype Block_2048_Bit is u8_Array(1 .. 2048 / 8);
   subtype Block_4096_Bit is u8_Array(1 .. 4096 / 8);
   subtype Block_8192_Bit is u8_Array(1 .. 8192 / 8);

   Wrong_Opertaion_Order : exception;
   Format_Violation : exception;

   function To_Hex(A : u8) return String;
   function To_Hex(A : u8_Array) return String;
   function From_Hex(S : String) return u8_Array;
   function From_Ascii(S : String) return u8_Array;

   procedure Bit_Clear(Buffer : in out u8_Array; Index : in Positive);
   procedure Bit_Set(Buffer : in out u8_Array; Index : in Positive);
   procedure Bit_Set(Buffer : in out u8_Array; Index : in Positive; Value : in Bit);
   procedure Bit_Toggle(Buffer : in out u8_Array; Index : in Positive);
   function Bit_Get(Buffer : in u8_Array; Index : in Positive) return Bit;

end Crypto_Core_Types;
