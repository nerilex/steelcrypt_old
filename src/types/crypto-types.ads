--  Copyright (C) 2015  bg nerilex <bg@nerilex.org>
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
with System;

package Crypto.Types is

   type Storage_Order is (High_Order_First, Low_Order_First);
   subtype Bit_Order is System.Bit_Order;

   type Bit is mod 2;
   for Bit'Size use 1;
   subtype u1 is Bit;
   function Shift_Left  (Value : Bit; Amount : Natural)
                         return Bit;
   function Shift_Right (Value : Bit; Amount : Natural)
                         return Bit;
   function Shift_Right_Arithmetic (Value : Bit; Amount : Natural)
                                    return Bit;
   function Rotate_Left  (Value : Bit; Amount : Natural)
                          return Bit;
   function Rotate_Right (Value : Bit; Amount : Natural)
                          return Bit;

   type Nibble is mod 2 ** 4;
   for Nibble'Size use 4;

   function Shift_Left  (Value : Nibble; Amount : Natural)
                                              return Nibble;
   function Shift_Right (Value : Nibble; Amount : Natural)
                         return Nibble;
--     function Shift_Right_Arithmetic (Value : Nibble; Amount : Natural)
--                                      return Nibble;
   function Rotate_Left  (Value : Nibble; Amount : Natural)
                          return Nibble;
   function Rotate_Right (Value : Nibble; Amount : Natural)
                          return Nibble;
   type u4 is new Nibble;

--     use type Unsigned_8;
--     use type Unsigned_16;
--     use type Unsigned_32;
--     use type Unsigned_64;

--     subtype u8  is Unsigned_8;
--     subtype u16 is Unsigned_16;
--     subtype u32 is Unsigned_32;
--     subtype u64 is Unsigned_64;
--
--     function Shift_Left  (Value : Unsigned_8; Amount : Natural)
--                           return Unsigned_8 renames Interfaces.Shift_Left;
--     function Shift_Right (Value : Unsigned_8; Amount : Natural)
--                           return Unsigned_8 renames Interfaces.Shift_Right;
--     function Shift_Right_Arithmetic (Value : Unsigned_8; Amount : Natural)
--                           return Unsigned_8 renames Interfaces.Shift_Right_Arithmetic;
--     function Rotate_Left  (Value : Unsigned_8; Amount : Natural)
--                           return Unsigned_8 renames Interfaces.Rotate_Left;
--     function Rotate_Right (Value : Unsigned_8; Amount : Natural)
--                           return Unsigned_8 renames Interfaces.Rotate_Right;
--
--     function Shift_Left  (Value : Unsigned_16; Amount : Natural)
--                           return Unsigned_16 renames Interfaces.Shift_Left;
--     function Shift_Right (Value : Unsigned_16; Amount : Natural)
--                           return Unsigned_16 renames Interfaces.Shift_Right;
--     function Shift_Right_Arithmetic (Value : Unsigned_16; Amount : Natural)
--                                      return Unsigned_16 renames Interfaces.Shift_Right_Arithmetic;
--     function Rotate_Left  (Value : Unsigned_16; Amount : Natural)
--                            return Unsigned_16 renames Interfaces.Rotate_Left;
--     function Rotate_Right (Value : Unsigned_16; Amount : Natural)
--                            return Unsigned_16 renames Interfaces.Rotate_Right;
--
--     function Shift_Left  (Value : Unsigned_32; Amount : Natural)
--                           return Unsigned_32 renames Interfaces.Shift_Left;
--     function Shift_Right (Value : Unsigned_32; Amount : Natural)
--                           return Unsigned_32 renames Interfaces.Shift_Right;
--     function Shift_Right_Arithmetic (Value : Unsigned_32; Amount : Natural)
--                                      return Unsigned_32 renames Interfaces.Shift_Right_Arithmetic;
--     function Rotate_Left  (Value : Unsigned_32; Amount : Natural)
--                            return Unsigned_32 renames Interfaces.Rotate_Left;
--     function Rotate_Right (Value : Unsigned_32; Amount : Natural)
--                            return Unsigned_32 renames Interfaces.Rotate_Right;
--
--     function Shift_Left  (Value : Unsigned_64; Amount : Natural)
--                           return Unsigned_64 renames Interfaces.Shift_Left;
--     function Shift_Right (Value : Unsigned_64; Amount : Natural)
--                           return Unsigned_64 renames Interfaces.Shift_Right;
--     function Shift_Right_Arithmetic (Value : Unsigned_64; Amount : Natural)
--                                      return Unsigned_64 renames Interfaces.Shift_Right_Arithmetic;
--     function Rotate_Left  (Value : Unsigned_64; Amount : Natural)
--                            return Unsigned_64 renames Interfaces.Rotate_Left;
--     function Rotate_Right (Value : Unsigned_64; Amount : Natural)
--                            return Unsigned_64 renames Interfaces.Rotate_Right;


   type u8 is new Unsigned_8;
   for u8'Size use 8;

   type u16 is new Unsigned_16;
   for u16'Size use 16;

   type u32 is new Unsigned_32;
   for u32'Size use 32;

   type u64 is new Unsigned_64;
   for u64'Size use 64;

--        function Shift_Left  (Value : u8; Amount : Natural)
--                              return u8; -- renames Interfaces.Shift_Left;
--        function Shift_Right (Value : u8; Amount : Natural)
--                              return u8; -- renames Interfaces.Shift_Right;
--        function Shift_Right_Arithmetic (Value : u8; Amount : Natural)
--                              return u8; -- renames Interfaces.Shift_Right_Arithmetic;
--        function Rotate_Left  (Value : u8; Amount : Natural)
--                              return u8; -- renames Interfaces.Rotate_Left;
--        function Rotate_Right (Value : u8; Amount : Natural)
--                              return u8; -- renames Interfaces.Rotate_Right;
--
--        function Shift_Left  (Value : u16; Amount : Natural)
--                              return u16; -- renames Interfaces.Shift_Left;
--        function Shift_Right (Value : u16; Amount : Natural)
--                              return u16; -- renames Interfaces.Shift_Right;
--        function Shift_Right_Arithmetic (Value : u16; Amount : Natural)
--                                         return u16; -- renames Interfaces.Shift_Right_Arithmetic;
--        function Rotate_Left  (Value : u16; Amount : Natural)
--                               return u16; -- renames Interfaces.Rotate_Left;
--        function Rotate_Right (Value : u16; Amount : Natural)
--                               return u16; -- renames Interfaces.Rotate_Right;
--
--        function Shift_Left  (Value : u32; Amount : Natural)
--                              return u32; -- renames Interfaces.Shift_Left;
--        function Shift_Right (Value : u32; Amount : Natural)
--                              return u32; -- renames Interfaces.Shift_Right;
--        function Shift_Right_Arithmetic (Value : u32; Amount : Natural)
--                                         return u32; -- renames Interfaces.Shift_Right_Arithmetic;
--        function Rotate_Left  (Value : u32; Amount : Natural)
--                               return u32; -- renames Interfaces.Rotate_Left;
--        function Rotate_Right (Value : u32; Amount : Natural)
--                               return u32; -- renames Interfaces.Rotate_Right;
--
--        function Shift_Left  (Value : u64; Amount : Natural)
--                              return u64; -- renames Interfaces.Shift_Left;
--        function Shift_Right (Value : u64; Amount : Natural)
--                              return u64; -- renames Interfaces.Shift_Right;
--        function Shift_Right_Arithmetic (Value : u64; Amount : Natural)
--                                         return u64; -- renames Interfaces.Shift_Right_Arithmetic;
--        function Rotate_Left  (Value : u64; Amount : Natural)
--                               return u64; -- renames Interfaces.Rotate_Left;
--        function Rotate_Right (Value : u64; Amount : Natural)
--                               return u64; -- renames Interfaces.Rotate_Right;

--     pragma Import (Intrinsic, Shift_Left);
--     pragma Import (Intrinsic, Shift_Right);
--     pragma Import (Intrinsic, Shift_Right_Arithmetic);
--     pragma Import (Intrinsic, Rotate_Left);
--     pragma Import (Intrinsic, Rotate_Right);

   type u1_Array is array (Integer range <>) of u1;
   type u4_Array is array (Integer range <>) of u4;
   type u8_Array is array (Integer range <>) of u8;
   type u16_Array is array (Integer range <>) of u16;
   type u32_Array is array (Integer range <>) of u32;
   type u64_Array is array (Integer range <>) of u64;
   subtype Bit_Array is u1_Array;
   subtype Nibble_Array is u4_Array;

   subtype Octet_Index_32_Bit is Integer'Base range 1 .. 32 / 8;
   subtype Octet_Index_48_Bit is Integer'Base range 1 .. 48 / 8;
   subtype Octet_Index_56_Bit is Integer'Base range 1 .. 56 / 8;
   subtype Octet_Index_64_Bit is Integer'Base range 1 .. 64 / 8;
   subtype Octet_Index_96_Bit is Integer'Base range 1 .. 96 / 8;
   subtype Octet_Index_128_Bit is Integer'Base range 1 .. 128 / 8;
   subtype Octet_Index_160_Bit is Integer'Base range 1 .. 160 / 8;
   subtype Octet_Index_168_Bit is Integer'Base range 1 .. 168 / 8;
   subtype Octet_Index_192_Bit is Integer'Base range 1 .. 192 / 8;
   subtype Octet_Index_224_Bit is Integer'Base range 1 .. 224 / 8;
   subtype Octet_Index_256_Bit is Integer'Base range 1 .. 256 / 8;
   subtype Octet_Index_384_Bit is Integer'Base range 1 .. 384 / 8;
   subtype Octet_Index_512_Bit is Integer'Base range 1 .. 512 / 8;
   subtype Octet_Index_768_Bit is Integer'Base range 1 .. 768 / 8;
   subtype Octet_Index_1024_Bit is Integer'Base range 1 .. 1024 / 8;
   subtype Octet_Index_1536_Bit is Integer'Base range 1 .. 1536 / 8;
   subtype Octet_Index_2048_Bit is Integer'Base range 1 .. 2048 / 8;
   subtype Octet_Index_4096_Bit is Integer'Base range 1 .. 4096 / 8;
   subtype Octet_Index_8192_Bit is Integer'Base range 1 .. 8192 / 8;


   subtype Block_32_Bit is u8_Array (Octet_Index_32_Bit);
   subtype Block_48_Bit is u8_Array (Octet_Index_48_Bit);
   subtype Block_56_Bit is u8_Array (Octet_Index_56_Bit);
   subtype Block_64_Bit is u8_Array (Octet_Index_64_Bit);
   subtype Block_96_Bit is u8_Array (Octet_Index_96_Bit);
   subtype Block_128_Bit is u8_Array (Octet_Index_128_Bit);
   subtype Block_160_Bit is u8_Array (Octet_Index_160_Bit);
   subtype Block_168_Bit is u8_Array (Octet_Index_168_Bit);
   subtype Block_192_Bit is u8_Array (Octet_Index_192_Bit);
   subtype Block_224_Bit is u8_Array (Octet_Index_224_Bit);
   subtype Block_256_Bit is u8_Array (Octet_Index_256_Bit);
   subtype Block_384_Bit is u8_Array (Octet_Index_384_Bit);
   subtype Block_512_Bit is u8_Array (Octet_Index_512_Bit);
   subtype Block_768_Bit is u8_Array (Octet_Index_768_Bit);
   subtype Block_1024_Bit is u8_Array (Octet_Index_1024_Bit);
   subtype Block_1536_Bit is u8_Array (Octet_Index_1536_Bit);
   subtype Block_2048_Bit is u8_Array (Octet_Index_2048_Bit);
   subtype Block_4096_Bit is u8_Array (Octet_Index_4096_Bit);
   subtype Block_8192_Bit is u8_Array (Octet_Index_8192_Bit);

   Wrong_Opertaion_Order : exception;
   Format_Violation : exception;
   Invalid_Key_Size : exception;

   function To_Hex
     (A          : u8_Array;
      Upper_Case : Boolean := False;
      Spacing    : Natural := 0) return String;
   function From_Hex (S : String) return u8_Array;
   function From_Ascii (S : String) return u8_Array;

end Crypto.Types;
