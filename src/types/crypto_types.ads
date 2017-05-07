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

with Crypto_Generic_Utils;
with Crypto_Generic_Block_Utils;
with Crypto_Core_Types; use Crypto_Core_Types;
with System;

with Ada.Direct_IO;
with Ada.Sequential_IO;

package Crypto_Types is

   subtype Bit_Order is System.Bit_Order;

   package Crypto_Utils_u8 is new Crypto_Generic_Utils
     (T       => u8,
      T_Array => u8_Array);
   package Crypto_Utils_u16 is new Crypto_Generic_Utils
     (T       => u16,
      T_Array => u16_Array);
   package Crypto_Utils_u32 is new Crypto_Generic_Utils
     (T       => u32,
      T_Array => u32_Array);
   package Crypto_Utils_u64 is new Crypto_Generic_Utils
     (T       => u64,
      T_Array => u64_Array);

   package Crypto_Utils_32u8 is new Crypto_Generic_Block_Utils
     (T       => u8,
      T_Array => Block_32_Bit,
      T_Array_Index => Octet_Index_32_Bit);

   package Crypto_Utils_48u8 is new Crypto_Generic_Block_Utils
     (T       => u8,
      T_Array => Block_48_Bit,
      T_Array_Index => Octet_Index_48_Bit);
   package Crypto_Utils_56u8 is new Crypto_Generic_Block_Utils
     (T       => u8,
      T_Array => Block_56_Bit,
      T_Array_Index => Octet_Index_56_Bit);
   package Crypto_Utils_64u8 is new Crypto_Generic_Block_Utils
     (T       => u8,
      T_Array => Block_64_Bit,
      T_Array_Index => Octet_Index_64_Bit);
   package Crypto_Utils_96u8 is new Crypto_Generic_Block_Utils
     (T       => u8,
      T_Array => Block_96_Bit,
      T_Array_Index => Octet_Index_96_Bit);
   package Crypto_Utils_128u8 is new Crypto_Generic_Block_Utils
     (T       => u8,
      T_Array => Block_128_Bit,
      T_Array_Index => Octet_Index_128_Bit);
   package Crypto_Utils_160u8 is new Crypto_Generic_Block_Utils
     (T       => u8,
      T_Array => Block_160_Bit,
      T_Array_Index => Octet_Index_160_Bit);
   package Crypto_Utils_168u8 is new Crypto_Generic_Block_Utils
     (T       => u8,
      T_Array => Block_168_Bit,
      T_Array_Index => Octet_Index_168_Bit);
   package Crypto_Utils_192u8 is new Crypto_Generic_Block_Utils
     (T       => u8,
      T_Array => Block_192_Bit,
      T_Array_Index => Octet_Index_192_Bit);
   package Crypto_Utils_224u8 is new Crypto_Generic_Block_Utils
     (T       => u8,
      T_Array => Block_224_Bit,
      T_Array_Index => Octet_Index_224_Bit);
   package Crypto_Utils_256u8 is new Crypto_Generic_Block_Utils
     (T       => u8,
      T_Array => Block_256_Bit,
      T_Array_Index => Octet_Index_256_Bit);
   package Crypto_Utils_384u8 is new Crypto_Generic_Block_Utils
     (T       => u8,
      T_Array => Block_384_Bit,
      T_Array_Index => Octet_Index_384_Bit);
   package Crypto_Utils_512u8 is new Crypto_Generic_Block_Utils
     (T       => u8,
      T_Array => Block_512_Bit,
      T_Array_Index => Octet_Index_512_Bit);
   package Crypto_Utils_768u8 is new Crypto_Generic_Block_Utils
     (T       => u8,
      T_Array => Block_768_Bit,
      T_Array_Index => Octet_Index_768_Bit);
   package Crypto_Utils_1024u8 is new Crypto_Generic_Block_Utils
     (T       => u8,
      T_Array => Block_1024_Bit,
      T_Array_Index => Octet_Index_1024_Bit);
   package Crypto_Utils_1536u8 is new Crypto_Generic_Block_Utils
     (T       => u8,
      T_Array => Block_1536_Bit,
      T_Array_Index => Octet_Index_1536_Bit);
   package Crypto_Utils_2048u8 is new Crypto_Generic_Block_Utils
     (T       => u8,
      T_Array => Block_2048_Bit,
      T_Array_Index => Octet_Index_2048_Bit);
   package Crypto_Utils_4096u8 is new Crypto_Generic_Block_Utils
     (T       => u8,
      T_Array => Block_4096_Bit,
      T_Array_Index => Octet_Index_4096_Bit);
   package Crypto_Utils_8192u8 is new Crypto_Generic_Block_Utils
     (T       => u8,
      T_Array => Block_8192_Bit,
      T_Array_Index => Octet_Index_8192_Bit);

   package u8_Direct_IO is new Ada.Direct_IO (u8);
   package u16_Direct_IO is new Ada.Direct_IO (u16);
   package u32_Direct_IO is new Ada.Direct_IO (u32);
   package u64_Direct_IO is new Ada.Direct_IO (u64);

   package u8_Sequential_IO is new Ada.Sequential_IO (u8);
   package u16_Sequential_IO is new Ada.Sequential_IO (u16);
   package u32_Sequential_IO is new Ada.Sequential_IO (u32);
   package u64_Sequential_IO is new Ada.Sequential_IO (u64);

   use Crypto_Core_Types;
   use Crypto_Utils_u8;
   use Crypto_Utils_u16;
   use Crypto_Utils_u32;
   use Crypto_Utils_u64;
   use Crypto_Utils_32u8;
   use Crypto_Utils_48u8;
   use Crypto_Utils_56u8;
   use Crypto_Utils_64u8;
   use Crypto_Utils_96u8;
   use Crypto_Utils_128u8;
   use Crypto_Utils_160u8;
   use Crypto_Utils_168u8;
   use Crypto_Utils_192u8;
   use Crypto_Utils_224u8;
   use Crypto_Utils_256u8;
   use Crypto_Utils_384u8;
   use Crypto_Utils_512u8;
   use Crypto_Utils_768u8;
   use Crypto_Utils_1024u8;
   use Crypto_Utils_1536u8;
   use Crypto_Utils_2048u8;
   use Crypto_Utils_4096u8;
   use Crypto_Utils_8192u8;

   use u8_Direct_IO;
   use u16_Direct_IO;
   use u32_Direct_IO;
   use u64_Direct_IO;

   use u8_Sequential_IO;
   use u16_Sequential_IO;
   use u32_Sequential_IO;
   use u64_Sequential_IO;

end Crypto_Types;
