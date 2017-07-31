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

with Crypto.Types.Generic_Utils;
with Crypto.Types.Generic_Block_Utils;
with Crypto.Types; use Crypto.Types;
with System;

with Ada.Direct_IO;
with Ada.Sequential_IO;

package Crypto.Types.X is

   use type Crypto.Types.u1;
   package Utils_u1 is new Crypto.Types.Generic_Utils
     (T       => u1,
      T_Array => u1_Array
,
    Shift_Left   => Crypto.Types.Shift_Left,
    Shift_Right  => Crypto.Types.Shift_Right,
    Rotate_Left  => Crypto.Types.Rotate_Left,
    Rotate_Right => Crypto.Types.Rotate_Right
     );

   use type Crypto.Types.u4;
   package Utils_u4 is new Crypto.Types.Generic_Utils
     (T       => u4,
      T_Array => u4_Array
,
    Shift_Left   => Crypto.Types.Shift_Left,
    Shift_Right  => Crypto.Types.Shift_Right,
    Rotate_Left  => Crypto.Types.Rotate_Left,
    Rotate_Right => Crypto.Types.Rotate_Right
     );

   package Utils_u8 is new Crypto.Types.Generic_Utils
     (T       => u8,
      T_Array => u8_Array
      ,
      Shift_Left   => Crypto.Types.Shift_Left,
      Shift_Right  => Crypto.Types.Shift_Right,
      Rotate_Left  => Crypto.Types.Rotate_Left,
      Rotate_Right => Crypto.Types.Rotate_Right
     );

   package Utils_u16 is new Crypto.Types.Generic_Utils
     (T       => u16,
      T_Array => u16_Array
 ,
      Shift_Left   => Crypto.Types.Shift_Left,
      Shift_Right  => Crypto.Types.Shift_Right,
      Rotate_Left  => Crypto.Types.Rotate_Left,
      Rotate_Right => Crypto.Types.Rotate_Right
     );

   package Utils_u32 is new Crypto.Types.Generic_Utils
     (T       => u32,
      T_Array => u32_Array
 ,
      Shift_Left   => Crypto.Types.Shift_Left,
      Shift_Right  => Crypto.Types.Shift_Right,
      Rotate_Left  => Crypto.Types.Rotate_Left,
      Rotate_Right => Crypto.Types.Rotate_Right
     );

   package Utils_u64 is new Crypto.Types.Generic_Utils
     (T       => u64,
      T_Array => u64_Array
 ,
      Shift_Left   => Crypto.Types.Shift_Left,
      Shift_Right  => Crypto.Types.Shift_Right,
      Rotate_Left  => Crypto.Types.Rotate_Left,
      Rotate_Right => Crypto.Types.Rotate_Right
     );

   package Utils_32u8 is new Crypto.Types.Generic_Block_Utils
     (T       => u8,
      T_Array => Block_32_Bit,
      T_Array_Index => Octet_Index_32_Bit
 ,
      Shift_Left   => Crypto.Types.Shift_Left,
      Shift_Right  => Crypto.Types.Shift_Right,
      Rotate_Left  => Crypto.Types.Rotate_Left,
      Rotate_Right => Crypto.Types.Rotate_Right
     );

   package Utils_48u8 is new Crypto.Types.Generic_Block_Utils
     (T       => u8,
      T_Array => Block_48_Bit,
      T_Array_Index => Octet_Index_48_Bit
 ,
      Shift_Left   => Crypto.Types.Shift_Left,
      Shift_Right  => Crypto.Types.Shift_Right,
      Rotate_Left  => Crypto.Types.Rotate_Left,
      Rotate_Right => Crypto.Types.Rotate_Right
     );
   package Utils_56u8 is new Crypto.Types.Generic_Block_Utils
     (T       => u8,
      T_Array => Block_56_Bit,
      T_Array_Index => Octet_Index_56_Bit
 ,
      Shift_Left   => Crypto.Types.Shift_Left,
      Shift_Right  => Crypto.Types.Shift_Right,
      Rotate_Left  => Crypto.Types.Rotate_Left,
      Rotate_Right => Crypto.Types.Rotate_Right
     );
   package Utils_64u8 is new Crypto.Types.Generic_Block_Utils
     (T       => u8,
      T_Array => Block_64_Bit,
      T_Array_Index => Octet_Index_64_Bit
 ,
      Shift_Left   => Crypto.Types.Shift_Left,
      Shift_Right  => Crypto.Types.Shift_Right,
      Rotate_Left  => Crypto.Types.Rotate_Left,
      Rotate_Right => Crypto.Types.Rotate_Right
     );
   package Utils_96u8 is new Crypto.Types.Generic_Block_Utils
     (T       => u8,
      T_Array => Block_96_Bit,
      T_Array_Index => Octet_Index_96_Bit
 ,
      Shift_Left   => Crypto.Types.Shift_Left,
      Shift_Right  => Crypto.Types.Shift_Right,
      Rotate_Left  => Crypto.Types.Rotate_Left,
      Rotate_Right => Crypto.Types.Rotate_Right
     );
   package Utils_128u8 is new Crypto.Types.Generic_Block_Utils
     (T       => u8,
      T_Array => Block_128_Bit,
      T_Array_Index => Octet_Index_128_Bit
 ,
      Shift_Left   => Crypto.Types.Shift_Left,
      Shift_Right  => Crypto.Types.Shift_Right,
      Rotate_Left  => Crypto.Types.Rotate_Left,
      Rotate_Right => Crypto.Types.Rotate_Right
     );
   package Utils_160u8 is new Crypto.Types.Generic_Block_Utils
     (T       => u8,
      T_Array => Block_160_Bit,
      T_Array_Index => Octet_Index_160_Bit
 ,
      Shift_Left   => Crypto.Types.Shift_Left,
      Shift_Right  => Crypto.Types.Shift_Right,
      Rotate_Left  => Crypto.Types.Rotate_Left,
      Rotate_Right => Crypto.Types.Rotate_Right
     );
   package Utils_168u8 is new Crypto.Types.Generic_Block_Utils
     (T       => u8,
      T_Array => Block_168_Bit,
      T_Array_Index => Octet_Index_168_Bit
 ,
      Shift_Left   => Crypto.Types.Shift_Left,
      Shift_Right  => Crypto.Types.Shift_Right,
      Rotate_Left  => Crypto.Types.Rotate_Left,
      Rotate_Right => Crypto.Types.Rotate_Right
     );
   package Utils_192u8 is new Crypto.Types.Generic_Block_Utils
     (T       => u8,
      T_Array => Block_192_Bit,
      T_Array_Index => Octet_Index_192_Bit
 ,
      Shift_Left   => Crypto.Types.Shift_Left,
      Shift_Right  => Crypto.Types.Shift_Right,
      Rotate_Left  => Crypto.Types.Rotate_Left,
      Rotate_Right => Crypto.Types.Rotate_Right
     );
   package Utils_224u8 is new Crypto.Types.Generic_Block_Utils
     (T       => u8,
      T_Array => Block_224_Bit,
      T_Array_Index => Octet_Index_224_Bit
 ,
      Shift_Left   => Crypto.Types.Shift_Left,
      Shift_Right  => Crypto.Types.Shift_Right,
      Rotate_Left  => Crypto.Types.Rotate_Left,
      Rotate_Right => Crypto.Types.Rotate_Right
     );
   package Utils_256u8 is new Crypto.Types.Generic_Block_Utils
     (T       => u8,
      T_Array => Block_256_Bit,
      T_Array_Index => Octet_Index_256_Bit
 ,
      Shift_Left   => Crypto.Types.Shift_Left,
      Shift_Right  => Crypto.Types.Shift_Right,
      Rotate_Left  => Crypto.Types.Rotate_Left,
      Rotate_Right => Crypto.Types.Rotate_Right
     );
   package Utils_384u8 is new Crypto.Types.Generic_Block_Utils
     (T       => u8,
      T_Array => Block_384_Bit,
      T_Array_Index => Octet_Index_384_Bit
 ,
      Shift_Left   => Crypto.Types.Shift_Left,
      Shift_Right  => Crypto.Types.Shift_Right,
      Rotate_Left  => Crypto.Types.Rotate_Left,
      Rotate_Right => Crypto.Types.Rotate_Right
     );
   package Utils_512u8 is new Crypto.Types.Generic_Block_Utils
     (T       => u8,
      T_Array => Block_512_Bit,
      T_Array_Index => Octet_Index_512_Bit
 ,
      Shift_Left   => Crypto.Types.Shift_Left,
      Shift_Right  => Crypto.Types.Shift_Right,
      Rotate_Left  => Crypto.Types.Rotate_Left,
      Rotate_Right => Crypto.Types.Rotate_Right
     );
   package Utils_768u8 is new Crypto.Types.Generic_Block_Utils
     (T       => u8,
      T_Array => Block_768_Bit,
      T_Array_Index => Octet_Index_768_Bit
 ,
      Shift_Left   => Crypto.Types.Shift_Left,
      Shift_Right  => Crypto.Types.Shift_Right,
      Rotate_Left  => Crypto.Types.Rotate_Left,
      Rotate_Right => Crypto.Types.Rotate_Right
     );
   package Utils_1024u8 is new Crypto.Types.Generic_Block_Utils
     (T       => u8,
      T_Array => Block_1024_Bit,
      T_Array_Index => Octet_Index_1024_Bit
 ,
      Shift_Left   => Crypto.Types.Shift_Left,
      Shift_Right  => Crypto.Types.Shift_Right,
      Rotate_Left  => Crypto.Types.Rotate_Left,
      Rotate_Right => Crypto.Types.Rotate_Right
     );
   package Utils_1536u8 is new Crypto.Types.Generic_Block_Utils
     (T       => u8,
      T_Array => Block_1536_Bit,
      T_Array_Index => Octet_Index_1536_Bit
 ,
      Shift_Left   => Crypto.Types.Shift_Left,
      Shift_Right  => Crypto.Types.Shift_Right,
      Rotate_Left  => Crypto.Types.Rotate_Left,
      Rotate_Right => Crypto.Types.Rotate_Right
     );
   package Utils_2048u8 is new Crypto.Types.Generic_Block_Utils
     (T       => u8,
      T_Array => Block_2048_Bit,
      T_Array_Index => Octet_Index_2048_Bit
 ,
      Shift_Left   => Crypto.Types.Shift_Left,
      Shift_Right  => Crypto.Types.Shift_Right,
      Rotate_Left  => Crypto.Types.Rotate_Left,
      Rotate_Right => Crypto.Types.Rotate_Right
     );
   package Utils_4096u8 is new Crypto.Types.Generic_Block_Utils
     (T       => u8,
      T_Array => Block_4096_Bit,
      T_Array_Index => Octet_Index_4096_Bit
 ,
      Shift_Left   => Crypto.Types.Shift_Left,
      Shift_Right  => Crypto.Types.Shift_Right,
      Rotate_Left  => Crypto.Types.Rotate_Left,
      Rotate_Right => Crypto.Types.Rotate_Right
     );
   package Utils_8192u8 is new Crypto.Types.Generic_Block_Utils
     (T       => u8,
      T_Array => Block_8192_Bit,
      T_Array_Index => Octet_Index_8192_Bit
 ,
      Shift_Left   => Crypto.Types.Shift_Left,
      Shift_Right  => Crypto.Types.Shift_Right,
      Rotate_Left  => Crypto.Types.Rotate_Left,
      Rotate_Right => Crypto.Types.Rotate_Right
     );

   package u8_Direct_IO is new Ada.Direct_IO (u8);
   package u16_Direct_IO is new Ada.Direct_IO (u16);
   package u32_Direct_IO is new Ada.Direct_IO (u32);
   package u64_Direct_IO is new Ada.Direct_IO (u64);

   package u8_Sequential_IO is new Ada.Sequential_IO (u8);
   package u16_Sequential_IO is new Ada.Sequential_IO (u16);
   package u32_Sequential_IO is new Ada.Sequential_IO (u32);
   package u64_Sequential_IO is new Ada.Sequential_IO (u64);

--     use Crypto.Types.X.Utils_u8;
--     use Crypto.Types.Utils_u16;
--     use Crypto.Types.Utils_u32;
--     use Crypto.Types.Utils_u64;
--     use Crypto.Types.Utils_32u8;
--     use Crypto.Types.Utils_48u8;
--     use Crypto.Types.Utils_56u8;
--     use Crypto.Types.Utils_64u8;
--     use Crypto.Types.Utils_96u8;
--     use Crypto.Types.Utils_128u8;
--     use Crypto.Types.Utils_160u8;
--     use Crypto.Types.Utils_168u8;
--     use Crypto.Types.Utils_192u8;
--     use Crypto.Types.Utils_224u8;
--     use Crypto.Types.Utils_256u8;
--     use Crypto.Types.Utils_384u8;
--     use Crypto.Types.Utils_512u8;
--     use Crypto.Types.Utils_768u8;
--     use Crypto.Types.Utils_1024u8;
--     use Crypto.Types.Utils_1536u8;
--     use Crypto.Types.Utils_2048u8;
--     use Crypto.Types.Utils_4096u8;
--     use Crypto.Types.Utils_8192u8;
--
--     use u8_Direct_IO;
--     use u16_Direct_IO;
--     use u32_Direct_IO;
--     use u64_Direct_IO;
--
--     use u8_Sequential_IO;
--     use u16_Sequential_IO;
--     use u32_Sequential_IO;
--     use u64_Sequential_IO;

end Crypto.Types.X;
