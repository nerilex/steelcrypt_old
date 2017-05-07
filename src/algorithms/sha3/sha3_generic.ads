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

with Crypto_Core_Types; use Crypto_Core_Types;
with Crypto_Types;      use Crypto_Types;
with Keccak_Parameters; use Keccak_Parameters;
with Keccak;            use Keccak;

use Crypto_Types.Crypto_Utils_u8;
use Crypto_Types.Crypto_Utils_u64;

generic
   Capacity_Bits     : Capacity_T;
   Suffix            : u64 := 2#10#;
   Suffix_Bit_Length : u8  := 2;

package Sha3_Generic is

   type Context_T is private;

   Block_Size_Bits   : constant Natural := b - Capacity_Bits;
   Digest_Size_Bits  : constant Natural := Capacity_Bits / 2;
   Block_Size_Bytes  : constant Natural := Block_Size_Bits / 8;
   Digest_Size_Bytes : constant Natural := Digest_Size_Bits / 8;

   subtype Block_T is u8_Array (1 .. Block_Size_Bytes);
   subtype Digest_T is u8_Array (1 .. Digest_Size_Bytes);

   procedure Initialize (Context : out Context_T);
   procedure Next_Block (Context : in out Context_T; Block : in Block_T);
   procedure Last_Block
     (Context : in out Context_T;
      Block   : in     u8_Array;
      Bits    : in     Integer := -1);
   procedure Squeeze (Context : in out Context_T; Data : out u8_Array);
   procedure Get_Digest (Context : in out Context_T; Digest : out Digest_T);

   procedure Hash
     (Data   : in     u8_Array;
      Digest :    out Digest_T;
      Bits   : in     Integer := -1);

private

   Rate_Bits      : constant Capacity_T       := b - Capacity_Bits;
   Capacity_Bytes : constant Capacity_Bytes_T := Capacity_Bits / 8;
   Rate_Bytes     : constant Capacity_Bytes_T := Rate_Bits / 8;
   Digest_Bits    : constant Natural          := Capacity_Bits / 2;
   Digest_Bytes   : constant Natural          := Digest_Bits / 8;

   type Context_T is record
      A : State_T;
   end record;

end Sha3_Generic;
