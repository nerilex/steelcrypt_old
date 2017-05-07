--  Copyright (C) 2016  bg nerilex <bg@nerilex.org>
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

use Crypto_Types.Crypto_Utils_u8;
use Crypto_Types.Crypto_Utils_u64;

generic
   type Cell_T is mod <>;
   with function SubCell (a : Cell_T) return Cell_T;
   with function Lfsr_TK2 (a : Cell_T) return Cell_T;
   with function Lfsr_TK3 (a : Cell_T) return Cell_T;
   Rounds1, Rounds2, Rounds3 : Positive;
   Cell_Size_Bits            : Positive;

package Skinny_Generic is

   type Context1_T is private;
   type Context2_T is private;
   type Context3_T is private;
--     subtype Context1_T of Context_T;
--     subtype Context2_T of Context_T;
--     subtype Context3_T of Context_T;

   Block_Size_Bits  : constant Natural := 16 * Cell_Size_Bits;
   Block_Size_Bytes : constant Natural := Block_Size_Bits / 8;

   subtype Block_T is u8_Array (1 .. Block_Size_Bytes);
   subtype Key1_T is u8_Array (1 .. Block_Size_Bytes * 1);
   subtype Key2_T is u8_Array (1 .. Block_Size_Bytes * 2);
   subtype Key3_T is u8_Array (1 .. Block_Size_Bytes * 3);

   procedure Initialize (Context : out Context1_T; Key : in Key1_T);
   procedure Encrypt (Context : in Context1_T; Block : in out Block_T);
   procedure Decrypt (Context : in Context1_T; Block : in out Block_T);

   procedure Initialize (Context : out Context2_T; Key : in Key2_T);
   procedure Encrypt (Context : in Context2_T; Block : in out Block_T);
   procedure Decrypt (Context : in Context2_T; Block : in out Block_T);

   procedure Initialize (Context : out Context3_T; Key : in Key3_T);
   procedure Encrypt (Context : in Context3_T; Block : in out Block_T);
   procedure Decrypt (Context : in Context3_T; Block : in out Block_T);

private

   type Cell_Array_T is array (0 .. 15) of Cell_T;
   type Context_T is array (Positive range <>) of Cell_Array_T;

   type Context1_T is new Context_T (1 .. Rounds1);
   type Context2_T is new Context_T (1 .. Rounds2);
   type Context3_T is new Context_T (1 .. Rounds3);

--     subtype Context1_T is Context_T(Round(1));
--     subtype Context2_T is Context_T(Round(2));
--     subtype Context3_T is Context_T(Round(3));

end Skinny_Generic;
