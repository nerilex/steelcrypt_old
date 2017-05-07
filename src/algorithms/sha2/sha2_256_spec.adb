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

with Crypto_Types;

package body SHA2_256_Spec is

   use Crypto_Types.Crypto_Utils_u32;

   IV : constant u32_Array (1 .. 8) :=
     (16#6A09E667#,
      16#BB67AE85#,
      16#3C6EF372#,
      16#A54FF53A#,
      16#510E527F#,
      16#9B05688C#,
      16#1F83D9AB#,
      16#5BE0CD19#);

   procedure Initialize (Context : out Context_T) is
   begin
      Context.H       := IV;
      Context.counter := 0;
   end Initialize;

   procedure Next_Block
     (Context : in out Context_T;
      Block   : in     Block_512_Bit)
   is
   begin
      SHA2_Small.Next_Block (SHA2_Small.Context_T (Context), Block);
   end Next_Block;

   procedure Last_Block
     (Context : in out Context_T;
      Block   : in     u8_Array;
      Bits    : in     Integer := -1)
   is
   begin
      SHA2_Small.Last_Block (SHA2_Small.Context_T (Context), Block, Bits);
   end Last_Block;

   procedure Get_Digest
     (Context : in out Context_T;
      Digest  :    out Block_256_Bit)
   is
   begin
      Store_be (Digest, Context.H);
   end Get_Digest;

end SHA2_256_Spec;
