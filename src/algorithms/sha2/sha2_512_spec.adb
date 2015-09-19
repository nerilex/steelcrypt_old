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

package body SHA2_512_Spec is

   IV : constant u64_Array(1 .. 8) :=
     (
      16#6a09e667f3bcc908#, 16#bb67ae8584caa73b#, 16#3c6ef372fe94f82b#, 16#a54ff53a5f1d36f1#,
      16#510e527fade682d1#, 16#9b05688c2b3e6c1f#, 16#1f83d9abfb41bd6b#, 16#5be0cd19137e2179#
     );

   procedure Initialize(Context : out Context_T) is
   begin
      Context.H := IV;
      Context.Counter := 0;
   end;

   procedure Next_Block(Context : in out Context_T; Block : in Block_1024_Bit) is
   begin
      Sha2_Large.Next_Block(Sha2_Large.Context_T(Context), Block);
   end;

   procedure Last_Block(Context : in out Context_T; Block : in u8_Array; Bits : in Integer := -1) is
   begin
      Sha2_Large.Last_Block(Sha2_Large.Context_T(Context), Block, Bits);
   end;

   procedure Get_Digest(Context : in out Context_T; Digest : out Block_512_Bit) is
   begin
      Store_BE(Digest, Context.H);
   end;

end SHA2_512_Spec;
