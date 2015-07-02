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

package body SHA2_384 is

   IV : constant u64_Array(1 .. 8) :=
     (
      16#cbbb9d5dc1059ed8#, 16#629a292a367cd507#, 16#9159015a3070dd17#, 16#152fecd8f70e5939#,
      16#67332667ffc00b31#, 16#8eb44a8768581511#, 16#db0c2e0d64f98fa7#, 16#47b5481dbefa4fa4#
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

   procedure Get_Digest(Context : in out Context_T; Digest : out Block_384_Bit) is
   begin
      Store_BE(Digest, Context.H(1 .. 6));
   end;


   procedure Hash(Data : in u8_Array; Digest : out Block_384_Bit; Bits : in Integer := -1) is
      Context : Context_T;
   begin
      Initialize(Context);
      Last_Block(Context, Data, Bits);
      Get_Digest(Context, Digest);
   end;



end SHA2_384;
