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

with Crypto_Core_Types; use Crypto_Core_Types;

generic
   Name : String;
   Digest_Size_Bits : Natural;
   Block_Size_Bits : Natural;
   type Context_T is private;
   with procedure Initialize(Context : out Context_T);
   with procedure Next_Block(Context : in out Context_T; Block : in u8_Array);
   with procedure Last_Block(Context : in out Context_T; Block : in u8_Array; Bits : in Integer := -1);
   with procedure Get_Digest(Context : in out Context_T; Digest : out u8_Array);
   with procedure Hash(Data : in u8_Array; Digest : out u8_Array; Bits : in Integer := -1);

package Nessie_Hash_Test_Generator is

   Verbose : Boolean := True;
   procedure Run(FileName : String := "");
   procedure Run_File;

   Default_Suffix : constant String := ".test-vectors";

private

   Digest_Size_Bytes : constant Natural := (Digest_Size_Bits + 7) / 8;
   Block_Size_Bytes : constant Natural := (Block_Size_Bits + 7) / 8;

end Nessie_Hash_Test_Generator;
