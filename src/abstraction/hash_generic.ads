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

   Name_Intern : String;

   type Context_T_Intern is private;

   Block_Size_Bits_Intern  : Natural;
   Digest_Size_Bits_Intern : Natural;

   with procedure Initialize_Intern(Context : out Context_T_Intern);
   with procedure Next_Block_Intern(Context : in out Context_T_Intern; Block : in u8_Array);
   with procedure Last_Block_Intern(Context : in out Context_T_Intern; Block : in u8_Array; Bits : in Integer := -1);
   with procedure Get_Digest_Intern(Context : in out Context_T_Intern; Digest : out u8_Array);

package Hash_Generic is

   Name : String renames Name_Intern;

   subtype Context_T is Context_T_Intern;

   Block_Size_Bits  : Natural renames Block_Size_Bits_Intern;
   Digest_Size_Bits : Natural renames Digest_Size_Bits_Intern;

   Block_Size_Bytes  : Natural := (Block_Size_Bits) / 8;
   Digest_Size_Bytes : Natural := (Digest_Size_Bits) / 8;

   procedure Initialize(Context : out Context_T_Intern) renames Initialize_Intern;
   procedure Next_Block(Context : in out Context_T_Intern; Block : in u8_Array) renames Next_Block_Intern;
   procedure Last_Block(Context : in out Context_T_Intern; Block : in u8_Array; Bits : in Integer := -1) renames Last_Block_Intern;
   procedure Get_Digest(Context : in out Context_T_Intern; Digest : out u8_Array) renames Get_Digest_Intern;

   procedure Hash(Data : in u8_Array; Digest : out u8_Array; Bits : in Integer := -1);

private

end Hash_Generic;
