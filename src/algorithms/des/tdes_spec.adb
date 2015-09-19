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

package body TDES_Spec is

   procedure Initialize(Context : out Context_T; Key : in u8_Array) is
   begin
      if Key'Length = 16 then
         DES_Spec.Initialize(Context.Ctx1, Key(Key'First +  0 .. Key'First +  7));
         DES_Spec.Initialize(Context.Ctx2, Key(Key'First +  8 .. Key'First + 15));
         DES_Spec.Initialize(Context.Ctx3, Key(Key'First +  0 .. Key'First +  7));
      elsif Key'Length = 24 then
         DES_Spec.Initialize(Context.Ctx1, Key(Key'First +  0 .. Key'First +  7));
         DES_Spec.Initialize(Context.Ctx2, Key(Key'First +  8 .. Key'First + 15));
         DES_Spec.Initialize(Context.Ctx3, Key(Key'First + 16 .. Key'First + 23));
      else
         raise Invalid_Key_Size;
      end if;
   end;

   procedure Encrypt(Context : in Context_T; Block: in out Block_64_Bit) is
   begin
      DES_Spec.Encrypt(Context.Ctx1, Block);
      DES_Spec.Decrypt(Context.Ctx2, Block);
      DES_Spec.Encrypt(Context.Ctx3, Block);
   end Encrypt;

   procedure Decrypt(Context : in Context_T; Block: in out Block_64_Bit) is
   begin
      DES_Spec.Decrypt(Context.Ctx3, Block);
      DES_Spec.Encrypt(Context.Ctx2, Block);
      DES_Spec.Decrypt(Context.Ctx1, Block);
   end Decrypt;

end TDES_Spec;
