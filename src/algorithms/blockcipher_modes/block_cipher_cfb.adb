--  Copyright (C) 2017  bg nerilex <bg@nerilex.org>
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

with Crypto_Types; use Crypto_Types;
use Crypto_Types.Crypto_Utils_u8;

package body Block_Cipher_CFB is

   procedure Initialize
     (Context :    out Context_T;
      Key     : in     u8_Array) is
   begin
      Cipher.Initialize(Context => Context.Cipher_Context, Key => Key);
   end Initialize;


   procedure Set_Initialization_Vector
     (Context : in out Context_T;
      IV      : in     Initialization_Vector_T) is
   begin
      Context.Block := IV;
      Context.Buffer_Fill := Width_Bits;
   end Set_Initialization_Vector;

   procedure Encrypt
     (Context : in out Context_T;
      Block   : in out u8_Array) is
      t : u8;
   begin
      for i in Block'Range loop
         if Context.Buffer_Fill = Width_Bits then
            Cipher.Encrypt(Context => Context.Cipher_Context,
                           Block => Context.Block);
            Context.Block := Rotate_Array_Left(Context.Block, Width_Bits);
            Context.Buffer_Fill := 0;
         end if;
         t := Context.Block(Context.Block'First + Context.Buffer_Fill / 8) xor Block(i);
         Context.Block(Context.Block'First + Context.Buffer_Fill / 8) := t;
         Block(i) := t;
         Context.Buffer_Fill := Context.Buffer_Fill + 8;
      end loop;
   end Encrypt;

   procedure Decrypt
     (Context : in out Context_T;
      Block   : in out u8_Array) is
      t : u8;
   begin
      for i in Block'Range loop
         if Context.Buffer_Fill = Width_Bits then
            Cipher.Encrypt(Context => Context.Cipher_Context,
                           Block => Context.Block);
            Context.Block := Rotate_Array_Left(Context.Block, Width_Bits);
            Context.Buffer_Fill := 0;
         end if;
         t := Context.Block(Context.Block'First + Context.Buffer_Fill / 8) xor Block(i);
         Context.Block(Context.Block'First + Context.Buffer_Fill / 8) := Block(i);
         Block(i) := t;
         Context.Buffer_Fill := Context.Buffer_Fill + 8;
      end loop;
   end Decrypt;

   procedure Encrypt
     (Context : in out Context_T;
      Block   : in out Bit_Array) is null;


   procedure Decrypt
     (Context : in out Context_T;
      Block   : in out Bit_Array) is null;

end Block_Cipher_CFB;
