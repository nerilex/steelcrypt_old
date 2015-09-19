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

package Pi16Cipher_Spec is

   type Context_T is private;

   Cipher_Name : constant String := "pi16cipher";
   Width_Bits : constant := 16;

   N : constant := 4;
   R : constant := 3;

   IS_Bits : constant := (N * 4 * Width_Bits);
   IS_Bytes : constant := IS_Bits / 8;
   Rate_Bits : constant := IS_Bits / 2;
   Rate_Bytes : constant := Rate_Bits / 8;
   Capacity_Bits : constant := IS_Bits - Rate_Bits;
   Capacity_Bytes : constant := Capacity_Bits / 8;
   Block_Bits : constant := Rate_Bits;
   Block_Bytes : constant := Block_Bits / 8;
   Tag_Bits : constant := Rate_Bits;
   Tag_Bytes : constant := Tag_Bits / 8;
   Secret_Message_Number_Bits : constant := Block_Bits;
   Secret_Message_Number_Bytes : constant := Secret_Message_Number_Bits / 8;

   subtype Block_Number_T is Natural;

   subtype Block_T is u8_Array (1 .. Rate_Bytes);

   procedure Initialize(Context : out Context_T; Key : in u8_Array; Public_Nonce : in u8_Array);
--     procedure Encrypt_Secret_Message_Number(Context : in out Context; Secret_Message_Number : in u8_Array);
--     procedure Header_Next_Block(Context : in out Context_T; Header : in u8_Array);
--     procedure Header_Last_Block(Context : in out Context_T; Header : in u8_Array);
--     procedure Encrypt_Next_Block(Context : in out Context_T; Block : in out u8_Array);
--     procedure Encrypt_Last_Block(Context : in out Context_T; Block : in out u8_Array);
--     procedure Decrypt_Next_Block(Context : in out Context_T; Block : in out u8_Array);
--     procedure Decrypt_Last_Block(Context : in out Context_T; Block : in out u8_Array);
--     procedure Get_Tag(Context : in Context_T; Tag : out u8_Array);
--     function Is_Valid(Context : in Context_T; Tag : in u8_Array) return Boolean;

   function Encrypt(Msg : u8_Array; AD : u8_Array; Public_Nonce : u8_Array; Secret_Nonce : Block_T; Key : u8_Array) return u8_Array;
   procedure Decrypt(Is_Valid : out Boolean; Msg : out u8_Array; Secret_Nonce : out Block_T; Cipher : in u8_Array; AD : in u8_Array; Public_Nonce : in u8_Array; Key : in u8_Array);


private

   subtype Word_T is u16;
   subtype Chunk_T is u16_Array(1 .. 4);
   subtype Tag_Int_T is u16_Array(1 .. 4 * N / 2);
   subtype Tag_T is u8_Array(1 .. Rate_Bytes);

   type State_T is array (1 .. N) of Chunk_T;

   type Context_T is record
      State : State_T;
      Tag : Tag_Int_T;
      Counter : u64;
   end record;

end Pi16Cipher_Spec;
