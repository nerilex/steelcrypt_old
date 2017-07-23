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

with Crypto_Core_Types; use Crypto_Core_Types;
with Crypto_Types;      use Crypto_Types;
use Crypto_Types.Crypto_Utils_u8;

with Ada.Unchecked_Deallocation;
with AES; use AES;

with Xml_Kat_Reader;
with Schema.Validators;     use Schema.Validators;
with Schema.Schema_Readers; use Schema.Schema_Readers;
with Input_Sources.File;    use Input_Sources.File;
with Ada.Text_IO;        use Ada.Text_IO;
with Ada.Command_line;   use Ada.Command_Line;
with Sax.Readers;        use Sax.Readers;



procedure Test_AES_XML is
   type AES_ECB_KAT_Entrys_T is (key, ciphertext, plaintext, index);

   type u8_Array_Access is access all u8_Array;

   type AES_ECB_KAT_T is record
      index : natural;
      ciphertext : u8_Array_Access;
      plaintext : u8_Array_Access;
      key : u8_Array_Access;
   end record;

   procedure Set(Kat : in out AES_ECB_KAT_T; KAT_Entry : AES_ECB_KAT_Entrys_T; Value : String) is
--      Tmp : aliased u8_Array(1 .. (Value'Length + 1) / 2);
   begin
      case KAT_Entry is
         when index => Kat.index := Natural'Value(Value);
         when ciphertext =>
--            Tmp := From_Hex(Value);
            Kat.ciphertext := new u8_Array(1 .. (Value'Length + 1) / 2);
            Kat.ciphertext.all := From_Hex(Value);
         when plaintext =>
--            Tmp := From_Hex(Value);
            Kat.plaintext := new u8_Array(1 .. (Value'Length + 1) / 2);
            Kat.plaintext.all := From_Hex(Value);
         when key =>
--            Tmp := From_Hex(Value);
            Kat.key := new u8_Array(1 .. (Value'Length + 1) / 2);
            Kat.key.all := From_Hex(Value);
      end case;
   end Set;

   function Test(Kat : AES_ECB_KAT_T) return Boolean is
      package Cipher renames AES.AES_128;
      Ctx : Cipher.Context_T;
      Tmp : u8_Array := Kat.plaintext.all;
   begin
--        Put_Line("== Test ==");
--        Put_Line("  key:        " & To_Hex(Kat.key.all));
--        Put_Line("  plaintext:  " & To_Hex(Kat.plaintext.all));
--        Put_Line("  ciphertext: " & To_Hex(Kat.ciphertext.all));
      Cipher.Initialize(Ctx, Kat.key.all);
      Cipher.Encrypt(Ctx, Tmp);
      if Tmp /= Kat.ciphertext.all then
         return False;
      end if;
      Cipher.Decrypt(Ctx, Tmp);
      if Tmp /= Kat.plaintext.all then
         return False;
      end if;
      return True;
   end Test;

   function Create(index : Natural) return AES_ECB_KAT_T is
      Kat : AES_ECB_KAT_T;
   begin
      Kat.index := index;
      return Kat;
   end Create;

   procedure Free_u8_Array is new Ada.Unchecked_Deallocation
     (      Object => u8_Array,
            Name => u8_Array_Access );

   procedure Destroy(Kat : in out AES_ECB_KAT_T) is
   begin
      Free_u8_Array(Kat.ciphertext);
      Free_u8_Array(Kat.plaintext);
      Free_u8_Array(Kat.key);
   end Destroy;

   package Aes_Xml_Kat_Reader is new Xml_Kat_Reader(KAT_Type_Name => "kat_vector_without_iv",
                                                    KAT_T        => AES_ECB_KAT_T,
                                                    KAT_Entrys_T => AES_ECB_KAT_Entrys_T,
                                                    Set          => Set,
                                                    Test         => Test,
                                                    Create       => Create,
                                                    Destroy      => Destroy);
   TV_File : File_Input;
   Schema_File : File_Input;
   Kat_Schema_Reader : Schema_Reader;
   Xml_Reader : Aes_Xml_Kat_Reader.Reader;
begin
   Open ("testvectors/xml/schema/block-cipher_kat.xsd", Schema_File);
   Kat_Schema_Reader.Parse(Schema_File);
   Schema_File.Close;

   Xml_Reader.Set_Grammar(Kat_Schema_Reader.Get_Grammar);
   Xml_Reader.Set_Feature(Schema_Validation_Feature, True);

   for Arg in 1 .. Argument_Count loop
      New_Line;
      Put_Line("Testing with " & Argument(Arg));
      Set_Public_Id (TV_File, Argument(Arg));
      Open (Argument(Arg), TV_File);
      Xml_Reader.Parse(Input => TV_File);
      Close (TV_File);
   end loop;

exception
   when Schema.Validators.XML_Validation_Error =>
      Put_Line ("ERROR: " & Get_Error_Message (Kat_Schema_Reader));
end Test_AES_XML;