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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Crypto_Types; use Crypto_Types;
with System; use System;
use Crypto_Types.Crypto_Types_u8;

--  Test vectors -- set 1
--  =====================
--
--  Set 1, vector#  0:
--  message="" (empty string)
--  hash=E3B0C44298FC1C149AFBF4C8996FB924
--  27AE41E4649B934CA495991B7852B855
--
--  Set 1, vector#  1:
--  message="a"
--  hash=CA978112CA1BBDCAFAC231B39A23DC4D
--  A786EFF8147C4E72B9807785AFEE48BB
--
--  Set 1, vector#  2:
--  message="abc"
--  hash=BA7816BF8F01CFEA414140DE5DAE2223
--  B00361A396177A9CB410FF61F20015AD
--
--  Set 1, vector#  3:
--  message="message digest"
--  hash=F7846F55CF23E14EEBEAB5B4E1550CAD
--  5B509E3348FBC4EFA3A1413D393CB650
--
--  Set 1, vector#  4:
--  message="abcdefghijklmnopqrstuvwxyz"
--  hash=71C480DF93D6AE2F1EFAD1447C66C952
--  5E316218CF51FC8D9ED832F2DAF18B73
--
--  Set 1, vector#  5:
--  message="abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
--  hash=248D6A61D20638B8E5C026930C3E6039
--  A33CE45964FF2167F6ECEDD419DB06C1
--
--  Set 1, vector#  6:
--  message="A...Za...z0...9"
--  hash=DB4BFCBD4DA0CD85A60C3C37D3FBD880
--  5C77F15FC6B1FDFE614EE0A7C8FDB4C0
--
--  Set 1, vector#  7:
--  message=8 times "1234567890"
--  hash=F371BC4A311F2B009EEF952DD83CA80E
--  2B60026C8E935592D0F9C308453C813E
--
--  Set 1, vector#  8:
--  message=1 million times "a"
--  hash=CDC76E5C9914FB9281A1C7E284D73E67
--  F1809A48A497200E046D39CCC7112CD0



package body Nessie_Hash_Test_Generator is

   Set_1_Labels : constant array (1 .. 9) of access constant String :=
     (
       new String'("""" & """" & " (empty string)"),
       new String'("""" & "a" & """"),
       new String'("""" & "abc" & """"),
       new String'("""" & "message digest" & """"),
       new String'("""" & "abcdefghijklmnopqrstuvwxyz" & """"),
       new String'("""" & "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" & """"),
       new String'("""" & "A...Za...z0...9" & """"),
       new String'("8 times " & """" & "1234567890" & """"),
       new String'("1 million times " & """" & "a" & """")
      );

   Set_1_Values : constant array (1 .. 8) of access constant String :=
     (
      new String'(""),
      new String'("a"),
      new String'("abc"),
      new String'("message digest"),
      new String'("abcdefghijklmnopqrstuvwxyz"),
      new String'("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"),
      new String'("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
      new String'("1234567890123456789012345678901234567890" &
                  "1234567890123456789012345678901234567890")
     );


   Step : Natural;


   procedure Print_Header is
   begin
      Put_Line("********************************************************************************");
      Put_Line("* SteelCrypt - NESSIE TestVectors                                              *");
      Put_Line("********************************************************************************");
      New_Line;
      Put_Line("Primitive Name: " & Name);
      Put_Line((16 + Name'Length) * '=');
      Put_Line("Hash size:" & Integer'Image(Digest_Size_Bits) & " bits");
      New_Line;
   end Print_Header;

   procedure Print_Set_Header(Set_No : in Positive; Comment : in String := "") is
   begin
      Put_Line("Test vectors -- set" & Integer'Image(Set_No));
      Put_Line("=====================");
      if Comment'Length > 0 then
         Put_Line(Comment);
      end if;
      New_Line;
   end Print_Set_Header;

   procedure Print_Vector_Header(Set_No : in Positive; Vector_No : in Natural) is
      Vector_No_Str : constant String := Trim(Source => Integer'Image(Vector_No), Side => Both);
   begin
      Put("Set" & Integer'Image(Set_No) &", vector#");
      if Vector_No_Str'Length < 3 then Put(' '); end if;
      if Vector_No_Str'Length < 2 then Put(' '); end if;
      Put_Line(Vector_No_Str & ":");
   end Print_Vector_Header;

   procedure Print_Item(Tag : in String; Block : in u8_Array) is
      Split : constant Boolean := Block'Length > 24;
      j : Integer := Block'First;
   begin
      if Tag'Length < 30 then
         Put((30 - Tag'Length) * ' ');
      end if;
      if Split then
         Put_Line(Tag & "=" & To_Hex(A => Block(j .. j + 15), Upper_Case => True));
         j := j + 16;
         while j + 16 < Block'Last loop
            Put_Line(31 * ' ' & To_Hex(A => Block(j .. j + 15), Upper_Case => True));
            j := j + 16;
         end loop;
         if j <= Block'Last then
            Put_Line(31 * ' ' & To_Hex(A => Block(j .. Block'Last), Upper_Case => True));
         end if;
      else
         Put_Line(Tag & "=" & To_Hex(A => Block, Upper_Case => True));
      end if;
   end Print_Item;

   procedure Print_Item(Tag : in String; Item : in String) is
   begin
      if Tag'Length < 30 then
         Put((30 - Tag'Length) * ' ');
      end if;
      Put_Line(Tag & "=" & Item);
   end Print_Item;

   procedure Hash_Test(Data : in u8_Array; Bits : in Integer := -1) is
      Digest : u8_Array(1 .. Digest_Size_Bytes);
   begin
      Hash(Data => Data, Digest => Digest, Bits => Bits);
      Print_Item("hash", Digest);
   end Hash_Test;

   procedure Hash_Test(Data : in String) is
   begin
      Hash_Test(From_Ascii(Data));
   end;


   procedure Initialize_Display is
   begin
      Step := 0;
      if Verbose then
         New_Line(File => Standard_Error);
      end if;
   end Initialize_Display;

   procedure Update_Display is
   begin
      if Verbose then
--           Put(File => Standard_Error,
--               Item => Character'Val(13) & " ==> " & Name & ": ");
--           --           Put(File => Standard_Error, Item => Float'Image(100.0 * Float(Step) / Float(Total_Number_Of_Primitive_Runs)) & "%");
--           Put(File => Standard_Error,
--               Item => 100.0 * Float(Step) / Float(Total_Number_Of_Primitive_Runs),
--               Fore => 3,
--               Aft  => 2,
--               Exp  => 0 );
--           Put(File => Standard_Error,
--               Item => '%' );
         null;
      end if;
   end Update_Display;

   procedure Set_1_Vector_8 is
      Block : constant u8_Array(1 .. Block_Size_Bytes) := (others => Character'Pos('a'));
      Ctx : Context_T;
      Digest : u8_Array(1 .. Digest_Size_Bytes);
      c : Natural := 1_000_000;
   begin
      Print_Vector_Header(1, 8);
      Print_Item("message", Set_1_Labels(9).all);
      Initialize(Ctx);
      while c > Block_Size_Bytes loop
         Next_Block(Ctx, Block);
         c := c - Block_Size_Bytes;
      end loop;
      Last_Block(Ctx, Block(1 .. c));
      Get_Digest(Ctx, Digest);
      Print_Item("hash", Digest);
      New_Line;
   end;

   procedure Set_1 is
   begin
      Print_Set_Header(1);
      for i in 1 .. 8 loop
         Print_Vector_Header(1, i - 1);
         Print_Item("message", Set_1_Labels(i).all);
         Hash_Test(Set_1_Values(i).all);
         New_Line;
      end loop;
      Set_1_Vector_8;
   end Set_1;

   procedure Set_2 is
      Block : constant u8_Array(1 .. 1024 / 8) := (others => 0);
   begin
      Print_Set_Header(2, "Message digests of strings of 0-bits and variable length:");
      for i in 0 .. 1023 loop
         Print_Vector_Header(2, i);
         Print_Item("message", Trim(Integer'Image(i), Both) & " zero bits");
         Hash_Test(Block, i);
         New_Line;
      end loop;
      New_Line;
   end Set_2;

   procedure Set_3 is
      Block : u8_Array(1 .. 512 / 8) := (others => 0);
   begin
      Print_Set_Header(3, "Message digests of all 512-bit strings S containing a single 1-bit:");
      for i in 0 .. 511 loop
         Print_Vector_Header(3, i);
         Bit_Set(Block, i, 1, High_Order_First);
         Put(23 * ' ');
         Put("message=512-bit string: ");
         Put(Item => i / 8,
             Width => 2 );
         Put("*00,");
         Put(To_Hex(Block(Block'First + i / 8)));
         Put(",");
         Put(Item => (511 - i) / 8,
             Width => 2 );
         Put_Line("*00");
         Hash_Test(Block);
         Bit_Set(Block, i, 0, High_Order_First);
         New_Line;
      end loop;
      New_Line;
   end Set_3;

   procedure Set_4 is
      Digest : u8_Array(1 .. Digest_Size_Bytes) := (others => 0);
   begin
      Print_Set_Header(4);
      Print_Vector_Header(4, 0);
      Print_Item("message", Trim(Integer'Image(Digest_Size_Bits), Both) & " zero bits");
      Hash_Test(Digest);
      for i in 1 .. 100_000 loop
         Hash(Digest, Digest);
      end loop;
      Print_Item("iterated 100000 times", Digest);
      New_Line;
   end Set_4;

   procedure Run(FileName : String := "") is
      File : File_Type;
      Redirected : Boolean := False;
   begin
--        Initialize_Display;
      if FileName /= "" and FileName /= "-" then
         Redirected := True;
         Create(File => File, Name => FileName, Mode => Out_File);
         Set_Output(File);
      end if;
      Print_Header;
      Set_1;
      Set_2;
      Set_3;
      Set_4;
      New_Line; New_Line;
      Put_Line("End of test vectors");
      Flush;
      if Redirected then
         Close(File);
         Set_Output(Standard_Output);
      end if;
   end Run;

   procedure Run_File is
   begin
      Run(Name & Default_Suffix);
   end Run_File;


end Nessie_Hash_Test_Generator;
