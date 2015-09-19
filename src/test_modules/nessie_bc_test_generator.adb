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
--  with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Crypto_Types; use Crypto_Types;
with System; use System;
use Crypto_Types.Crypto_Utils_u8;


--  ********************************************************************************
--  *Project NESSIE - New European Schemes for Signature, Integrity, and Encryption*
--  ********************************************************************************
--
--  Primitive Name: Des
--  ===================
--  Key size: 64 bits
--  Block size: 64 bits
--
--  Test vectors -- set 1
--  =====================
--
--  Set 1, vector#  0:
--  key=8000000000000000
--  Plain=0000000000000000
--  cipher=95A8D72813DAA94D
--  decrypted=0000000000000000
--  Iterated 100 times=F749E1F8DEFAF605
--  Iterated 1000 times=F396DD0B33D04244
--
--  Set 1, vector#  1:
--  key=4000000000000000
--  plain=0000000000000000
--  cipher=0EEC1487DD8C26D5
--  decrypted=0000000000000000
--  Iterated 100 times=E5BEE86B600F3B48
--  Iterated 1000 times=1D5931D700EF4E15


package body Nessie_BC_Test_Generator is

   use Cipher;
   Total_Number_Of_Encrypts : constant Natural :=
     (Key_Size_Bits + Block_Size_Bits + 256 + 2) * (1 + 1000 + 1);

   Total_Number_Of_Decrypts : constant Natural :=
     (Key_Size_Bits + Block_Size_Bits + 256 + 2) * (1 + 1000 + 1);

   Total_Number_Of_Primitive_Runs : constant Natural :=
     Total_Number_Of_Encrypts + Total_Number_Of_Decrypts;

   Primitive_Runs_Per_Encrypt_Test : constant := 2002;
   Primitive_Runs_Per_Decrypt_Test : constant := 2;

   Step : Natural;

   Kasumi_Test_Key : constant u8_Array(1 .. 16) :=
     (
      16#2B#, 16#D6#, 16#45#, 16#9F#, 16#82#, 16#C5#, 16#B3#, 16#00#,
      16#95#, 16#2C#, 16#49#, 16#10#, 16#48#, 16#81#, 16#FF#, 16#48#
     );

   Kasumi_Test_Plain : constant u8_Array(1 .. 8) :=
     (
      16#EA#, 16#02#, 16#47#, 16#14#, 16#AD#, 16#5C#, 16#4D#, 16#84#
     );

   procedure Print_Header is
   begin
      Put_Line("********************************************************************************");
      Put_Line("* SteelCrypt - NESSIE TestVectors                                              *");
      Put_Line("********************************************************************************");
      New_Line;
      Put_Line("Primitive Name: " & Cipher.Name);
      Put_Line((16 + Cipher.Name'Length) * '=');
      Put_Line("Key size:" & Integer'Image(Key_Size_Bits) & " bits");
      Put_Line("Block size:" & Integer'Image(Block_Size_Bits) & " bits");
      New_Line;
   end Print_Header;

   procedure Print_Set_Header(Set_No : in Positive) is
   begin
      Put_Line("Test vectors -- set" & Integer'Image(Set_No));
      Put_Line("=====================");
      New_Line;
   end Print_Set_Header;

   procedure Print_Vector_Header(Set_No : in Positive; Vector_No : in Natural) is
      Vector_No_Str : constant String := Trim(Source => Integer'Image(Vector_No), Side => Both);
   begin
      Put("Set" & Integer'Image(Set_No) &", vector#");
      Put((3 - Vector_No_Str'Length) * ' ');
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


   procedure Run_Encrypt_Test(Key : in Key_T; Block : in Block_T) is
      Temp : Block_T := Block;
      Context : Context_T;
   begin
      Print_Item("key", Key);
      Print_Item("plain", Temp);
      Initialize(Context, Key);
      Encrypt(Context, Temp);
      Print_Item("cipher", Temp);
      Decrypt(Context, Temp);
      Print_Item("decrypted", Temp);
      if Temp /= Block then
         Put_Line("!!! decrypted /= plain");
      end if;
      for i in 1 .. 100 loop
         Encrypt(Context, Temp);
      end loop;
      Print_Item("Iterated 100 times", Temp);
      for i in 101 .. 1000 loop
         Encrypt(Context, Temp);
      end loop;
      Print_Item("Iterated 1000 times", Temp);
      for i in 1 .. 1000 loop
         Decrypt(Context, Temp);
      end loop;
      if Temp /= Block then
         Put_Line("!!! decrypted /= plain");
      end if;
   end Run_Encrypt_Test;

   procedure Run_Decrypt_Test(Key : in Key_T; Block : in Block_T) is
      Temp : Block_T := Block;
      Context : Context_T;
   begin
      Print_Item("key", Key);
      Print_Item("cipher", Temp);
      Initialize(Context, Key);
      Decrypt(Context, Temp);
      Print_Item("plain", Temp);
      Encrypt(Context, Temp);
      Print_Item("encrypted", Temp);
      if Temp /= Block then
         Put_Line("!!! encrypted /= plain");
      end if;
   end Run_Decrypt_Test;

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
         --         Set_Col(File => Standard_Error, To => 1);
         Put(File => Standard_Error,
             Item => Character'Val(13) & " ==> " & Cipher.Name & ": ");
         --           Put(File => Standard_Error, Item => Float'Image(100.0 * Float(Step) / Float(Total_Number_Of_Primitive_Runs)) & "%");
         Put(File => Standard_Error,
             Item => 100.0 * Float(Step) / Float(Total_Number_Of_Primitive_Runs),
             Fore => 3,
             Aft  => 2,
             Exp  => 0 );
         Put(File => Standard_Error,
             Item => '%' );
      end if;
   end Update_Display;


   procedure Run_Test(Key : in Key_T; Block : in Block_T; Encrypt_Test : in Boolean := True) is
   begin
      if Encrypt_Test then
         Run_Encrypt_Test(Key, Block);
         Step := Step + Primitive_Runs_Per_Encrypt_Test;
      else
         Run_Decrypt_Test(Key, Block);
         Step := Step + Primitive_Runs_Per_Decrypt_Test;
      end if;
      Update_Display;
   end Run_Test;

   procedure Set_1(Set_No : in Positive; Encrypt_Test : in Boolean := True) is
      Key : Key_T;
      Block : constant Block_T := (others => 0);
   begin
      Print_Set_Header(Set_No);
      for i in 0 .. Key_Size_Bits - 1 loop
         Print_Vector_Header(Set_No, i);
         Key := (others => 0);
         Bit_Set(A => Key, Bit_Address => i, Value => 1, Order => High_Order_First);
         Run_Test(Key, Block, Encrypt_Test);
         New_Line;
      end loop;
   end Set_1;

   procedure Set_2(Set_No : in Positive; Encrypt_Test : in Boolean := True) is
      Key : constant Key_T := (others => 0);
      Block : Block_T;
   begin
      Print_Set_Header(Set_No);
      for i in 0 .. Block_Size_Bits - 1 loop
         Print_Vector_Header(Set_No, i);
         Block := (others => 0);
         Bit_Set(A => Block, Bit_Address => i, Value => 1, Order => High_Order_First);
         Run_Test(Key, Block, Encrypt_Test);
         New_Line;
      end loop;
   end Set_2;

   procedure Set_3(Set_No : in Positive; Encrypt_Test : in Boolean := True) is
      Key : Key_T;
      Block : Block_T;
   begin
      Print_Set_Header(Set_No);
      for i in u8'Range loop
         Print_Vector_Header(Set_No, Natural(i));
         Block := (others => i);
         Key := (others => i);
         Run_Test(Key, Block, Encrypt_Test);
         New_Line;
      end loop;
   end Set_3;

   procedure Set_4(Set_No : in Positive; Encrypt_Test : in Boolean := True) is
      Key : Key_T;
      Block : Block_T;
   begin
      Print_Set_Header(Set_No);

      Print_Vector_Header(Set_No, 0);
      for i in 0 .. Key'Length - 1 loop
         Key(Key'First + i) :=  u8(i mod 256);
      end loop;
      for i in 0 .. Block'Length - 1 loop
         Block(Block'First + i) :=  u8((17 * i) mod 256);
      end loop;
      Run_Test(Key, Block, Encrypt_Test);
      New_Line;

      Print_Vector_Header(Set_No, 1);
      for i in 0 .. Key'Length - 1 loop
         Key(Key'First + i) :=  Kasumi_Test_Key(Kasumi_Test_Key'First + (i mod Kasumi_Test_Key'Length));
      end loop;
      for i in 0 .. Block'Length - 1 loop
         Block(Block'First + i) :=  Kasumi_Test_Plain(Kasumi_Test_Plain'First + (i mod Kasumi_Test_Plain'Length));
      end loop;
      Run_Test(Key, Block, Encrypt_Test);
      New_Line;
   end Set_4;

   procedure Run(FileName : String := "") is
      File : File_Type;
      Redirected : Boolean := False;
   begin
      Initialize_Display;
      if FileName /= "" and FileName /= "-" then
         Redirected := True;
         Create(File => File, Name => FileName, Mode => Out_File);
         Set_Output(File);
      end if;
      Print_Header;
      Set_1(1, True);
      Set_2(2, True);
      Set_3(3, True);
      Set_4(4, True);
      Set_1(5, False);
      Set_2(6, False);
      Set_3(7, False);
      Set_4(8, False);
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
      Run(Cipher.Name & Default_Suffix);
   end Run_File;


end Nessie_BC_Test_Generator;
