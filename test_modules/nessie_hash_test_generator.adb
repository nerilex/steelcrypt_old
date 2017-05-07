--  Copyright (C) 2015  bg nerilex <bg@nerilex.org>
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

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings;         use Ada.Strings;
with Ada.Strings.Fixed;   use Ada.Strings.Fixed;
with Crypto_Types;        use Crypto_Types;
with System;              use System;
use Crypto_Types.Crypto_Utils_u8;

package body Nessie_Hash_Test_Generator is

   use Hash;

   Set_1_Labels : constant array (1 .. 9) of access constant String :=
     (new String'("""" & """" & " (empty string)"),
      new String'("""" & "a" & """"),
      new String'("""" & "abc" & """"),
      new String'("""" & "message digest" & """"),
      new String'("""" & "abcdefghijklmnopqrstuvwxyz" & """"),
      new String'
        ("""" &
         "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" &
         """"),
      new String'("""" & "A...Za...z0...9" & """"),
      new String'("8 times " & """" & "1234567890" & """"),
      new String'("1 million times " & """" & "a" & """"));

   Set_1_Values : constant array (1 .. 8) of access constant String :=
     (new String'(""),
      new String'("a"),
      new String'("abc"),
      new String'("message digest"),
      new String'("abcdefghijklmnopqrstuvwxyz"),
      new String'("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"),
      new String'
        ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
      new String'
        ("1234567890123456789012345678901234567890" &
         "1234567890123456789012345678901234567890"));

   procedure Print_Header is
   begin
      Put_Line
        ("********************************************************************************");
      Put_Line
        ("* SteelCrypt - NESSIE TestVectors                                              *");
      Put_Line
        ("********************************************************************************");
      New_Line;
      Put_Line ("Primitive Name: " & Hash.Name);
      Put_Line ((16 + Hash.Name'Length) * '=');
      Put_Line
        ("Hash size:" & Integer'Image (Hash.Digest_Size_Bits) & " bits");
      New_Line;
   end Print_Header;

   procedure Print_Set_Header
     (Set_No  : in Positive;
      Comment : in String := "")
   is
   begin
      Put_Line ("Test vectors -- set" & Integer'Image (Set_No));
      Put_Line ("=====================");
      if Comment'Length > 0 then
         Put_Line (Comment);
      end if;
      New_Line;
   end Print_Set_Header;

   procedure Print_Vector_Header
     (Set_No    : in Positive;
      Vector_No : in Natural)
   is
      Vector_No_Str : constant String :=
        Trim (Source => Integer'Image (Vector_No), Side => Both);
   begin
      Put ("Set" & Integer'Image (Set_No) & ", vector#");
      if Vector_No_Str'Length < 3 then
         Put (' ');
      end if;
      if Vector_No_Str'Length < 2 then
         Put (' ');
      end if;
      Put_Line (Vector_No_Str & ":");
   end Print_Vector_Header;

   procedure Print_Item (Tag : in String; Block : in u8_Array) is
      Split : constant Boolean := Block'Length > 24;
      j     : Integer          := Block'First;
   begin
      if Tag'Length < 30 then
         Put ((30 - Tag'Length) * ' ');
      end if;
      if Split then
         Put_Line
           (Tag & "=" & To_Hex (A => Block (j .. j + 15), Upper_Case => True));
         j := j + 16;
         while j + 16 < Block'Last loop
            Put_Line
              (31 * ' ' &
               To_Hex (A => Block (j .. j + 15), Upper_Case => True));
            j := j + 16;
         end loop;
         if j <= Block'Last then
            Put_Line
              (31 * ' ' &
               To_Hex (A => Block (j .. Block'Last), Upper_Case => True));
         end if;
      else
         Put_Line (Tag & "=" & To_Hex (A => Block, Upper_Case => True));
      end if;
   end Print_Item;

   procedure Print_Item (Tag : in String; Item : in String) is
   begin
      if Tag'Length < 30 then
         Put ((30 - Tag'Length) * ' ');
      end if;
      Put_Line (Tag & "=" & Item);
   end Print_Item;

   procedure Hash_Test (Data : in u8_Array; Bits : in Integer := -1) is
      Digest : u8_Array (1 .. Digest_Size_Bytes);
   begin
      Hash.Hash (Data => Data, Digest => Digest, Bits => Bits);
      Print_Item ("hash", Digest);
   end Hash_Test;

   procedure Hash_Test (Data : in String) is
   begin
      Hash_Test (From_Ascii (Data));
   end Hash_Test;

   procedure Set_1_Vector_8 is
      Block : constant u8_Array (1 .. Block_Size_Bytes) :=
        (others => Character'Pos ('a'));
      Ctx    : Context_T;
      Digest : u8_Array (1 .. Digest_Size_Bytes);
      c      : Natural := 1_000_000;
   begin
      Print_Vector_Header (1, 8);
      Print_Item ("message", Set_1_Labels (9).all);
      Initialize (Ctx);
      while c > Block_Size_Bytes loop
         Next_Block (Ctx, Block);
         c := c - Block_Size_Bytes;
      end loop;
      Last_Block (Ctx, Block (1 .. c));
      Get_Digest (Ctx, Digest);
      Print_Item ("hash", Digest);
      New_Line;
   end Set_1_Vector_8;

   procedure Set_1 is
   begin
      Print_Set_Header (1);
      for i in 1 .. 8 loop
         Print_Vector_Header (1, i - 1);
         Print_Item ("message", Set_1_Labels (i).all);
         Hash_Test (Set_1_Values (i).all);
         New_Line;
      end loop;
      Set_1_Vector_8;
   end Set_1;

   procedure Set_2 is
      Block : constant u8_Array (1 .. 1024 / 8) := (others => 0);
   begin
      Print_Set_Header
        (2,
         "Message digests of strings of 0-bits and variable length:");
      for i in 0 .. 1023 loop
         Print_Vector_Header (2, i);
         Print_Item ("message", Trim (Integer'Image (i), Both) & " zero bits");
         Hash_Test (Block, i);
         New_Line;
      end loop;
      New_Line;
   end Set_2;

   procedure Set_3 is
      Block : u8_Array (1 .. 512 / 8) := (others => 0);
   begin
      Print_Set_Header
        (3,
         "Message digests of all 512-bit strings S containing a single 1-bit:");
      for i in 0 .. 511 loop
         Print_Vector_Header (3, i);
         Bit_Set (Block, i, 1, High_Order_First);
         Put (23 * ' ');
         Put ("message=512-bit string: ");
         Put (Item => i / 8, Width => 2);
         Put ("*00,");
         Put (To_Hex (Block (Block'First + i / 8)));
         Put (",");
         Put (Item => (511 - i) / 8, Width => 2);
         Put_Line ("*00");
         Hash_Test (Block);
         Bit_Set (Block, i, 0, High_Order_First);
         New_Line;
      end loop;
      New_Line;
   end Set_3;

   procedure Set_4 is
      Digest : u8_Array (1 .. Digest_Size_Bytes) := (others => 0);
   begin
      Print_Set_Header (4);
      Print_Vector_Header (4, 0);
      Print_Item
        ("message",
         Trim (Integer'Image (Digest_Size_Bits), Both) & " zero bits");
      Hash_Test (Digest);
      for i in 1 .. 100_000 loop
         Hash.Hash (Digest, Digest);
      end loop;
      Print_Item ("iterated 100000 times", Digest);
      New_Line;
   end Set_4;

   procedure Run (FileName : String := "") is
      File       : File_Type;
      Redirected : Boolean := False;
   begin
      if FileName /= "" and FileName /= "-" then
         Redirected := True;
         Create (File => File, Name => FileName, Mode => Out_File);
         Set_Output (File);
      end if;
      Print_Header;
      Set_1;
      Set_2;
      Set_3;
      Set_4;
      New_Line;
      New_Line;
      Put_Line ("End of test vectors");
      Flush;
      if Redirected then
         Close (File);
         Set_Output (Standard_Output);
      end if;
   end Run;

   procedure Run_File is
   begin
      Run (Hash.Name & Default_Suffix);
   end Run_File;

end Nessie_Hash_Test_Generator;
