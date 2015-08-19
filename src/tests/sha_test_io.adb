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
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;


--
--  #  CAVS 11.0
--  #  "SHA-256 ShortMsg" information
--  #  SHA-256 tests are configured for BIT oriented implementations
--  #  Generated on Tue Mar 15 08:29:11 2011
--
--  [L = 32]
--
--  Len = 0
--  Msg = 00
--  MD = e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
--
--  Len = 1
--  Msg = 00
--  MD = bd4f9e98beb68c6ead3243b1b4c7fed75fa4feaab1f84795cbd8a98676a2a375
--
--  Len = 2
--  Msg = 80
--  MD = 18f331f626210ff9bad6995d8cff6e891adba50eb2fdbddcaa921221cdc333ae
--
--  Len = 3
--  Msg = 60
--  MD = 1f7794d4b0b67d3a6edcd17aba2144a95828032f7943ed26bf0c7c7628945f48
--

package body Sha_Test_IO is

   procedure Open(Context : out Context_T; FileName : in String) is
   begin
      Open(Context.File, In_File, FileName);
   end Open;


   procedure Goto_Data(F : in out File_Type) is
      C : Character;
      After_Equal : boolean := false;
      Ignored : boolean;
   begin
      loop
         exit when End_Of_File(F);
         if After_Equal then
            Look_Ahead(F, C, ignored);
            case C is
               when '0' .. '9' => exit;
               when 'A' .. 'F' => exit;
               when 'a' .. 'f' => exit;
               when others => null;
            end case;
         end if;
         Get(F, C);
         if C = '=' then
            After_Equal := true;
         end if;
      end loop;
   end Goto_Data;


   procedure Get_Next_Type(Context : in out Context_T; Next : out Next_Type) is
      C : Character;
   begin
      loop
         exit when End_Of_File(Context.File);
         Get(Context.File, C);
         exit when End_Of_File(Context.File);
         case C is
            when '#' => Skip_Line(Context.File);
            when '[' => exit;
            when 'L' => exit;
            when 'C' => exit;
            when 'S' => Get(Context.File, C); exit;
            when 'M' => Get(Context.File, C); exit;
            when others => null;
         end case;
      end loop;
      if not End_Of_File(Context.File) then
         Goto_Data(Context.File);
      end if;
      if End_Of_File(Context.File) then
         Close(Context.File);
         Next := Finish;
      else
         case C is
            when '[' => Next := Digest_Length;
            when 'L' => Next := Message_Length;
            when 'e' => Next := Seed; -- second char from "Seed"
            when 'C' => Next := Count;
            when 's' => Next := Message_Block; -- second char from "Msg"
            when 'D' => Next := Message_Digest; -- second char from "MD"
            when others => raise Format_Violation;
         end case;
      end if;
   end Get_Next_type;

   procedure Get_Integer(Context : in out Context_T; Value : out Integer) is
   begin
      Get(Context.File, Value);
   end Get_Integer;

   procedure Get_Data(Context : in out Context_T; Block : out u8_Array) is
      L,P : Natural := 0;
      I : Integer := Block'First;
      S : String(1 .. 80);
   begin
      L := Block'Length * 2;
      loop
         if L > S'Length then
            P := S'Length;
         else
            P := L;
         end if;
         Get(Context.File, S(1 .. P));
         Block(I .. I + P / 2 - 1) := From_Hex(S(1 .. P));
         L := L - P;
         I := I + P / 2;
         if L = 0 or End_Of_File(Context.File) then
            exit;
         end if;
      end loop;
   end Get_Data;

   procedure Test_With_File(FileName : in String) is
      f : Context_T;
      nt : Next_Type;
      count_val : Integer;
      dlen : Integer := Digest_Size_Bits / 8;
      len, lenb : Integer;
      Digest_Size_Bytes : constant Natural := (Digest_Size_Bits + 7 ) / 8;
      digest, ref_Digest : u8_Array(1 .. Digest_Size_Bytes) := (others => 0);
      ok_test, fail_test, num : Natural := 0;
      seed_val : u8_Array(1 .. Digest_Size_Bytes) := (others => 0);
   begin
      New_Line;
      Put("== " & FileName &" ==");
      Open(f, FileName);
      loop
         Get_Next_Type(f, nt);
         case nt is
         when Sha_Test_IO.Finish =>
            New_Line;
            Put("    (ok: " & Integer'Image(ok_test) & " / fail: " & Integer'Image(fail_test) & ")");
            New_Line;
            exit;
         when Digest_Length =>
            Get_Integer(f, dlen);
         when Seed =>
            Get_Data(f, Seed_Val);
         when Count =>
            Get_Integer(f, count_val);
            if num mod 64 = 0 then
               New_Line;
               Put("  ");
               Put(Item => num, Width => 6);
               Put(" :  ");
            end if;
            num := num + 1;
            declare
               blob : u8_Array(1 .. 3 * Digest_Size_Bytes);
            begin
               blob(1 + 0 * Digest_Size_Bytes .. 1 * Digest_Size_Bytes) := Seed_Val;
               blob(1 + 1 * Digest_Size_Bytes .. 2 * Digest_Size_Bytes) := Seed_Val;
               blob(1 + 2 * Digest_Size_Bytes .. 3 * Digest_Size_Bytes) := Seed_Val;
               for i in 1 .. 1000 loop
                  Hash(blob, Digest);
                  blob(1 .. 2 * Digest_Size_Bytes) := blob(1 + 1 * Digest_Size_Bytes .. 3 * Digest_Size_Bytes);
                  blob(1 + 2 * Digest_Size_Bytes .. 3 * Digest_Size_Bytes) := Digest;
               end loop;
               Seed_Val := Digest;
            end;
         when Message_Length =>
            Get_Integer(f, len);
            if len = 0 then
               lenb := 1;
            else
               lenb := (len + 7) / 8;
            end if;
         when Message_Digest =>
            Get_Data(f, ref_digest(1 .. dlen));
            if ref_digest = digest then
               ok_test := ok_test + 1;
               Put('*');
            else
               fail_test := fail_test + 1;
               Put('!');
               Put_Line("  DBG: is:     " & To_Hex(digest));
               Put_Line("  DBG: should: " & To_Hex(ref_digest));
            end if;
         when Message_Block =>
            declare
               buf : u8_Array(1 .. lenb);
            begin
               if num mod 64 = 0 then
                  New_Line;
                  Put("  ");
                  Put(Item => num, Width => 6);
                  Put(" :  ");
               end if;
               num := num + 1;
               Get_Data(f, buf);
--                 Put_Line("  DBG: dlen = " & Integer'Image(dlen) & ";  len = " & Integer'Image(len));
               Hash(buf, digest(1 .. dlen), len);
            end;
         end case;
      end loop;
   end;


end Sha_Test_IO;
