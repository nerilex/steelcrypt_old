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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Crypto_Core_Types is

   function To_Hex(A : u8; Upper_Case : Boolean := false) return String is
      S : String(1 .. 2);
      Hex_Table_L : constant array (0 .. 15) of Character :=
        ( '0', '1', '2', '3',
          '4', '5', '6', '7',
          '8', '9', 'a', 'b',
          'c', 'd', 'e', 'f');
      Hex_Table_U : constant array (0 .. 15) of Character :=
        ( '0', '1', '2', '3',
          '4', '5', '6', '7',
          '8', '9', 'A', 'B',
          'C', 'D', 'E', 'F');
   begin
      if Upper_Case then
         S(1) := Hex_Table_U(Integer(Shift_Right(A, 4)));
         S(2) := Hex_Table_U(Integer(A and 16#0f#));
      else
         S(1) := Hex_Table_L(Integer(Shift_Right(A, 4)));
         S(2) := Hex_Table_L(Integer(A and 16#0f#));
      end if;
      return S;
   end To_Hex;


   function To_Hex(A : u8_Array; Upper_Case : Boolean := false; Spacing : Natural := 0) return String is
      S : String(1 .. A'Length * (2 + Spacing));
      k : Positive := 1;
   begin
      for i in A'Range loop
         S(k .. k + 1) := To_Hex(A(i), Upper_Case);
         S(k + 2 .. k + 1 + Spacing) := Spacing * ' ';
         k := k + 2 + Spacing;
      end loop;
      return S;
   end To_Hex;

   function Get_Hex_Value(C : Character) return Integer is
     r : Integer;
   begin
      case C is
         when '0' => r := 0;
         when '1' => r := 1;
         when '2' => r := 2;
         when '3' => r := 3;
         when '4' => r := 4;
         when '5' => r := 5;
         when '6' => r := 6;
         when '7' => r := 7;
         when '8' => r := 8;
         when '9' => r := 9;
         when 'A' => r := 10;
         when 'a' => r := 10;
         when 'B' => r := 11;
         when 'b' => r := 11;
         when 'C' => r := 12;
         when 'c' => r := 12;
         when 'D' => r := 13;
         when 'd' => r := 13;
         when 'E' => r := 14;
         when 'e' => r := 14;
         when 'F' => r := 15;
         when 'f' => r := 15;
         when others => r:= -1;
      end case;
      return r;
   end;


   function From_Hex(S : String) return u8_Array is
      A : u8_Array(1 .. (S'Length + 1) / 2) := (others => 0);
      C : Character;
      V : Integer range -1 .. 15;
      Index : Positive := 1;
      Inside : Boolean := False;
   begin
      for i in S'Range loop
         C := S(i);
         if C /= ' ' and then C /= '_' then
            V := Get_Hex_Value(C);
            if V = -1 then
               raise Format_Violation;
            else
               if Inside then
                  A(Index) := A(Index) or u8(V);
                  Inside := False;
                  Index := Index + 1;
               else
                  A(Index) := Shift_Left(u8(V), 4);
                  Inside := True;
               end if;
            end if;
         end if;
      end loop;
      return A;
   end From_Hex;

   function From_Ascii(S : String) return u8_Array is
      A : u8_Array(1 .. S'Length);
      Index : Integer := 1;
   begin
      for i in S'Range loop
         A(Index) := u8(Character'Pos(S(i)));
         Index := INdex + 1;
      end loop;
      return A;
   end From_Ascii;

end Crypto_Core_types;
