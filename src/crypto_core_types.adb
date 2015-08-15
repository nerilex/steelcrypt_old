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

package body Crypto_Core_Types is

   function To_Hex(A : u8) return String is
      S : String(1 .. 2);
      Hex_Table : constant array (0 .. 15) of Character :=
        ( '0', '1', '2', '3',
          '4', '5', '6', '7',
          '8', '9', 'a', 'b',
          'c', 'd', 'e', 'f');
   begin
      S(1) := Hex_Table(Integer(Shift_Right(A, 4)));
      S(2) := Hex_Table(Integer(A and 16#0f#));
      return S;
   end To_Hex;


   function To_Hex(A : u8_Array) return String is
      S : String(1 .. A'Length * 2);
      k : Positive := 1;
   begin
      for i in A'Range loop
         S(k .. k + 1) := To_Hex(A(i));
         k := k + 2;
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

   procedure Bit_Clear(Buffer : in out u8_Array; Index : in Positive) is
   begin
      Buffer(Buffer'First + Integer(Index / 8)) := Buffer(Buffer'First + Integer(Index / 8)) and (not Shift_Left(1, 7 - (Index - 1) mod 8));
   end Bit_Clear;

   procedure Bit_Set(Buffer : in out u8_Array; Index : in Positive) is
   begin
      Buffer(Integer(Buffer'First + Index / 8)) := Buffer(Buffer'First + Integer(Index / 8)) or Shift_Left(1, 7 - (Index - 1) mod 8);
   end Bit_Set;

   procedure Bit_Toggle(Buffer : in out u8_Array; Index : in Positive) is
   begin
      Buffer(Integer(Buffer'First + Index / 8)) := Buffer(Buffer'First + Integer(Index / 8)) xor Shift_Left(1, 7 - (Index - 1) mod 8);
   end Bit_Toggle;


   procedure Bit_Set(Buffer : in out u8_Array; Index : in Positive; Value : in Bit) is
   begin
      if Value = 1 then
         Bit_Set(Buffer, Index);
      else
         Bit_Clear(Buffer, Index);
      end if;
   end Bit_Set;

   function Bit_Get(Buffer : in u8_Array; Index : in Positive) return Bit is
   begin
      return Bit(Shift_Right(Buffer(Buffer'First + Index / 8), 7 - (Index - 1) mod 8) and 1);
   end Bit_Get;


end Crypto_Core_types;
