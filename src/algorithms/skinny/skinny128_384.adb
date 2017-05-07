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

package body Skinny128_384 is

   function Initialize (Tweakey : in Key_T) return Context_T is
   begin
      return Context_T(Core.Initialize(Tweakey));
   end Initialize;

   function Encrypt
     (Context : in Context_T;
      Block   : in Block_T) return Block_T is
   begin
      return Core.Encrypt(Core.Context_T(Context), Block);
   end;

   function Decrypt
     (Context : in Context_T;
      Block   : in Block_T) return Block_T is
   begin
      return Core.Decrypt(Core.Context_T(Context), Block);
   end;

   function Permute_State(In_State : State_T) return State_T is
      type Map_Entry_T is record
         Row : Row_Index_T;
         Col : Column_Index_T;
      end record;
      Map : constant array (Row_Index_T, Column_Index_T) of Map_Entry_T :=
        ( ((3, 2), (4, 4), (3, 1), (4, 2)),
          ((3, 3), (4, 3), (4, 1), (3, 4)),
          ((1, 1), (1, 2), (1, 3), (1, 4)),
          ((2, 1), (2, 2), (2, 3), (2, 4)) );
      Tmp : State_T;
      index : Map_Entry_T;
   begin
      for row in Row_Index_T loop
         for col in Column_Index_T loop
            index := Map(row, col);
            Tmp(row)(col) := In_State(index.Row)(index.Col);
         end loop;
      end loop;
      return Tmp;
   end Permute_State;


   function TK_Update(Tk : Tweakey_State_T) return Tweakey_State_T is
      Tmp : Tweakey_State_T;
      function LFSR_1(x : Cell_T) return Cell_T is
         y : Cell_T;
      begin
         y := Shift_Left(x, 1);
         y := y xor ((Shift_Right(x, 5) xor Shift_Right(x, 7)) and 1);
         return y;
      end LFSR_1;
      function LFSR_2(x : Cell_T) return Cell_T is
         y : Cell_T;
      begin
         y := Shift_Right(x, 1);
         y := y xor ((Shift_Left(x, 1) xor Shift_Left(x, 7)) and 2#1000_0000#);
         return y;
      end LFSR_2;
      function LFSR_1(x : State_T) return State_T is
         y : State_T;
      begin
         for row in Row_Index_T(1) .. Row_Index_T(2) loop
            for col in x(row)'Range loop
               y(row)(col) := LFSR_1(x(row)(col));
            end loop;
         end loop;
         return y;
      end LFSR_1;
      function LFSR_2(x : State_T) return State_T is
         y : State_T;
      begin
         for row in Row_Index_T(1) .. Row_Index_T(2) loop
            for col in x(row)'Range loop
               y(row)(col) := LFSR_2(x(row)(col));
            end loop;
         end loop;
         return y;
      end LFSR_2;

   begin
      Tmp(1) := Permute_State(Tk(1));
      Tmp(2) := LFSR_1(Permute_State(Tk(2)));
      Tmp(3) := LFSR_2(Permute_State(Tk(3)));
      return Tmp;
   end;

   function Get_Round_Tweakey(Tk : Tweakey_State_T) return Round_Tweakey_T is
      Round_Tweakey : Round_Tweakey_T;
      Tmp : Cell_T;
      Col : Column_Index_T;
      Row : Row_Index_T;
   begin
         for  i in 0 .. Round_Tweakey'Length - 1 loop
         Tmp := 0;
         Row := Row_Index_T(1 + (i / 4));
         Col := Column_Index_T(1 + (i mod 4));
         for state in Tk'Range loop
            Tmp := Tmp xor Tk(state)(Row)(Col);
         end loop;
         Round_Tweakey(Round_Tweakey'First + i) := Tmp;
         end loop;

      return Round_Tweakey;
   end;

   function Load_Tweakey(Key : u8_Array) return Tweakey_State_T is
      Tk : Tweakey_State_T;
      index : Integer := Key'First;
   begin
      for state in Tk'Range loop
         for row in Tk(state)'Range loop
            for col in Tk(state)(row)'Range loop
               Tk(state)(row)(col) := Key(index);
               index := index + 1;
            end loop;
         end loop;
      end loop;
      return Tk;
   end;

end Skinny128_384;
