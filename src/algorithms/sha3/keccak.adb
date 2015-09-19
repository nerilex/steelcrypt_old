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

--  with Ada.Text_IO; use Ada.Text_IO;

package body Keccak is

--     procedure Print_State(A : State_T; S : String := "" ) is
--        c : Natural := 1;
--        l : u8_Array(1 .. 8);
--     begin
--        if S'Length > 0 then
--           Put_Line(S);
--        end if;
--        for i in y_T'Range loop
--           for j in x_T'Range loop
--              Store_le(A     => l,
--                       value => A(j, i));
--              for z in l'Range loop
--                 Put(To_Hex(l(z)));
--                 Put(' ');
--                 if c mod 16 = 0 then
--                    New_Line;
--                 end if;
--                 c := c + 1;
--              end loop;
--           end loop;
--        end loop;
--        New_Line;
--     end;

   procedure theta( A : in out State_T ) is
      C : Array (x_T) of Word_T;
      D : Array (x_T) of Word_T;
   begin
      for i in x_T'Range loop
         C(i) := A(i, 0) xor A(i, 1) xor A(i, 2) xor A(i, 3) xor A(i, 4);
      end loop;
      for i in x_T'Range loop
         D(i) := C(i - 1) xor Rotate_Left(C(i + 1), 1);
      end loop;
      for i in x_T'Range loop
         for j in y_T'Range loop
            A(i, j) := A(i, j) xor D(i);
         end loop;
      end loop;
   end theta;

   procedure rho( A : in out State_T ) is
      Rotation_Table : constant Array (x_T, y_T) of z_T :=
        (
           (  0, 36,  3, 41, 18 ),
           (  1, 44, 10, 45,  2 ),
           ( 62,  6, 43, 15, 61 ),
           ( 28, 55, 25, 21, 56 ),
           ( 27, 20, 39,  8, 14 )
         );
   begin
      for i in x_T'Range loop
         for j in y_T'Range loop
            A(i, j) := Rotate_Left(A(i, j), Integer(Rotation_Table(i, j)));
         end loop;
      end loop;
   end rho;

   procedure pi( A : in out State_T ) is
      Ax : constant State_T := A;
   begin
      for i in x_T'Range loop
         for j in y_T'Range loop
            A(i, j) := Ax(i + 3 * x_T(j), y_T(i));
         end loop;
      end loop;
   end pi;

   procedure chi( A : in out State_T ) is
      Ax : constant State_T := A;
   begin
      for i in x_T'Range loop
         for j in y_T'Range loop
            A(i, j) := A(i, j) xor ((not Ax(i + 1, j)) and Ax(i + 2, j));
         end loop;
      end loop;
   end chi;

   procedure iota( A : in out State_T; round : Natural ) is
      Round_Constants : constant Array(1 .. rounds) of Word_T :=
        (
         16#0000000000000001#, -- round 1
         16#0000000000008082#, -- round 2
         16#800000000000808a#, -- round 3
         16#8000000080008000#, -- round 4
         16#000000000000808b#, -- round 5
         16#0000000080000001#, -- round 6
         16#8000000080008081#, -- round 7
         16#8000000000008009#, -- round 8
         16#000000000000008a#, -- round 9
         16#0000000000000088#, -- round 10
         16#0000000080008009#, -- round 11
         16#000000008000000a#, -- round 12
         16#000000008000808b#, -- round 13
         16#800000000000008b#, -- round 14
         16#8000000000008089#, -- round 15
         16#8000000000008003#, -- round 16
         16#8000000000008002#, -- round 17
         16#8000000000000080#, -- round 18
         16#000000000000800a#, -- round 19
         16#800000008000000a#, -- round 20
         16#8000000080008081#, -- round 21
         16#8000000000008080#, -- round 22
         16#0000000080000001#, -- round 23
         16#8000000080008008#  -- round 24
         );
   begin
      A(0, 0) := A(0, 0) xor Round_Constants(round);
   end iota;

   procedure Permute ( A : in out State_T ) is
   begin
--          Print_State(A, "### Initial state:");
      for i in 1 .. rounds loop
--           Put_Line("## Round " & Integer'Image(i) & ":");
         theta(A);
--           Print_State(A, "After Theta:");
         rho(A);
--           Print_State(A, "After Rho:");
         pi(A);
--           Print_State(A, "After Pi:");
         chi(A);
--           Print_State(A, "After Chi:");
         iota(A, i);
--           Print_State(A, "After Iota:");
      end loop;
   end Permute;

end Keccak;


