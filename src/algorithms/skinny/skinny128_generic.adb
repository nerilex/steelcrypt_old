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

--  with Ada.Text_IO; use Ada.Text_IO;


package body Skinny128_Generic is
   use Row_Utils;

   function Round_Constant(Round : Natural) return Cell_T is
      Table : constant Array (0 .. 61) of Cell_T :=
        (
  16#01#, 16#03#, 16#07#, 16#0F#, 16#1F#, 16#3E#, 16#3D#, 16#3B#,
  16#37#, 16#2F#, 16#1E#, 16#3C#, 16#39#, 16#33#, 16#27#, 16#0E#,
  16#1D#, 16#3A#, 16#35#, 16#2B#, 16#16#, 16#2C#, 16#18#, 16#30#,
  16#21#, 16#02#, 16#05#, 16#0B#, 16#17#, 16#2E#, 16#1C#, 16#38#,
  16#31#, 16#23#, 16#06#, 16#0D#, 16#1B#, 16#36#, 16#2D#, 16#1A#,
  16#34#, 16#29#, 16#12#, 16#24#, 16#08#, 16#11#, 16#22#, 16#04#,
  16#09#, 16#13#, 16#26#, 16#0C#, 16#19#, 16#32#, 16#25#, 16#0A#,
  16#15#, 16#2A#, 16#14#, 16#28#, 16#10#, 16#20#
       );
   begin
      return Table((Round - 1) mod Table'Length);
   end Round_Constant;


   function Sbox(Cell : Cell_T) return Cell_T is
      Table : constant Sbox_T :=
        (
         16#65#, 16#4c#, 16#6a#, 16#42#, 16#4b#, 16#63#, 16#43#, 16#6b#,
         16#55#, 16#75#, 16#5a#, 16#7a#, 16#53#, 16#73#, 16#5b#, 16#7b#,
         16#35#, 16#8c#, 16#3a#, 16#81#, 16#89#, 16#33#, 16#80#, 16#3b#,
         16#95#, 16#25#, 16#98#, 16#2a#, 16#90#, 16#23#, 16#99#, 16#2b#,
         16#e5#, 16#cc#, 16#e8#, 16#c1#, 16#c9#, 16#e0#, 16#c0#, 16#e9#,
         16#d5#, 16#f5#, 16#d8#, 16#f8#, 16#d0#, 16#f0#, 16#d9#, 16#f9#,
         16#a5#, 16#1c#, 16#a8#, 16#12#, 16#1b#, 16#a0#, 16#13#, 16#a9#,
         16#05#, 16#b5#, 16#0a#, 16#b8#, 16#03#, 16#b0#, 16#0b#, 16#b9#,
         16#32#, 16#88#, 16#3c#, 16#85#, 16#8d#, 16#34#, 16#84#, 16#3d#,
         16#91#, 16#22#, 16#9c#, 16#2c#, 16#94#, 16#24#, 16#9d#, 16#2d#,
         16#62#, 16#4a#, 16#6c#, 16#45#, 16#4d#, 16#64#, 16#44#, 16#6d#,
         16#52#, 16#72#, 16#5c#, 16#7c#, 16#54#, 16#74#, 16#5d#, 16#7d#,
         16#a1#, 16#1a#, 16#ac#, 16#15#, 16#1d#, 16#a4#, 16#14#, 16#ad#,
         16#02#, 16#b1#, 16#0c#, 16#bc#, 16#04#, 16#b4#, 16#0d#, 16#bd#,
         16#e1#, 16#c8#, 16#ec#, 16#c5#, 16#cd#, 16#e4#, 16#c4#, 16#ed#,
         16#d1#, 16#f1#, 16#dc#, 16#fc#, 16#d4#, 16#f4#, 16#dd#, 16#fd#,
         16#36#, 16#8e#, 16#38#, 16#82#, 16#8b#, 16#30#, 16#83#, 16#39#,
         16#96#, 16#26#, 16#9a#, 16#28#, 16#93#, 16#20#, 16#9b#, 16#29#,
         16#66#, 16#4e#, 16#68#, 16#41#, 16#49#, 16#60#, 16#40#, 16#69#,
         16#56#, 16#76#, 16#58#, 16#78#, 16#50#, 16#70#, 16#59#, 16#79#,
         16#a6#, 16#1e#, 16#aa#, 16#11#, 16#19#, 16#a3#, 16#10#, 16#ab#,
         16#06#, 16#b6#, 16#08#, 16#ba#, 16#00#, 16#b3#, 16#09#, 16#bb#,
         16#e6#, 16#ce#, 16#ea#, 16#c2#, 16#cb#, 16#e3#, 16#c3#, 16#eb#,
         16#d6#, 16#f6#, 16#da#, 16#fa#, 16#d3#, 16#f3#, 16#db#, 16#fb#,
         16#31#, 16#8a#, 16#3e#, 16#86#, 16#8f#, 16#37#, 16#87#, 16#3f#,
         16#92#, 16#21#, 16#9e#, 16#2e#, 16#97#, 16#27#, 16#9f#, 16#2f#,
         16#61#, 16#48#, 16#6e#, 16#46#, 16#4f#, 16#67#, 16#47#, 16#6f#,
         16#51#, 16#71#, 16#5e#, 16#7e#, 16#57#, 16#77#, 16#5f#, 16#7f#,
         16#a2#, 16#18#, 16#ae#, 16#16#, 16#1f#, 16#a7#, 16#17#, 16#af#,
         16#01#, 16#b2#, 16#0e#, 16#be#, 16#07#, 16#b7#, 16#0f#, 16#bf#,
         16#e2#, 16#ca#, 16#ee#, 16#c6#, 16#cf#, 16#e7#, 16#c7#, 16#ef#,
         16#d2#, 16#f2#, 16#de#, 16#fe#, 16#d7#, 16#f7#, 16#df#, 16#ff# );
   begin
      return Table(Cell);
   end Sbox;

   function InvSbox(Cell : Cell_T) return Cell_T is
      Table : constant Sbox_T :=
        (
         16#ac#, 16#e8#, 16#68#, 16#3c#, 16#6c#, 16#38#, 16#a8#, 16#ec#,
         16#aa#, 16#ae#, 16#3a#, 16#3e#, 16#6a#, 16#6e#, 16#ea#, 16#ee#,
         16#a6#, 16#a3#, 16#33#, 16#36#, 16#66#, 16#63#, 16#e3#, 16#e6#,
         16#e1#, 16#a4#, 16#61#, 16#34#, 16#31#, 16#64#, 16#a1#, 16#e4#,
         16#8d#, 16#c9#, 16#49#, 16#1d#, 16#4d#, 16#19#, 16#89#, 16#cd#,
         16#8b#, 16#8f#, 16#1b#, 16#1f#, 16#4b#, 16#4f#, 16#cb#, 16#cf#,
         16#85#, 16#c0#, 16#40#, 16#15#, 16#45#, 16#10#, 16#80#, 16#c5#,
         16#82#, 16#87#, 16#12#, 16#17#, 16#42#, 16#47#, 16#c2#, 16#c7#,
         16#96#, 16#93#, 16#03#, 16#06#, 16#56#, 16#53#, 16#d3#, 16#d6#,
         16#d1#, 16#94#, 16#51#, 16#04#, 16#01#, 16#54#, 16#91#, 16#d4#,
         16#9c#, 16#d8#, 16#58#, 16#0c#, 16#5c#, 16#08#, 16#98#, 16#dc#,
         16#9a#, 16#9e#, 16#0a#, 16#0e#, 16#5a#, 16#5e#, 16#da#, 16#de#,
         16#95#, 16#d0#, 16#50#, 16#05#, 16#55#, 16#00#, 16#90#, 16#d5#,
         16#92#, 16#97#, 16#02#, 16#07#, 16#52#, 16#57#, 16#d2#, 16#d7#,
         16#9d#, 16#d9#, 16#59#, 16#0d#, 16#5d#, 16#09#, 16#99#, 16#dd#,
         16#9b#, 16#9f#, 16#0b#, 16#0f#, 16#5b#, 16#5f#, 16#db#, 16#df#,
         16#16#, 16#13#, 16#83#, 16#86#, 16#46#, 16#43#, 16#c3#, 16#c6#,
         16#41#, 16#14#, 16#c1#, 16#84#, 16#11#, 16#44#, 16#81#, 16#c4#,
         16#1c#, 16#48#, 16#c8#, 16#8c#, 16#4c#, 16#18#, 16#88#, 16#cc#,
         16#1a#, 16#1e#, 16#8a#, 16#8e#, 16#4a#, 16#4e#, 16#ca#, 16#ce#,
         16#35#, 16#60#, 16#e0#, 16#a5#, 16#65#, 16#30#, 16#a0#, 16#e5#,
         16#32#, 16#37#, 16#a2#, 16#a7#, 16#62#, 16#67#, 16#e2#, 16#e7#,
         16#3d#, 16#69#, 16#e9#, 16#ad#, 16#6d#, 16#39#, 16#a9#, 16#ed#,
         16#3b#, 16#3f#, 16#ab#, 16#af#, 16#6b#, 16#6f#, 16#eb#, 16#ef#,
         16#26#, 16#23#, 16#b3#, 16#b6#, 16#76#, 16#73#, 16#f3#, 16#f6#,
         16#71#, 16#24#, 16#f1#, 16#b4#, 16#21#, 16#74#, 16#b1#, 16#f4#,
         16#2c#, 16#78#, 16#f8#, 16#bc#, 16#7c#, 16#28#, 16#b8#, 16#fc#,
         16#2a#, 16#2e#, 16#ba#, 16#be#, 16#7a#, 16#7e#, 16#fa#, 16#fe#,
         16#25#, 16#70#, 16#f0#, 16#b5#, 16#75#, 16#20#, 16#b0#, 16#f5#,
         16#22#, 16#27#, 16#b2#, 16#b7#, 16#72#, 16#77#, 16#f2#, 16#f7#,
         16#2d#, 16#79#, 16#f9#, 16#bd#, 16#7d#, 16#29#, 16#b9#, 16#fd#,
         16#2b#, 16#2f#, 16#bb#, 16#bf#, 16#7b#, 16#7f#, 16#fb#, 16#ff# );
   begin
      return Table(Cell);
   end InvSbox;


   function Initialize (Tweakey : in Tweakey_T) return Context_T is
      Context : Context_T;
      Current_State : Tweakey_State_T;
   begin
      Current_State := Load_Tweakey(Tweakey);
      for i in Context.Round_Tweakeys'Range loop
         Context.Round_Tweakeys(i) := Get_Round_Tweakey(Current_State);
--           Put("DBG Context(" & Integer'Image(i) & "): ");
--           Put(To_Hex(A => u8_Array(Context.Round_Tweakeys(i))));
--           New_Line;
         Current_State := TK_Update(Current_State);
      end loop;
      return Context;
   end Initialize;

   function Encrypt
     (Context : in Context_T;
      Block   : in Block_T) return Block_T is
      State : State_T := (Row_T(Block( 1 .. 4)),
                          Row_T(Block( 5 ..  8)),
                          Row_T(Block( 9 .. 12)),
                          Row_T(Block(13 .. 16)));
      Tmp : Row_T;
      Block_Out : Block_T;
   begin
      for Round in 1 .. Rounds loop
         -- Sbox application
         for i in State'Range loop
            for j in State(i)'Range loop
               State(i)(j) := Sbox(State(i)(j));
            end loop;
         end loop;
         -- AddConstant
         State(1)(1) := State(1)(1) xor (Round_Constant(Round) and 16#0f#);
         State(2)(1) := State(2)(1) xor (Shift_Right(Round_Constant(Round), 4));
         State(3)(1) := State(3)(1) xor 16#02#;
         -- AddRoundTweakey
         State(1) := State(1) xor Row_T(Context.Round_Tweakeys(Round)(1 .. 4));
         State(2) := State(2) xor Row_T(Context.Round_Tweakeys(Round)(5 .. 8));
         -- ShiftRows
         State(2) := State(2)(4)      & State(2)(1 .. 3);
         State(3) := State(3)(3 .. 4) & State(3)(1 .. 2);
         State(4) := State(4)(2 .. 4) & State(4)(1);
         -- MixColumns
         Tmp := State(3) xor State(1);
         State(3) := State(3) xor State(2);
         State(2) := State(1);
         State(1) := Tmp xor State(4);
         State(4) := Tmp;
      end loop;
      Block_Out := (
                    u8_Array(State(1)) &
                      u8_Array(State(2)) &
                      u8_Array(State(3)) &
                      u8_Array(State(4)) );
      return Block_Out;
   end Encrypt;

   function Decrypt
     (Context : in Context_T;
      Block   : in Block_T) return Block_T is
      State : State_T := (Row_T(Block( 1 .. 4)),
                          Row_T(Block( 5 ..  8)),
                          Row_T(Block( 9 .. 12)),
                          Row_T(Block(13 .. 16)));
      Tmp : Row_T;
      Block_Out : Block_T;
   begin
      for Round in reverse 1 .. Rounds loop
         -- MixColumns
         Tmp := State(4) xor State(2);
         State(4) := State(4) xor State(1);
         State(1) := State(2);
         State(2) := Tmp xor State(3);
         State(3) := Tmp;
         -- ShiftRows
         State(2) := State(2)(2 .. 4) & State(2)(1);
         State(3) := State(3)(3 .. 4) & State(3)(1 .. 2);
         State(4) := State(4)(4)      & State(4)(1 .. 3);
         -- AddRoundTweakey
         State(1) := State(1) xor Row_T(Context.Round_Tweakeys(Round)(1 .. 4));
         State(2) := State(2) xor Row_T(Context.Round_Tweakeys(Round)(5 .. 8));
         -- AddConstant
         State(1)(1) := State(1)(1) xor (Round_Constant(Round) and 16#0f#);
         State(2)(1) := State(2)(1) xor (Shift_Right(Round_Constant(Round), 4));
         State(3)(1) := State(3)(1) xor 16#02#;
         -- Sbox application
         for i in State'Range loop
            for j in State(i)'Range loop
               State(i)(j) := InvSbox(State(i)(j));
            end loop;
         end loop;
      end loop;
      Block_Out := (
                    u8_Array(State(1)) &
                      u8_Array(State(2)) &
                      u8_Array(State(3)) &
                      u8_Array(State(4)) );
      return Block_Out;
   end Decrypt;

end Skinny128_Generic;
