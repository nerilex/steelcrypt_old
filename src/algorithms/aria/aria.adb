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

package body ARIA is

   SBox : constant array (Integer range 1 .. 4, u8 range 0 .. 255) of u8 := ( (
      16#63#, 16#7c#, 16#77#, 16#7b#, 16#f2#, 16#6b#, 16#6f#, 16#c5#, 16#30#, 16#01#, 16#67#, 16#2b#, 16#fe#, 16#d7#, 16#ab#, 16#76#,
      16#ca#, 16#82#, 16#c9#, 16#7d#, 16#fa#, 16#59#, 16#47#, 16#f0#, 16#ad#, 16#d4#, 16#a2#, 16#af#, 16#9c#, 16#a4#, 16#72#, 16#c0#,
      16#b7#, 16#fd#, 16#93#, 16#26#, 16#36#, 16#3f#, 16#f7#, 16#cc#, 16#34#, 16#a5#, 16#e5#, 16#f1#, 16#71#, 16#d8#, 16#31#, 16#15#,
      16#04#, 16#c7#, 16#23#, 16#c3#, 16#18#, 16#96#, 16#05#, 16#9a#, 16#07#, 16#12#, 16#80#, 16#e2#, 16#eb#, 16#27#, 16#b2#, 16#75#,
      16#09#, 16#83#, 16#2c#, 16#1a#, 16#1b#, 16#6e#, 16#5a#, 16#a0#, 16#52#, 16#3b#, 16#d6#, 16#b3#, 16#29#, 16#e3#, 16#2f#, 16#84#,
      16#53#, 16#d1#, 16#00#, 16#ed#, 16#20#, 16#fc#, 16#b1#, 16#5b#, 16#6a#, 16#cb#, 16#be#, 16#39#, 16#4a#, 16#4c#, 16#58#, 16#cf#,
      16#d0#, 16#ef#, 16#aa#, 16#fb#, 16#43#, 16#4d#, 16#33#, 16#85#, 16#45#, 16#f9#, 16#02#, 16#7f#, 16#50#, 16#3c#, 16#9f#, 16#a8#,
      16#51#, 16#a3#, 16#40#, 16#8f#, 16#92#, 16#9d#, 16#38#, 16#f5#, 16#bc#, 16#b6#, 16#da#, 16#21#, 16#10#, 16#ff#, 16#f3#, 16#d2#,
      16#cd#, 16#0c#, 16#13#, 16#ec#, 16#5f#, 16#97#, 16#44#, 16#17#, 16#c4#, 16#a7#, 16#7e#, 16#3d#, 16#64#, 16#5d#, 16#19#, 16#73#,
      16#60#, 16#81#, 16#4f#, 16#dc#, 16#22#, 16#2a#, 16#90#, 16#88#, 16#46#, 16#ee#, 16#b8#, 16#14#, 16#de#, 16#5e#, 16#0b#, 16#db#,
      16#e0#, 16#32#, 16#3a#, 16#0a#, 16#49#, 16#06#, 16#24#, 16#5c#, 16#c2#, 16#d3#, 16#ac#, 16#62#, 16#91#, 16#95#, 16#e4#, 16#79#,
      16#e7#, 16#c8#, 16#37#, 16#6d#, 16#8d#, 16#d5#, 16#4e#, 16#a9#, 16#6c#, 16#56#, 16#f4#, 16#ea#, 16#65#, 16#7a#, 16#ae#, 16#08#,
      16#ba#, 16#78#, 16#25#, 16#2e#, 16#1c#, 16#a6#, 16#b4#, 16#c6#, 16#e8#, 16#dd#, 16#74#, 16#1f#, 16#4b#, 16#bd#, 16#8b#, 16#8a#,
      16#70#, 16#3e#, 16#b5#, 16#66#, 16#48#, 16#03#, 16#f6#, 16#0e#, 16#61#, 16#35#, 16#57#, 16#b9#, 16#86#, 16#c1#, 16#1d#, 16#9e#,
      16#e1#, 16#f8#, 16#98#, 16#11#, 16#69#, 16#d9#, 16#8e#, 16#94#, 16#9b#, 16#1e#, 16#87#, 16#e9#, 16#ce#, 16#55#, 16#28#, 16#df#,
      16#8c#, 16#a1#, 16#89#, 16#0d#, 16#bf#, 16#e6#, 16#42#, 16#68#, 16#41#, 16#99#, 16#2d#, 16#0f#, 16#b0#, 16#54#, 16#bb#, 16#16#
   ),
   (
      16#e2#, 16#4e#, 16#54#, 16#fc#, 16#94#, 16#c2#, 16#4a#, 16#cc#, 16#62#, 16#0d#, 16#6a#, 16#46#, 16#3c#, 16#4d#, 16#8b#, 16#d1#,
      16#5e#, 16#fa#, 16#64#, 16#cb#, 16#b4#, 16#97#, 16#be#, 16#2b#, 16#bc#, 16#77#, 16#2e#, 16#03#, 16#d3#, 16#19#, 16#59#, 16#c1#,
      16#1d#, 16#06#, 16#41#, 16#6b#, 16#55#, 16#f0#, 16#99#, 16#69#, 16#ea#, 16#9c#, 16#18#, 16#ae#, 16#63#, 16#df#, 16#e7#, 16#bb#,
      16#00#, 16#73#, 16#66#, 16#fb#, 16#96#, 16#4c#, 16#85#, 16#e4#, 16#3a#, 16#09#, 16#45#, 16#aa#, 16#0f#, 16#ee#, 16#10#, 16#eb#,
      16#2d#, 16#7f#, 16#f4#, 16#29#, 16#ac#, 16#cf#, 16#ad#, 16#91#, 16#8d#, 16#78#, 16#c8#, 16#95#, 16#f9#, 16#2f#, 16#ce#, 16#cd#,
      16#08#, 16#7a#, 16#88#, 16#38#, 16#5c#, 16#83#, 16#2a#, 16#28#, 16#47#, 16#db#, 16#b8#, 16#c7#, 16#93#, 16#a4#, 16#12#, 16#53#,
      16#ff#, 16#87#, 16#0e#, 16#31#, 16#36#, 16#21#, 16#58#, 16#48#, 16#01#, 16#8e#, 16#37#, 16#74#, 16#32#, 16#ca#, 16#e9#, 16#b1#,
      16#b7#, 16#ab#, 16#0c#, 16#d7#, 16#c4#, 16#56#, 16#42#, 16#26#, 16#07#, 16#98#, 16#60#, 16#d9#, 16#b6#, 16#b9#, 16#11#, 16#40#,
      16#ec#, 16#20#, 16#8c#, 16#bd#, 16#a0#, 16#c9#, 16#84#, 16#04#, 16#49#, 16#23#, 16#f1#, 16#4f#, 16#50#, 16#1f#, 16#13#, 16#dc#,
      16#d8#, 16#c0#, 16#9e#, 16#57#, 16#e3#, 16#c3#, 16#7b#, 16#65#, 16#3b#, 16#02#, 16#8f#, 16#3e#, 16#e8#, 16#25#, 16#92#, 16#e5#,
      16#15#, 16#dd#, 16#fd#, 16#17#, 16#a9#, 16#bf#, 16#d4#, 16#9a#, 16#7e#, 16#c5#, 16#39#, 16#67#, 16#fe#, 16#76#, 16#9d#, 16#43#,
      16#a7#, 16#e1#, 16#d0#, 16#f5#, 16#68#, 16#f2#, 16#1b#, 16#34#, 16#70#, 16#05#, 16#a3#, 16#8a#, 16#d5#, 16#79#, 16#86#, 16#a8#,
      16#30#, 16#c6#, 16#51#, 16#4b#, 16#1e#, 16#a6#, 16#27#, 16#f6#, 16#35#, 16#d2#, 16#6e#, 16#24#, 16#16#, 16#82#, 16#5f#, 16#da#,
      16#e6#, 16#75#, 16#a2#, 16#ef#, 16#2c#, 16#b2#, 16#1c#, 16#9f#, 16#5d#, 16#6f#, 16#80#, 16#0a#, 16#72#, 16#44#, 16#9b#, 16#6c#,
      16#90#, 16#0b#, 16#5b#, 16#33#, 16#7d#, 16#5a#, 16#52#, 16#f3#, 16#61#, 16#a1#, 16#f7#, 16#b0#, 16#d6#, 16#3f#, 16#7c#, 16#6d#,
      16#ed#, 16#14#, 16#e0#, 16#a5#, 16#3d#, 16#22#, 16#b3#, 16#f8#, 16#89#, 16#de#, 16#71#, 16#1a#, 16#af#, 16#ba#, 16#b5#, 16#81#
   ),
   (
      16#52#, 16#09#, 16#6a#, 16#d5#, 16#30#, 16#36#, 16#a5#, 16#38#, 16#bf#, 16#40#, 16#a3#, 16#9e#, 16#81#, 16#f3#, 16#d7#, 16#fb#,
      16#7c#, 16#e3#, 16#39#, 16#82#, 16#9b#, 16#2f#, 16#ff#, 16#87#, 16#34#, 16#8e#, 16#43#, 16#44#, 16#c4#, 16#de#, 16#e9#, 16#cb#,
      16#54#, 16#7b#, 16#94#, 16#32#, 16#a6#, 16#c2#, 16#23#, 16#3d#, 16#ee#, 16#4c#, 16#95#, 16#0b#, 16#42#, 16#fa#, 16#c3#, 16#4e#,
      16#08#, 16#2e#, 16#a1#, 16#66#, 16#28#, 16#d9#, 16#24#, 16#b2#, 16#76#, 16#5b#, 16#a2#, 16#49#, 16#6d#, 16#8b#, 16#d1#, 16#25#,
      16#72#, 16#f8#, 16#f6#, 16#64#, 16#86#, 16#68#, 16#98#, 16#16#, 16#d4#, 16#a4#, 16#5c#, 16#cc#, 16#5d#, 16#65#, 16#b6#, 16#92#,
      16#6c#, 16#70#, 16#48#, 16#50#, 16#fd#, 16#ed#, 16#b9#, 16#da#, 16#5e#, 16#15#, 16#46#, 16#57#, 16#a7#, 16#8d#, 16#9d#, 16#84#,
      16#90#, 16#d8#, 16#ab#, 16#00#, 16#8c#, 16#bc#, 16#d3#, 16#0a#, 16#f7#, 16#e4#, 16#58#, 16#05#, 16#b8#, 16#b3#, 16#45#, 16#06#,
      16#d0#, 16#2c#, 16#1e#, 16#8f#, 16#ca#, 16#3f#, 16#0f#, 16#02#, 16#c1#, 16#af#, 16#bd#, 16#03#, 16#01#, 16#13#, 16#8a#, 16#6b#,
      16#3a#, 16#91#, 16#11#, 16#41#, 16#4f#, 16#67#, 16#dc#, 16#ea#, 16#97#, 16#f2#, 16#cf#, 16#ce#, 16#f0#, 16#b4#, 16#e6#, 16#73#,
      16#96#, 16#ac#, 16#74#, 16#22#, 16#e7#, 16#ad#, 16#35#, 16#85#, 16#e2#, 16#f9#, 16#37#, 16#e8#, 16#1c#, 16#75#, 16#df#, 16#6e#,
      16#47#, 16#f1#, 16#1a#, 16#71#, 16#1d#, 16#29#, 16#c5#, 16#89#, 16#6f#, 16#b7#, 16#62#, 16#0e#, 16#aa#, 16#18#, 16#be#, 16#1b#,
      16#fc#, 16#56#, 16#3e#, 16#4b#, 16#c6#, 16#d2#, 16#79#, 16#20#, 16#9a#, 16#db#, 16#c0#, 16#fe#, 16#78#, 16#cd#, 16#5a#, 16#f4#,
      16#1f#, 16#dd#, 16#a8#, 16#33#, 16#88#, 16#07#, 16#c7#, 16#31#, 16#b1#, 16#12#, 16#10#, 16#59#, 16#27#, 16#80#, 16#ec#, 16#5f#,
      16#60#, 16#51#, 16#7f#, 16#a9#, 16#19#, 16#b5#, 16#4a#, 16#0d#, 16#2d#, 16#e5#, 16#7a#, 16#9f#, 16#93#, 16#c9#, 16#9c#, 16#ef#,
      16#a0#, 16#e0#, 16#3b#, 16#4d#, 16#ae#, 16#2a#, 16#f5#, 16#b0#, 16#c8#, 16#eb#, 16#bb#, 16#3c#, 16#83#, 16#53#, 16#99#, 16#61#,
      16#17#, 16#2b#, 16#04#, 16#7e#, 16#ba#, 16#77#, 16#d6#, 16#26#, 16#e1#, 16#69#, 16#14#, 16#63#, 16#55#, 16#21#, 16#0c#, 16#7d#
   ),
   (
      16#30#, 16#68#, 16#99#, 16#1b#, 16#87#, 16#b9#, 16#21#, 16#78#, 16#50#, 16#39#, 16#db#, 16#e1#, 16#72#, 16#09#, 16#62#, 16#3c#,
      16#3e#, 16#7e#, 16#5e#, 16#8e#, 16#f1#, 16#a0#, 16#cc#, 16#a3#, 16#2a#, 16#1d#, 16#fb#, 16#b6#, 16#d6#, 16#20#, 16#c4#, 16#8d#,
      16#81#, 16#65#, 16#f5#, 16#89#, 16#cb#, 16#9d#, 16#77#, 16#c6#, 16#57#, 16#43#, 16#56#, 16#17#, 16#d4#, 16#40#, 16#1a#, 16#4d#,
      16#c0#, 16#63#, 16#6c#, 16#e3#, 16#b7#, 16#c8#, 16#64#, 16#6a#, 16#53#, 16#aa#, 16#38#, 16#98#, 16#0c#, 16#f4#, 16#9b#, 16#ed#,
      16#7f#, 16#22#, 16#76#, 16#af#, 16#dd#, 16#3a#, 16#0b#, 16#58#, 16#67#, 16#88#, 16#06#, 16#c3#, 16#35#, 16#0d#, 16#01#, 16#8b#,
      16#8c#, 16#c2#, 16#e6#, 16#5f#, 16#02#, 16#24#, 16#75#, 16#93#, 16#66#, 16#1e#, 16#e5#, 16#e2#, 16#54#, 16#d8#, 16#10#, 16#ce#,
      16#7a#, 16#e8#, 16#08#, 16#2c#, 16#12#, 16#97#, 16#32#, 16#ab#, 16#b4#, 16#27#, 16#0a#, 16#23#, 16#df#, 16#ef#, 16#ca#, 16#d9#,
      16#b8#, 16#fa#, 16#dc#, 16#31#, 16#6b#, 16#d1#, 16#ad#, 16#19#, 16#49#, 16#bd#, 16#51#, 16#96#, 16#ee#, 16#e4#, 16#a8#, 16#41#,
      16#da#, 16#ff#, 16#cd#, 16#55#, 16#86#, 16#36#, 16#be#, 16#61#, 16#52#, 16#f8#, 16#bb#, 16#0e#, 16#82#, 16#48#, 16#69#, 16#9a#,
      16#e0#, 16#47#, 16#9e#, 16#5c#, 16#04#, 16#4b#, 16#34#, 16#15#, 16#79#, 16#26#, 16#a7#, 16#de#, 16#29#, 16#ae#, 16#92#, 16#d7#,
      16#84#, 16#e9#, 16#d2#, 16#ba#, 16#5d#, 16#f3#, 16#c5#, 16#b0#, 16#bf#, 16#a4#, 16#3b#, 16#71#, 16#44#, 16#46#, 16#2b#, 16#fc#,
      16#eb#, 16#6f#, 16#d5#, 16#f6#, 16#14#, 16#fe#, 16#7c#, 16#70#, 16#5a#, 16#7d#, 16#fd#, 16#2f#, 16#18#, 16#83#, 16#16#, 16#a5#,
      16#91#, 16#1f#, 16#05#, 16#95#, 16#74#, 16#a9#, 16#c1#, 16#5b#, 16#4a#, 16#85#, 16#6d#, 16#13#, 16#07#, 16#4f#, 16#4e#, 16#45#,
      16#b2#, 16#0f#, 16#c9#, 16#1c#, 16#a6#, 16#bc#, 16#ec#, 16#73#, 16#90#, 16#7b#, 16#cf#, 16#59#, 16#8f#, 16#a1#, 16#f9#, 16#2d#,
      16#f2#, 16#b1#, 16#00#, 16#94#, 16#37#, 16#9f#, 16#d0#, 16#2e#, 16#9c#, 16#6e#, 16#28#, 16#3f#, 16#80#, 16#f0#, 16#3d#, 16#d3#,
      16#25#, 16#8a#, 16#b5#, 16#e7#, 16#42#, 16#b3#, 16#c7#, 16#ea#, 16#f7#, 16#4c#, 16#11#, 16#33#, 16#03#, 16#a2#, 16#ac#, 16#60#
   ) );

   type Block_Index_T is mod 16;

   A_Table : constant array (1 .. 16,  1 .. 7) of Block_Index_T := (
      ( 3 , 4 , 6 , 8  , 9  , 13 , 14),
      ( 2 , 5 , 7 , 8  , 9  , 12 , 15),
      ( 1 , 4 , 6 , 10 , 11 , 12 , 15),
      ( 0 , 5 , 7 , 10 , 11 , 13 , 14),
      ( 0 , 2 , 5 , 8  , 11 , 14 , 15),
      ( 1 , 3 , 4 , 9  , 10 , 14 , 15),
      ( 0 , 2 , 7 , 9  , 10 , 12 , 13),
      ( 1 , 3 , 6 , 8  , 11 , 12 , 13),
      ( 0 , 1 , 4 , 7  , 10 , 13 , 15),
      ( 0 , 1 , 5 , 6  , 11 , 12 , 14),
      ( 2 , 3 , 5 , 6  , 8  , 13 , 15),
      ( 2 , 3 , 4 , 7  , 9  , 12 , 14),
      ( 1 , 2 , 6 , 7  , 9  , 11 , 12),
      ( 0 , 3 , 6 , 7  , 8  , 10 , 13),
      ( 0 , 3 , 4 , 5  , 9  , 11 , 14),
      ( 1 , 2 , 4 , 5  , 8  , 10 , 15)
   );

   C : constant array (1 .. 3) of Block_128_Bit :=
     (  ( 16#51#, 16#7c#, 16#c1#, 16#b7#, 16#27#, 16#22#, 16#0a#, 16#94#, 16#fe#, 16#13#, 16#ab#, 16#e8#, 16#fa#, 16#9a#, 16#6e#, 16#e0# ),
        ( 16#6d#, 16#b1#, 16#4a#, 16#cc#, 16#9e#, 16#21#, 16#c8#, 16#20#, 16#ff#, 16#28#, 16#b1#, 16#d5#, 16#ef#, 16#5d#, 16#e2#, 16#b0# ),
        ( 16#db#, 16#92#, 16#37#, 16#1d#, 16#21#, 16#26#, 16#e9#, 16#70#, 16#03#, 16#24#, 16#97#, 16#75#, 16#04#, 16#e8#, 16#c9#, 16#0e# )
     );

   Rotation_Constants : constant array(1 .. 5) of Integer range -31 .. 61 :=
     ( -19, -31, 61, 31, 19 );

   procedure Substitute_Odd(Block : in out Block_128_Bit) is
   begin
      for i in 0 .. 15 loop
         Block(i + 1) := SBox((i mod 4) + 1, Block(i + 1));
      end loop;
   end Substitute_Odd;

   procedure Substitute_Even(Block : in out Block_128_Bit) is
   begin
      for i in 0 .. 15 loop
         Block(i + 1) := SBox(((i + 2) mod 4) + 1, Block(i + 1));
      end loop;
   end Substitute_Even;

   procedure A(Block : in out Block_128_Bit) is
      temp : constant Block_128_Bit := Block;
   begin
      for i in A_Table'Range(1) loop
         Block(i) := 0;
         for j in A_Table'Range(2) loop
            Block(i) := Block(i) xor temp(Integer(A_Table(i, j)) + 1);
         end loop;
      end loop;
   end A;

   procedure F_Odd(Block : in out Block_128_Bit; Round_Key : in Block_128_Bit) is
   begin
      Block := Block xor Round_Key;
      Substitute_Odd(Block);
      A(Block);
   end F_Odd;

   procedure F_Even(Block : in out Block_128_Bit; Round_Key : in Block_128_Bit) is
   begin
      Block := Block xor Round_Key;
      Substitute_Even(Block);
      A(Block);
   end F_Even;

   procedure Initialize(Key : in u8_Array; Context : out Context_T) is
      C_Select : Array (1 .. 3) of Natural range 1 .. 3;
   begin
      case Key'Length is
         when 16 =>
            C_Select := (1, 2, 3);
            Context.Rounds := 12;
         when 24 =>
            C_Select := (2, 3, 1);
            Context.Rounds := 14;
         when 32 =>
            C_Select := (3, 1, 2);
            Context.Rounds := 16;
         when others => raise Constraint_Error;
      end case;
      Context.W(1) := PreKey_T(Key(1 .. 16));
      Context.W(2) := Context.W(1);
      F_Odd(Block_128_Bit(Context.W(2)), C(C_Select(1)));
--        if Key'Length > 16 then
--           Context.W(2)(1 .. Key'Last - 16) := PreKey_T(u8_Array(Context.W(2)(1 .. Key'Last - 16)) xor u8_Array(Key(17 .. Key'Last)));
--        end if;
      for i in 17 .. Key'Last loop
         Context.W(2)(i - 16) := Context.W(2)(i - 16) xor Key(i);
      end loop;
      Context.W(3) := Context.W(2);
      F_Even(Block_128_Bit(Context.W(3)), C(C_Select(2)));
      Context.W(3) := PreKey_T(u8_Array(Context.W(3)) xor u8_Array(Context.W(1)));
      Context.W(4) := Context.W(3);
      F_Odd(Block_128_Bit(Context.W(4)), C(C_Select(3)));
      Context.W(4) := PreKey_T(u8_Array(Context.W(4)) xor u8_Array(Context.W(2)));
   end Initialize;

   procedure Initialize(Key : in Key_128; Context : out Context_T) is
   begin
      Initialize(u8_Array(Key), Context);
   end Initialize;

   procedure Initialize(Key : in Key_192; Context : out Context_T) is
   begin
      Initialize(u8_Array(Key), Context);
   end Initialize;

   procedure Initialize(Key : in Key_256; Context : out Context_T) is
   begin
      Initialize(u8_Array(Key), Context);
   end Initialize;

   procedure Encrypt(Context : in Context_T; Block: in out Block_128_Bit) is
      temp_key : Block_128_Bit;
      n : Integer;
   begin
      for i in 0 .. Integer(Context.Rounds) - 2 loop
         temp_key := u8_Array(Context.W((i mod 4) + 1)) xor Rotate_be(u8_Array(Context.W(((i + 1) mod 4) + 1)), Rotation_Constants((i / 4) + 1));
         --           Put("temp_key(" & Integer'Image(i) & ": "); print_hex(temp_key); New_Line;
         if i mod 2 = 0 then
            F_Odd(Block, temp_key);
         else
            F_Even(Block, temp_Key);
         end if;
      end loop;
      n := Integer(Context.Rounds) - 1;
      temp_key := u8_Array(Context.W((n mod 4) + 1)) xor Rotate_be(u8_Array(Context.W(((n + 1) mod 4) + 1)), Rotation_Constants((n / 4) + 1));
      Block := Block xor temp_key;
      Substitute_Even(Block);
      n := n + 1;
      temp_key := u8_Array(Context.W((n mod 4) + 1)) xor Rotate_be(u8_Array(Context.W(((n + 1) mod 4) + 1)), Rotation_Constants((n / 4) + 1));
      Block := Block xor temp_key;
   end Encrypt;

   procedure Decrypt(Context : in Context_T; Block: in out Block_128_Bit) is
      temp_key : Block_128_Bit;
      n : Integer;
   begin
      n := Integer(Context.Rounds);
      temp_key := u8_Array(Context.W((n mod 4) + 1)) xor Rotate_be(u8_Array(Context.W(((n + 1) mod 4) + 1)), Rotation_Constants((n / 4) + 1));
      F_Odd(Block, temp_key);
      for i in reverse 2 .. Integer(Context.Rounds) - 1 loop
         temp_key := u8_Array(Context.W((i mod 4) + 1)) xor Rotate_be(u8_Array(Context.W(((i + 1) mod 4) + 1)), Rotation_Constants((i / 4) + 1));
         A(temp_key);
         --           Put("temp_key(" & Integer'Image(i) & ": "); print_hex(temp_key); New_Line;
         if i mod 2 = 0 then
            F_Odd(Block, temp_key);
         else
            F_Even(Block, temp_Key);
         end if;
      end loop;
      n :=  1;
      temp_key := u8_Array(Context.W((n mod 4) + 1)) xor Rotate_be(u8_Array(Context.W(((n + 1) mod 4) + 1)), Rotation_Constants((n / 4) + 1));
      A(temp_key);
      Block := Block xor temp_key;
      Substitute_Even(Block);
      n := n - 1;
      temp_key := u8_Array(Context.W((n mod 4) + 1)) xor Rotate_be(u8_Array(Context.W(((n + 1) mod 4) + 1)), Rotation_Constants((n / 4) + 1));
      Block := Block xor temp_key;
   end Decrypt;


end ARIA;
