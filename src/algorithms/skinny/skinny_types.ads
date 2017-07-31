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

package Skinny_Types is

   type State_Index_T is new Integer range 1 .. 16;
   type State_Index_Map_T is array (State_Index_T) of State_Index_T;
   subtype Row_Index_T    is Integer range 1 .. 4;
   subtype Column_Index_T is Integer range 1 .. 4;

end Skinny_Types;
