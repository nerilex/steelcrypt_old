--  Copyright (C) 2016  bg nerilex <bg@nerilex.org>
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

with Crypto.Types; use Crypto.Types;
with Crypto.Types.X;
with Skinny_Types;      use Skinny_Types;
with Crypto.Types.Generic_Block_Utils;
use Crypto.Types.X.Utils_u8;

package Skinny128_Types is
   use type Crypto.Types.u8;
   subtype Cell_T is u8;
--   type State_T is array (State_Index_T) of Cell_T;
--   package Skinny_Utils is new Crypto.Types.Generic_Block_Utils(T            => Cell_T,
--                                                          T_Array_Index => State_Index_T,
--                                                T_Array      => State_T);
   type Round_Tweakey_T is array (1 .. 8) of Cell_T;
--   type Row_Index_T is new Integer range 1 .. 4;
--   type Column_Index_T is new Integer range 1 .. 4;
   type Row_T is Array (Column_Index_T) of Cell_T;
   type State_T is Array (Row_Index_T) of Row_T;
--     package Row_Utils is new Crypto.Types.Generic_Block_Utils(T => Cell_T,
--                                                         T_Array => Row_T,
--                                                         T_Array_Index => Column_Index_T);
--   function Shift_Left(Value : Cell_T; Amount : Natural) return Cell_T renames Crypto.Types.Shift_Left;
   package Row_Utils is new Crypto.Types.Generic_Block_Utils
     (T => Cell_T,
      T_Array => Row_T,
      T_Array_Index => Column_Index_T,
      Shift_Left => Crypto.Types.Shift_Left,
      Shift_Right => Crypto.Types.Shift_Right,
      Rotate_Left => Crypto.Types.Rotate_Left,
      Rotate_Right => Crypto.Types.Rotate_Right);

end Skinny128_Types;
