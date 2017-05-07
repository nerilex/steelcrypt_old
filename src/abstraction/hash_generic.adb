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

package body Hash_Generic is

   procedure Hash
     (Data   : in     u8_Array;
      Digest :    out u8_Array;
      Bits   : in     Integer := -1)
   is
      Context : Context_T_Intern;
      Counter : Natural := Data'Length * 8;
      Index   : Integer := Data'First;
   begin
      if Bits >= 0 then
         Counter := Bits;
      end if;
      Initialize (Context);
      while Counter > Block_Size_Bits loop
         Next_Block (Context, Data (Index .. Index + Block_Size_Bytes - 1));
         Index   := Index + Block_Size_Bytes;
         Counter := Counter - Block_Size_Bits;
      end loop;
      Last_Block (Context, Data (Index .. Data'Last), Counter);
      Get_Digest (Context, Digest);
   end Hash;

end Hash_Generic;
