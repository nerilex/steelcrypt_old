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

with Ada.Text_IO; use Ada.Text_IO;
with Crypto_Core_Types; use Crypto_Core_Types;

generic

   type Global_Blob_T is (<>);
   type Global_Integer_T is (<>);
   type Local_Blob_T is (<>);
   type Local_Integer_T is (<>);

package Nist_Test_IO is

   type Context_T is limited private;

   type Global_Blob_Parameters    is array (Global_Blob_T) of access u8_Array;
   type Global_Integer_Parameters is array (Global_Integer_T) of Integer;
   type Local_Blob_Parameters     is array (Local_Blob_T) of access u8_Array;
   type Local_Integer_Parameters  is array (Local_Integer_T) of Integer;

   type Parameters_T is record
      Valid : Boolean;
      Global_Blob : Global_Blob_Parameters;
      Global_Integer : Global_Integer_Parameters;
      Local_Blob : Local_Blob_Parameters;
      Local_Integer : Local_Integer_Parameters;
   end record;

   procedure Open (Context : out Context_T; File_Name : String);
   procedure Get_Next(Context : in out Context_T; Parameters : out Parameters_T);

private

   type Context_T is record
      File : File_Type;
      Global_Blob : Global_Blob_Parameters;
      Global_Integer : Global_Integer_Parameters;
   end record;

end Nist_Test_IO;
