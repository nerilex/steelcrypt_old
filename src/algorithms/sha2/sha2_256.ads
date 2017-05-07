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

with SHA2_256_Spec; use SHA2_256_Spec;
with Hash_Generic;

package SHA2_256 is new Hash_Generic
  (Name_Intern => Name,
   Context_T_Intern => Context_T,
   Digest_Size_Bits_Intern => Digest_Size_Bits,
   Block_Size_Bits_Intern => Block_Size_Bits,
   Initialize_Intern => Initialize,
   Next_Block_Intern => Next_Block,
   Last_Block_Intern => Last_Block,
   Get_Digest_Intern => Get_Digest);
