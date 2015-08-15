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

with SHA3_Generic;

package SHA3 is

   package SHA3_224 is new SHA3_Generic(Capacity_Bits => 448);
   package SHA3_256 is new SHA3_Generic(Capacity_Bits => 512);
   package SHA3_384 is new SHA3_Generic(Capacity_Bits => 768);
   package SHA3_512 is new SHA3_Generic(Capacity_Bits => 1024);

end SHA3;
