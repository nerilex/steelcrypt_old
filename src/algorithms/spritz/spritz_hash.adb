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

package body Spritz_Hash is

   procedure Initialize (ctx : out Context) is
   begin
      Spritz.Initialize (Spritz.Context (ctx.ctx));
   end Initialize;

   procedure Add_Domain (ctx : in out Context; Domain : in String) is
   begin
      Spritz.Absorb (Spritz.Context (ctx.ctx), Domain);
      Spritz.AbsorbStop (Spritz.Context (ctx.ctx));
   end Add_Domain;

   procedure Add_Domain (ctx : in out Context; Domain : in u8_Array) is
   begin
      Spritz.Absorb (Spritz.Context (ctx.ctx), Domain);
      Spritz.AbsorbStop (Spritz.Context (ctx.ctx));
   end Add_Domain;

   procedure Add_Data (ctx : in out Context; data : in u8_Array) is
   begin
      Spritz.Absorb (Spritz.Context (ctx.ctx), data);
   end Add_Data;

   procedure Add_Data (ctx : in out Context; data : in String) is
   begin
      Spritz.Absorb (Spritz.Context (ctx.ctx), data);
   end Add_Data;

   procedure Extract_Hash (ctx : in out Context; Hash : out u8_Array) is
   begin
      Spritz.AbsorbStop (Spritz.Context (ctx.ctx));
      Spritz.Absorb (Spritz.Context (ctx.ctx), Natural (Hash'Length));
      Spritz.Squeeze (Spritz.Context (ctx.ctx), Hash);
   end Extract_Hash;

end Spritz_Hash;
