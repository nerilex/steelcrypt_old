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

package body Spritz_Stream is

   procedure Initialize (ctx : out Context; key : in u8_Array) is
   begin
      Spritz.Initialize(Spritz.Context(ctx.ctx));
      Spritz.Absorb(Spritz.Context(ctx.ctx), key);
   end Initialize;

   procedure Initialize (ctx : out Context; key : in String) is
   begin
      Spritz.Initialize(Spritz.Context(ctx.ctx));
      Spritz.Absorb(Spritz.Context(ctx.ctx), key);
   end Initialize;

   procedure Set_Iv (ctx : in out Context; iv : in u8_Array) is
   begin
      Spritz.AbsorbStop(Spritz.Context(ctx.ctx));
      Spritz.Absorb(Spritz.Context(ctx.ctx), iv);
   end Set_IV;

   procedure Encrypt (ctx : in out Context; data : in out u8_Array) is
      z : u8;
   begin
      for i in data'Range loop
         Spritz.Drip(Spritz.Context(ctx.ctx), z);
         data(i) := data(i) + z;
      end loop;
   end Encrypt;

   procedure Decrypt (ctx : in out Context; data : in out u8_Array) is
      z : u8;
   begin
      for i in data'Range loop
         Spritz.Drip(Spritz.Context(ctx.ctx), z);
         data(i) := data(i) - z;
      end loop;
   end Decrypt;

end Spritz_Stream;