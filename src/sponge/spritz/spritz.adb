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

package body Spritz is

   procedure InitializeContext (ctx : out Context) is
   begin
      ctx.i := 0;
      ctx.j := 0;
      ctx.k := 0;
      ctx.z := 0;
      ctx.a := 0;
      ctx.w := 1;
      for i in ctx.S'Range loop
         ctx.S(i) := u8(i);
      end loop;
   end;

   procedure Output (ctx : in out Context; z : out u8) is
   begin
      ctx.z := ctx.S(ctx.j + ctx.S(ctx.i + ctx.S(ctx.z + ctx.k)));
      z := ctx.z;
   end Output;

   procedure Update (ctx : in out Context) is
   begin
      ctx.i := ctx.i + ctx.w;
      ctx.j := ctx.k + ctx.S(ctx.j + ctx.S(ctx.i));
      ctx.k := ctx.i + ctx.k + ctx.S(ctx.j);
      Swap(ctx.S(ctx.i), ctx.S(ctx.j));
   end Update;

   procedure Crush (ctx : in out Context) is
   begin
      for v in u8 range 0 .. u8(N / 2 - 1) loop
         if ctx.S(v) > ctx.S(u8(N - 1) - v) then
            Swap(ctx.S(v), ctx.S(u8(N - 1) - v));
         end if;
      end loop;
   end Crush;

   procedure Whip (ctx : in out Context) is
   begin
      for i in 0 .. (2 * N - 1) loop
         Update(ctx);
      end loop;
      ctx.w := ctx.w + 2;
   end Whip;

   procedure Shuffle (ctx : in out Context) is
   begin
      Whip(ctx);
      Crush(ctx);
      Whip(ctx);
      Crush(ctx);
      Whip(ctx);
      ctx.a := 0;
   end Shuffle;

   procedure Drip (ctx : in out Context; z : out u8) is
   begin
      if ctx.a > 0 then
         Shuffle(ctx);
      end if;
      Update(ctx);
      Output(ctx, z);
   end Drip;

   procedure Squeeze (ctx : in out Context; P : out u8_Array) is
      z : u8;
   begin
      for i in P'Range loop
         Drip(ctx, z);
         P(i) := z;
      end loop;
   end Squeeze;

   procedure AbsorbStop (ctx : in out Context) is
   begin
      if ctx.a = u8(N / 2) then
         Shuffle(ctx);
      end if;
      ctx.a := ctx.a + 1;
   end AbsorbStop;

   procedure AbsorbNibble (ctx : in out Context; x : in u8) is
   begin
      if ctx.a = u8(N / 2) then
         Shuffle(ctx);
      end if;
      Swap(ctx.S(ctx.a), ctx.S(u8(N / 2) + x));
      ctx.a := ctx.a + 1;
   end AbsorbNibble;

   procedure Absorb (ctx : in out Context; x : in u8) is
   begin
      AbsorbNibble(ctx, x and 15);
      AbsorbNibble(ctx, Shift_Right(x, 4));
   end Absorb;

   procedure Absorb (ctx : in out Context; x : access constant u8_Array) is
   begin
      for i in x'Range loop
         Absorb(ctx, x(i));
      end loop;
   end Absorb;

   procedure Absorb (ctx : in out Context; x : access constant String) is
   begin
      for i in x'Range loop
         Absorb(ctx, u8(Character'Pos(x(i))));
      end loop;
   end Absorb;

end Spritz;
