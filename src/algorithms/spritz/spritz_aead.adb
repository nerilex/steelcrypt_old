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

package body Spritz_AEAD is

   procedure Initialize (ctx : out Context; key : in u8_Array) is
   begin
      Spritz.Initialize(ctx.ctx);
      Spritz.Absorb(ctx.ctx, key);
      Spritz.AbsorbStop(ctx.ctx);
      ctx.state := pre_nonce;
   end Initialize;

   procedure Initialize (ctx : out Context; key : in String) is
   begin
      Spritz.Initialize(ctx.ctx);
      Spritz.Absorb(ctx.ctx, key);
      Spritz.AbsorbStop(ctx.ctx);
      ctx.state := pre_nonce;
   end Initialize;

   procedure Set_Nonce (ctx : in out Context; nonce : in u8_Array) is
   begin
      if ctx.state /= pre_nonce then
         raise Wrong_Opertaion_Order;
      end if;
      Spritz.Absorb(ctx.ctx, nonce);
      Spritz.AbsorbStop(ctx.ctx);
      ctx.state := pre_header;
   end Set_Nonce;

   procedure Set_Nonce (ctx : in out Context; nonce : in String) is
   begin
      if ctx.state /= pre_nonce then
         raise Wrong_Opertaion_Order;
      end if;
      Spritz.Absorb(ctx.ctx, nonce);
      Spritz.AbsorbStop(ctx.ctx);
      ctx.state := pre_header;
   end Set_Nonce;

   procedure Add_Header (ctx : in out Context; header : in u8_Array) is
   begin
      if ctx.state /= pre_header then
         raise Wrong_Opertaion_Order;
      end if;
      Spritz.Absorb(ctx.ctx, header);
   end Add_Header;

   procedure Encrypt_Data (ctx : in out Context; data : in out u8_Array) is
      c : Integer := data'First;
   begin
      if ctx.state /= pre_header then
         Spritz.AbsorbStop(ctx.ctx);
         ctx.state := pre_data;
      end if;
      if ctx.state /= pre_data then
         raise Wrong_Opertaion_Order;
      end if;
      while c <= data'Last  loop
         if ctx.offset = 0 then
            Spritz.Squeeze(ctx.ctx, ctx.buffer);
         end if;
         data(c) := data(c) + ctx.buffer(ctx.offset);
         ctx.buffer(ctx.offset) := data(c);
         c := c + 1;
         ctx.offset := (ctx.offset + 1) mod (Spritz.N / 4);
         if ctx.offset = 0 then
            Spritz.Absorb(ctx.ctx, ctx.buffer);
         end if;
      end loop;
   end Encrypt_Data;

   procedure Extract_Tag (ctx : in out Context; tag : out u8_Array) is
   begin
      if ctx.state /= pre_data then
         raise Wrong_Opertaion_Order;
      end if;
      Spritz.AbsorbStop(ctx.ctx);
      Spritz.Absorb(ctx.ctx, Natural(tag'Length));
      Spritz.Squeeze(ctx.ctx, tag);
      ctx.state := post_tag;
   end;

   procedure Decrypt_Data (ctx : in out Context; data : in out u8_Array) is
      c : Integer := data'First;
   begin
      if ctx.state /= pre_header then
         Spritz.AbsorbStop(ctx.ctx);
         ctx.state := pre_data;
      end if;
      if ctx.state /= pre_data then
         raise Wrong_Opertaion_Order;
      end if;
      while c <= data'Last  loop
         if ctx.offset = 0 then
            Spritz.Squeeze(ctx.ctx, ctx.buffer);
         end if;
         data(c) := data(c) - ctx.buffer(ctx.offset);
         ctx.buffer(ctx.offset) := data(c);
         c := c + 1;
         ctx.offset := (ctx.offset + 1) mod (Spritz.N / 4);
         if ctx.offset = 0 then
            Spritz.Absorb(ctx.ctx, ctx.buffer);
         end if;
      end loop;
   end Decrypt_Data;

end Spritz_AEAD;