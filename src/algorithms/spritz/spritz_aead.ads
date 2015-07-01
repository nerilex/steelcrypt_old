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

with Crypto_Core_Types; use Crypto_Core_Types;
with Crypto_Types; use Crypto_Types;
with Spritz;

use Crypto_Types.Crypto_Types_u8;

package Spritz_AEAD is

   type Context is private;

   procedure Initialize (ctx : out Context; key : in u8_Array);
   procedure Initialize (ctx : out Context; key : in String);
   procedure Set_Nonce (ctx : in out Context; nonce : in u8_Array);
   procedure Set_Nonce (ctx : in out Context; nonce : in String);
   procedure Add_Header (ctx : in out Context; header : in u8_Array);
   procedure Encrypt_Data (ctx : in out Context; data : in out u8_Array);
   procedure Extract_Tag (ctx : in out Context; tag : out u8_Array);
   procedure Decrypt_Data (ctx : in out Context; data : in out u8_Array);

private

   type state_t is (pre_initialize, pre_nonce, pre_header, pre_data, post_tag);
   type Context is record
      state : state_t := pre_initialize;
      ctx : Spritz.Context;
      offset : Integer range 1 .. Spritz.N / 4;
      buffer : u8_Array(1 .. Spritz.N / 4);
   end record;

end Spritz_AEAD;
