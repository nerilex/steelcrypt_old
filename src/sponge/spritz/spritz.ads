with Crypto_Core_Types; use Crypto_Core_Types;
with Crypto_Types; use Crypto_Types;

use Crypto_Types.Crypto_Types_u8;

package Spritz is

   type Context is private;

   procedure InitializeContext (ctx : out Context);
   procedure AbsorbStop (ctx : in out Context);
   procedure Absorb (ctx : in out Context; x : in u8);
   procedure Absorb (ctx : in out Context; x : in u8_Array);
   procedure Absorb (ctx : in out Context; x : in String);
   procedure Drip (ctx : in out Context; z : out u8);
   procedure Squeeze (ctx : in out Context; P : out u8_Array);

private

   N : constant Integer:= 256;

   type S_Array is Array (u8 range <>) of u8;

   type Context is record
      S : S_Array (0 .. u8(N - 1));
      i, j, k, z, w, a : u8;
   end record;

end Spritz;
