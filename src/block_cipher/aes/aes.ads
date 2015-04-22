with Crypto_Types; use Crypto_Types;

package AES is

private
   polynom : constant u8 := 16#1B#;
   function gf256mul(a, b : u8) return u8;
end AES;
