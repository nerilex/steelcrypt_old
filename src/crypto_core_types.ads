with Interfaces; use Interfaces;

package Crypto_Core_Types is

   type u8  is new Unsigned_8;
   for u8'Size use 8;

   type u16 is new Unsigned_16;
   for u16'Size use 16;

   type u32 is new Unsigned_32;
   for u32'Size use 32;

   type u64 is new Unsigned_64;
   for u64'Size use 64;

   type u8_Array  is Array (Integer range <>) of u8;
   type u16_Array is Array (Integer range <>) of u16;
   type u32_Array is Array (Integer range <>) of u32;
   type u64_Array is Array (Integer range <>) of u64;

   type u8_Array_Access  is access all u8_Array;
   type u16_Array_Access is access all u16_Array;
   type u32_Array_Access is access all u32_Array;
   type u64_Array_Access is access all u64_Array;

end Crypto_Core_Types;
