with Crypto_Generic_Types;
with Crypto_Core_Types; use Crypto_Core_Types;

package Crypto_Types is

   package Crypto_Types_u8 is new  Crypto_Generic_Types(T => u8,  T_Array => u8_Array,  T_Array_Access => u8_Array_Access);
   package Crypto_Types_u16 is new Crypto_Generic_Types(T => u16, T_Array => u16_Array, T_Array_Access => u16_Array_Access);
   package Crypto_Types_u32 is new Crypto_Generic_Types(T => u32, T_Array => u32_Array, T_Array_Access => u32_Array_Access);
   package Crypto_Types_u64 is new Crypto_Generic_Types(T => u64, T_Array => u64_Array, T_Array_Access => u64_Array_Access);

--   use Crypto_Core_Types;
--   use Crypto_Types_u8;
--   use Crypto_Types_u16;
--   use Crypto_Types_u32;
--   use Crypto_Types_u64;


end Crypto_Types;
