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

-- ----------------------------------
-- - Generic Functions / Procedures -
-- ----------------------------------

-- --------------------------
-- - Functions / Procedures -
-- --------------------------
   package body Crypto_Generic_Block_Utils is

   -- compare two array with timing independent of content
--   function "="(Left, Right : T_Array ) return Boolean is
--      x : T := 0;
--   begin
--      if Left'Length /= Right'Length then
--        return false;
--      end if;
--      for i in Left'Range loop
--         x := x or (Left(i) xor Right(i));
--      end loop;
--      if x = 0 then
--         return true;
--      else
--         return false;
--      end if;
--   end "=";

   function "mod" (Left: T_Array_Index; Right : Natural) return T_Array_Index is
   begin
      return T_Array_Index(Natural(Left) mod Right);
   end "mod";

   function "mod" (Left: Natural; Right : T_Array_Index) return T_Array_Index is
   begin
      return T_Array_Index(Left mod Natural(Right));
   end "mod";

   -- xor each element on the left with the corresponding element on the right
   function "xor" (Left, Right : T_Array) return T_Array is
      r : T_Array;
   begin
      if Left'Length /= Right'Length then
         raise Constraint_Error;
      end if;
      for i in r'Range loop
         r (i) := Left (i) xor Right (Right'First - Left'First + i);
      end loop;
      return r;
   end "xor";

--     procedure eor (Left : in out T_Array; Right : in T_Array) is
--     begin
--        if Left'Length /= Right'Length then
--           raise Constraint_Error;
--        end if;
--        for i in Left'Range loop
--           Left(i) := Left(i) xor Right(i);
--        end loop;
--     end eor;

   -- xor the left element with each element on the right
   function "xor" (Left : T; Right : T_Array) return T is
      r : T := Left;
   begin
      for i in Right'Range loop
         r := r xor Right (i);
      end loop;
      return r;
   end "xor";

   -- xor each element on the left with the element on the right
   function "xor" (Left : T_Array; Right : T) return T_Array is
      r : T_Array;
   begin
      for i in r'Range loop
         r (i) := Left (i) xor Right;
      end loop;
      return r;
   end "xor";

   -- and each element on the left with the corresponding element on the right
   function "and" (Left, Right : T_Array) return T_Array is
      r : T_Array;
   begin
      if Left'Length /= Right'Length then
         raise Constraint_Error;
      end if;
      for i in r'Range loop
         r (i) := Left (i) and Right (Right'First - Left'First + i);
      end loop;
      return r;
   end "and";

   -- and the left element with each element on the right
   function "and" (Left : T; Right : T_Array) return T is
      r : T := Left;
   begin
      for i in Right'Range loop
         r := r and Right (i);
      end loop;
      return r;
   end "and";

   -- and each element on the left with the element on the right
   function "and" (Left : T_Array; Right : T) return T_Array is
      r : T_Array;
   begin
      for i in r'Range loop
         r (i) := Left (i) and Right;
      end loop;
      return r;
   end "and";

   -- or each element on the left with the corresponding element on the right
   function "or" (Left, Right : T_Array) return T_Array is
      r : T_Array;
   begin
      if Left'Length /= Right'Length then
         raise Constraint_Error;
      end if;
      for i in r'Range loop
         r (i) := Left (i) or Right (Right'First - Left'First + i);
      end loop;
      return r;
   end "or";

   -- or the left element with each element on the right
   function "or" (Left : T; Right : T_Array) return T is
      r : T := Left;
   begin
      for i in Right'Range loop
         r := r or Right (i);
      end loop;
      return r;
   end "or";

   -- or each element on the left with the element on the right
   function "or" (Left : T_Array; Right : T) return T_Array is
      r : T_Array;
   begin
      for i in r'Range loop
         r (i) := Left (i) or Right;
      end loop;
      return r;
   end "or";

   -- add each element on the left with the corresponding element on the right
   function "+" (Left, Right : T_Array) return T_Array is
      r : T_Array;
   begin
      if Left'Length /= Right'Length then
         raise Constraint_Error;
      end if;
      for i in r'Range loop
         r (i) := Left (i) + Right (Right'First - Left'First + i);
      end loop;
      return r;
   end "+";

   -- add the left element with each element on the right
   function "+" (Left : T; Right : T_Array) return T is
      r : T := Left;
   begin
      for i in Right'Range loop
         r := r + Right (i);
      end loop;
      return r;
   end "+";

   -- add each element on the left with the element on the right
   function "+" (Left : T_Array; Right : T) return T_Array is
      r : T_Array;
   begin
      for i in r'Range loop
         r (i) := Left (i) + Right;
      end loop;
      return r;
   end "+";

   -- subtract from each element on the left the corresponding element on the right
   function "-" (Left, Right : T_Array) return T_Array is
      r : T_Array;
   begin
      if Left'Length /= Right'Length then
         raise Constraint_Error;
      end if;
      for i in r'Range loop
         r (i) := Left (i) - Right (Right'First - Left'First + i);
      end loop;
      return r;
   end "-";

   -- subtract from the left element each element on the right
   function "-" (Left : T; Right : T_Array) return T is
      r : T := Left;
   begin
      for i in Right'Range loop
         r := r - Right (i);
      end loop;
      return r;
   end "-";

   -- subtract from each element on the left the element on the right
   function "-" (Left : T_Array; Right : T) return T_Array is
      r : T_Array;
   begin
      for i in r'Range loop
         r (i) := Left (i) - Right;
      end loop;
      return r;
   end "-";

--     procedure Rotate_Array_Left(A : T_Array_Access; Amount : Natural) is
--        b : T;
--     begin
--        for i in 1 .. Amount loop
--           b := A(T_Array_Index'First);
--           for j in T_Array_Index'First .. T_Array_Index'Last - 1 loop
--              A(j) := A(j + 1);
--           end loop;
--           A(T_Array_Index'Last) := b;
--        end loop;
--     end;

   function Rotate_Array_Left (A : T_Array; Amount : Natural) return T_Array is
      r : T_Array;
      x : T_Array_Index;
   begin
      x := Amount mod r'Length;
      if A'Length < 1 then
         raise Constraint_Error;
      end if;
      r (T_Array_Index'First .. T_Array_Index'Last - x)    := A (T_Array_Index'First + x .. T_Array_Index'Last);
      r (T_Array_Index'Last - x + 1 .. T_Array_Index'Last) := A (T_Array_Index'First .. T_Array_Index'First + x - 1);
      return r;
   end Rotate_Array_Left;

   function Rotate_Array_Right
     (A      : T_Array;
      Amount : Natural) return T_Array
   is
      r : T_Array;
      x : T_Array_Index;
   begin
      x := Amount mod r'Length;
      if A'Length < 1 then
         raise Constraint_Error;
      end if;
      r (T_Array_Index'First + x .. T_Array_Index'Last)      := A (T_Array_Index'First .. T_Array_Index'Last - x);
      r (T_Array_Index'First .. T_Array_Index'First + x - 1) := A (T_Array_Index'Last - x + 1 .. T_Array_Index'Last);
      return r;
   end Rotate_Array_Right;

   function Shift_Array_Left (A : T_Array; Amount : Natural) return T_Array is
      r : T_Array;
      x : T_Array_Index;
   begin
      x := Amount mod r'Length;
      if A'Length < 1 then
         raise Constraint_Error;
      end if;
      r (T_Array_Index'First .. T_Array_Index'Last - x) := A (T_Array_Index'First + x .. T_Array_Index'Last);
      for i in (T_Array_Index'Last - x + 1) .. T_Array_Index'Last loop
         r (i) := 0;
      end loop;
      return r;
   end Shift_Array_Left;

   function Shift_Array_Right (A : T_Array; Amount : Natural) return T_Array is
      r : T_Array;
      x : T_Array_Index;
   begin
      x := Amount mod r'Length;
      if A'Length < 1 then
         raise Constraint_Error;
      end if;
      r (T_Array_Index'First + x .. T_Array_Index'Last) := A (T_Array_Index'First .. T_Array_Index'Last - x);
      for i in T_Array_Index'First .. (T_Array_Index'First + x - 1) loop
         r (i) := 0;
      end loop;
      return r;
   end Shift_Array_Right;

   -- rotate the whole Array as continues big-endian integer; positive Amount rotates left (towards lower address)
   function Rotate_be (A : T_Array; Amount : Integer) return T_Array is
      r               : T_Array;
      c1, c2, tmp     : T;
      x               : Integer;
      word_rot        : Integer;
      bit_rot         : Integer;
      reverse_bit_rot : Integer;
   begin
      x        := Amount mod (A'Length * T'Size);
      word_rot := x / T'Size;
      bit_rot  := x mod T'Size;
      if word_rot > 0 then
         r := Rotate_Array_Left (A => A, Amount => Natural (abs word_rot));
      else
         r := Rotate_Array_Right (A => A, Amount => Natural (word_rot));
      end if;
      -- if bit rotation goes to the left
      if bit_rot > 0 then
         reverse_bit_rot := T'Size - bit_rot;
         c1              := Shift_Right (r (T_Array_Index'First), reverse_bit_rot);
         for i in reverse r'Range loop
            c2    := Shift_Right (r (i), reverse_bit_rot);
            tmp   := Shift_Left (r (i), bit_rot);
            r (i) := tmp or c1;
            c1    := c2;
         end loop;
      end if;
      -- if bit rotation goes to the right
      if bit_rot < 0 then
         bit_rot         := -bit_rot;
         reverse_bit_rot := T'Size - bit_rot;
         c1              := Shift_Left (r (T_Array_Index'Last), reverse_bit_rot);
         for i in r'Range loop
            c2    := Shift_Left (r (i), reverse_bit_rot);
            tmp   := Shift_Right (r (i), bit_rot);
            r (i) := tmp or c1;
            c1    := c2;
         end loop;
      end if;
      return r;
   end Rotate_be;

   -- rotate the whole Array as continues little-endian integer; positive Amount rotates left (towards higher address)
   function Rotate_le (A : T_Array; Amount : Integer) return T_Array is
      r               : T_Array;
      c1, c2, tmp     : T;
      x               : Integer;
      word_rot        : Integer;
      bit_rot         : Integer;
      reverse_bit_rot : Integer;
   begin
      x        := Amount mod (A'Length * T'Size);
      word_rot := x / T'Size;
      bit_rot  := x mod T'Size;
      if word_rot < 0 then
         r := Rotate_Array_Left (A => A, Amount => Natural (abs word_rot));
      else
         r := Rotate_Array_Right (A => A, Amount => Natural (word_rot));
      end if;
      -- if bit rotation goes to the left
      if bit_rot > 0 then
         reverse_bit_rot := T'Size - bit_rot;
         c1              := Shift_Right (r (T_Array_Index'Last), reverse_bit_rot);
         for i in r'Range loop
            c2    := Shift_Right (r (i), reverse_bit_rot);
            tmp   := Shift_Left (r (i), bit_rot);
            r (i) := tmp or c1;
            c1    := c2;
         end loop;
      end if;
      -- if bit rotation goes to the right
      if bit_rot < 0 then
         bit_rot         := -bit_rot;
         reverse_bit_rot := T'Size - bit_rot;
         c1              := Shift_Left (r (T_Array_Index'First), reverse_bit_rot);
         for i in reverse r'Range loop
            c2    := Shift_Left (r (i), reverse_bit_rot);
            tmp   := Shift_Right (r (i), bit_rot);
            r (i) := tmp or c1;
            c1    := c2;
         end loop;
      end if;
      return r;
   end Rotate_le;

   -- rotate each element by Amount to the left; negative values for Amount rotate to the right
   function Rotate_each (A : T_Array; Amount : Integer) return T_Array is
      r : T_Array;
   begin
      if Amount > 0 then
         for i in r'Range loop
            r (i) := Rotate_Left (A (i), Natural (Amount));
         end loop;
      end if;
      if Amount < 0 then
         for i in r'Range loop
            r (i) := Rotate_Right (A (i), Natural (-Amount));
         end loop;
      end if;
      if Amount = 0 then
         r := A;
      end if;
      return r;
   end Rotate_each;

   -- shift the whole Array as continues big-endian integer; positive Amount shifts left (towards lower address)
   function Shift_be (A : T_Array; Amount : Integer) return T_Array is
      r                 : T_Array;
      word_shift        : Integer;
      bit_shift         : Integer;
      reverse_bit_shift : Integer;
      c1, c2            : T := 0;
   begin
      -- left shift
      if Amount > 0 then
         word_shift        := Amount / T'Size;
         bit_shift         := Amount mod T'Size;
         reverse_bit_shift := T'Size - bit_shift;
         r                 := Shift_Array_Left (A => A, Amount => word_shift);
         for i in reverse r'Range loop
            c2    := Shift_Right (Value => r (i), Amount => reverse_bit_shift);
            r (i) := Shift_Left (Value => r (i), Amount => bit_shift) or c1;
            c1    := c2;
         end loop;
      end if;
      -- right shift
      if Amount < 0 then
         word_shift        := (-Amount) / T'Size;
         bit_shift         := (-Amount) mod T'Size;
         reverse_bit_shift := T'Size - bit_shift;
         r                 := Shift_Array_Right (A => A, Amount => word_shift);
         for i in r'Range loop
            c2    := Shift_Left (Value => r (i), Amount => reverse_bit_shift);
            r (i) := Shift_Right (Value => r (i), Amount => bit_shift) or c1;
            c1    := c2;
         end loop;
      end if;
      if Amount = 0 then
         r := A;
      end if;
      return r;
   end Shift_be;

   -- Shift the whole Array as continues little-endian integer; positive Amount shifts left (towards higher address)
   function Shift_le (A : T_Array; Amount : Integer) return T_Array is
      r                 : T_Array;
      word_shift        : Integer;
      bit_shift         : Integer;
      reverse_bit_shift : Integer;
      c1, c2            : T := 0;
   begin
      -- left shift
      if Amount > 0 then
         word_shift        := Amount / T'Size;
         bit_shift         := Amount mod T'Size;
         reverse_bit_shift := T'Size - bit_shift;
         r                 := Shift_Array_Right (A => A, Amount => word_shift);
         for i in r'Range loop
            c2    := Shift_Right (Value => r (i), Amount => reverse_bit_shift);
            r (i) := Shift_Left (Value => r (i), Amount => bit_shift) or c1;
            c1    := c2;
         end loop;
      end if;
      -- right shift
      if Amount < 0 then
         word_shift        := (-Amount) / T'Size;
         bit_shift         := (-Amount) mod T'Size;
         reverse_bit_shift := T'Size - bit_shift;
         r                 := Shift_Array_Left (A => A, Amount => word_shift);
         for i in reverse r'Range loop
            c2    := Shift_Left (Value => r (i), Amount => reverse_bit_shift);
            r (i) := Shift_Right (Value => r (i), Amount => bit_shift) or c1;
            c1    := c2;
         end loop;
      end if;
      if Amount = 0 then
         r := A;
      end if;
      return r;
   end Shift_le;

   -- shift each element by Amount to the left; negative values for Amount shift to the right
   function Shift_each (A : T_Array; Amount : Integer) return T_Array is
      r : T_Array;
   begin
      if Amount > 0 then
         for i in r'Range loop
            r (i) := Shift_Left (A (i), Natural (Amount));
         end loop;
      end if;
      if Amount < 0 then
         for i in r'Range loop
            r (i) := Shift_Right (A (i), Natural (-Amount));
         end loop;
      end if;
      if Amount = 0 then
         r := A;
      end if;
      return r;
   end Shift_each;


   --
   function Bit_Get
     (A           : in T;
      Bit_Address :    Bit_Address_T;
      Order       : in System.Bit_Order := System.Default_Bit_Order) return Bit
   is
      use System;
      Temp : T;
   begin
      if Order = High_Order_First then
         Temp := Shift_Right (A, T'Size - Bit_Address - 1);
      else
         Temp := Shift_Right (A, Bit_Address);
      end if;
      return Bit (Temp and 1);
   end Bit_Get;

   --
   procedure Bit_Clear
     (A           : in out T;
      Bit_Address :        Bit_Address_T;
      Order       : in     System.Bit_Order := System.Default_Bit_Order)
   is
      use System;
   begin
      if Order = High_Order_First then
         A := A and not Shift_Left (1, T'Size - Bit_Address - 1);
      else
         A := A and not Shift_Left (1, Bit_Address);
      end if;
   end Bit_Clear;

   --
   procedure Bit_Set
     (A           : in out T;
      Bit_Address :        Bit_Address_T;
      Value       :        Bit              := 1;
      Order       : in     System.Bit_Order := System.Default_Bit_Order)
   is
      use System;
   begin
      if Value = 0 then
         Bit_Clear (A => A, Bit_Address => Bit_Address, Order => Order);
      else
         if Order = High_Order_First then
            A := A or Shift_Left (1, T'Size - Bit_Address - 1);
         else
            A := A or Shift_Left (1, Bit_Address);
         end if;
      end if;
   end Bit_Set;

   --
   procedure Bit_Toggle
     (A           : in out T;
      Bit_Address :        Bit_Address_T;
      Order       : in     System.Bit_Order := System.Default_Bit_Order)
   is
      use System;
   begin
      if Order = High_Order_First then
         A := A xor Shift_Left (1, T'Size - Bit_Address - 1);
      else
         A := A xor Shift_Left (1, Bit_Address);
      end if;
   end Bit_Toggle;

   --
   function Bit_Get
     (A           : in T_Array;
      Bit_Address : in Bit_Address_Array_T;
      Order       : in System.Bit_Order := System.Default_Bit_Order) return Bit
   is
   begin
      return Bit_Get
          (A           => A (T_Array_Index'First + T_Array_Index(Bit_Address / T'Size)),
           Bit_Address => Bit_Address mod T'Size,
           Order       => Order);
   end Bit_Get;

   --
   procedure Bit_Clear
     (A           : in out T_Array;
      Bit_Address : in     Bit_Address_Array_T;
      Order       : in     System.Bit_Order := System.Default_Bit_Order)
   is
   begin
      Bit_Clear
        (A           => A (T_Array_Index'First + T_Array_Index(Bit_Address / T'Size)),
         Bit_Address => Bit_Address mod T'Size,
         Order       => Order);
   end Bit_Clear;

   --
   procedure Bit_Set
     (A           : in out T_Array;
      Bit_Address : in     Bit_Address_Array_T;
      Value       : in     Bit              := 1;
      Order       : in     System.Bit_Order := System.Default_Bit_Order)
   is
   begin
      Bit_Set
        (A           => A (T_Array_Index'First + T_Array_Index(Bit_Address / T'Size)),
         Bit_Address => Bit_Address mod T'Size,
         Value       => Value,
         Order       => Order);
   end Bit_Set;

   --
   procedure Bit_Toggle
     (A           : in out T_Array;
      Bit_Address : in     Bit_Address_Array_T;
      Order       : in     System.Bit_Order := System.Default_Bit_Order)
   is
   begin
      Bit_Set
        (A           => A (T_Array_Index'First + T_Array_Index(Bit_Address / T'Size)),
         Bit_Address => Bit_Address mod T'Size,
         Order       => Order);
   end Bit_Toggle;

   -- swap two elements
   procedure Swap (A, B : in out T) is
      temp : T;
   begin
      temp := A;
      A    := B;
      B    := temp;
   end Swap;

   -- swap two Arrays
   procedure Swap (A, B : in out T_Array) is
   begin
      if A'Length /= B'Length then
         raise Constraint_Error;
      end if;
      A := A xor B;
      B := B xor A;
      A := A xor B;
   end Swap;

end Crypto_Generic_Block_Utils;
