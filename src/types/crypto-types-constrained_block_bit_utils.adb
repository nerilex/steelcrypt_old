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

package body Crypto.Types.Constrained_Block_Bit_Utils is

   function Element_Address(Bit_Address : Bit_Block_Address_T) return T_Array_Index is
   begin
      return T_Array_Index(Integer(T_Array'First) + Bit_Address / T'Size);
   end;

   --
   function Bit_Get
     (A           : in T_Array;
      Bit_Address :    Bit_Block_Address_T;
      Order : in System.Bit_Order := System.Default_Bit_Order) return Bit is
   begin
      return Bit_Get(A => A(Element_Address(Bit_Address)),
                     Bit_Address => Bit_Address_T(Bit_Address mod T'Size),
                     Order => Order);
   end Bit_Get;

   --
   procedure Bit_Clear
     (A           : in out T_Array;
      Bit_Address :        Bit_Block_Address_T;
      Order       : in     System.Bit_Order := System.Default_Bit_Order) is
   begin
      Bit_Clear(A           => A(Element_Address(Bit_Address)),
                Bit_Address => Bit_Address_T(Bit_Address mod T'Size),
                Order       => Order);
   end Bit_Clear;

   --
   procedure Bit_Set
     (A           : in out T_Array;
      Bit_Address :        Bit_Block_Address_T;
      Value       :        Bit              := 1;
      Order       : in     System.Bit_Order := System.Default_Bit_Order) is
   begin
      Bit_Set(A           => A(Element_Address(Bit_Address)),
              Bit_Address => Bit_Address_T(Bit_Address mod T'Size),
              Value => Value,
              Order       => Order);
   end Bit_Set;

   --
   procedure Bit_Toggle
     (A           : in out T_Array;
      Bit_Address :        Bit_Block_Address_T;
      Order       : in     System.Bit_Order := System.Default_Bit_Order) is
   begin
      Bit_Toggle(A           => A(Element_Address(Bit_Address)),
                 Bit_Address => Bit_Address_T(Bit_Address mod T'Size),
                 Order       => Order);

   end Bit_Toggle;

end Crypto.Types.Constrained_Block_Bit_Utils;
