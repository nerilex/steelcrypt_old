with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Crypto_Types; use Crypto_Types;
with Crypto_Core_Types; use Crypto_Core_Types;

use Crypto_Types.Crypto_Types_u8;


procedure main is

   procedure print_hex(value : in u8) is
      hex_table : constant array (0 .. 15) of Character :=
        ( '0', '1', '2', '3',
       '4', '5', '6', '7',
       '8', '9', 'A', 'B',
       'C', 'D', 'E', 'F');
   begin
      Put(hex_table(Integer(Shift_Right(value, 4))));
      Put(hex_table(Integer(value and 16#F#)));
   end;

   procedure print_array_hex(A : in u8_Array) is
   begin
      for i in A'Range loop
         print_hex(A(i));
         Put(' ');
      end loop;
   end;

   a, b : u8_Array(0 .. 255);
begin
   for i in a'Range loop
      a(i) := u8(i);
      b(i) := u8(i);
   end loop;

   print_array_hex(a);
   New_Line;
   print_array_hex(b);
   New_Line;
   New_Line;

   for i in 1 .. a'Length * u8'Size loop
      a := Rotate_be(A => a, Amount => 1);
      Put("a:  ");
      print_array_hex(a);
      New_Line;
      b := Rotate_be(A => b, Amount => i);
      Put("b:  ");
      print_array_hex(b);
      New_Line;
      if (a /= b) then
         Put("Error @ i=");
         Put(i);
         New_Line;
      end if;
      b := Rotate_be(A => b, Amount => -i);
      Put("b': ");
      print_array_hex(b);
      New_Line;
   end loop;

   New_Line;
end main;
