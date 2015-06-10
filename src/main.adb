with Ada.Text_IO; use Ada.Text_IO;
with Crypto_Core_Types; use Crypto_Core_Types;
with Crypto_Types; use Crypto_Types;
with Spritz;
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

   procedure test_spritz(s : in String) is
      ctx : Spritz.Context;
      z : u8;
   begin
      Spritz.InitializeContext(ctx);
      Spritz.Absorb(ctx, s);
      Put(s);
      for i in 0 .. 6 - s'Length loop
         Put(" ");
      end loop;
      Put(": ");
      for j in 0 .. 7 loop
         Spritz.Drip(ctx, z);
         print_hex(z);
         Put(" ");
      end loop;
      New_Line;
   end test_spritz;

   procedure test_spritz_hash(s : in String) is
      ctx : Spritz.Context;
      z : u8;
   begin
      Spritz.InitializeContext(ctx);
      Spritz.Absorb(ctx, s);
      Spritz.AbsorbStop(ctx);
      Spritz.Absorb(ctx, u8(32));
      Put(s);
      for i in 0 .. 6 - s'Length loop
         Put(" ");
      end loop;
      Put(": ");
      for j in 0 .. 7 loop
         Spritz.Drip(ctx, z);
         print_hex(z);
         Put(" ");
      end loop;
      New_Line;
   end test_spritz_hash;

begin
   test_spritz("ABC");
   test_spritz("spam");
   test_spritz("arcfour");
   New_Line;

   test_spritz_hash("ABC");
   test_spritz_hash("spam");
   test_spritz_hash("arcfour");
   New_Line;
end main;
