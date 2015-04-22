package body AES is

   function gf256mul(a, b : u8) return u8 is
      r : u8 := 0;
      t1 : u8 := a;
      t2 : u8 := b;
   begin
      for i in 1 .. 8 loop
         if (t1 and 1) = 1 then
            r := r xor t2;
         end if;
         t1 := Shift_Right(t1, 1);
         if (t2 and 16#80#) = 16#80# then
            t2 := t2 xor polynom;
         end if;
         t2 := Shift_Left(t2, 1);
      end loop;
      return r;
   end gf256mul;

end AES;
