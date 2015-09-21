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

with Crypto_Types; use Crypto_Types;
use Crypto_Types.Crypto_Utils_u16;
use Crypto_Types.Crypto_Utils_u64;

with Ada.Text_IO; use Ada.Text_IO;

package body Pi16Cipher_Spec is

   subtype Rotation_Ammount_T is Integer range 0 .. Word_T'Size - 1;
   subtype Chunk_Index_T is Integer range Chunk_T'Range;
   subtype Rate_Block is u8_Array(1 .. Rate_Bytes);

   type Phi_Constants is array (1 .. 4) of Word_T;
   type Phi_Rotation_Constants is array (1 .. 4) of Rotation_Ammount_T;
   type Phi_V_Constants is array (1 .. 4) of Chunk_Index_T;

   Mu_Constants : constant Phi_Constants := ( 16#F0E8#, 16#E4E2#, 16#E1D8#, 16#D4D2# );
   Mu_Rotation_Constants : constant Phi_Rotation_Constants := ( 1, 4, 9, 11 );
   Nu_Constants : constant Phi_Constants := ( 16#D1CC#, 16#CAC9#, 16#C6C5#, 16#C3B8# );
   Nu_Rotation_Constants : constant Phi_Rotation_Constants := ( 2, 5, 7, 13 );

   Mu_V_Constants : constant Phi_V_Constants := ( 4, 3, 2, 1 );
   Nu_V_Constants : constant Phi_V_Constants := ( 2, 1, 4, 3 );

   Pi_Constants : constant array (1 .. 2 * R) of Chunk_T :=
     (
        ( 16#B4B2#, 16#B1AC#, 16#AAA9#, 16#A6A5# ),
        ( 16#A39C#, 16#9A99#, 16#9695#, 16#938E# ),
        ( 16#8D8B#, 16#8778#, 16#7472#, 16#716C# ),
        ( 16#6A69#, 16#6665#, 16#635C#, 16#5A59# ),
        ( 16#5655#, 16#534E#, 16#4D4B#, 16#473C# ),
        ( 16#3A39#, 16#3635#, 16#332E#, 16#2D2B# )
     );

   generic
      Constants : Phi_Constants;
      Rotation_Constants : Phi_Rotation_Constants;
      V_Constants : Phi_V_Constants;
   function Phi(Chunk : in Chunk_T) return Chunk_T;

   function Phi(Chunk : in Chunk_T) return Chunk_T is
      Ret : Chunk_T;
      Sum : Word_T;
   begin
      Sum := 0 + Chunk;
      for i in Chunk'Range loop
         Ret(i) := Rotate_Left(Constants(i) + Sum - Chunk(V_Constants(i)), Rotation_Constants(i));
      end loop;
      Sum := 0 xor Ret;
      Ret := Ret xor Sum;
      return Ret;
   end Phi;

   function Nu is new Phi (Constants => Nu_Constants, Rotation_Constants => Nu_Rotation_Constants, V_Constants => Nu_V_Constants);

   function Mu(Chunk : Chunk_T) return Chunk_T is
      function foo is new Phi (Constants => Mu_Constants, Rotation_Constants => Mu_Rotation_Constants, V_Constants => Mu_V_Constants);
      Ret : Chunk_T;
   begin
      Ret := foo(Chunk);
      Ret := Rotate_Array_Left(Ret, 2);
      return Ret;
   end Mu;

   function "*" (X : Chunk_T; Y : Chunk_T) return Chunk_T is
      Ret : Chunk_T;
   begin
      Ret := Mu(X) + Nu(Y);
      Ret := Rotate_Array_Left(Ret, 1);
      return Ret;
   end "*";

   function E1 (C : Chunk_T; I : State_T) return State_T is
      J : State_T;
   begin
      J(1) := C * I(1);
        for index in 2 .. N loop
           J(index) := J(index - 1) * I(index);
        end loop;
      return J;
   end E1;

   function E2 (C : Chunk_T; I : State_T) return State_T is
      J : State_T;
   begin
      J(N) := I(N) * C;
      for index in reverse 1 .. N - 1 loop
         J(index) := I(index) * J(index + 1);
      end loop;
      return J;
   end E2;

   function Pi (State : State_T) return State_T is
      Ret : State_T := State;
   begin
      for round in 0 .. R - 1 loop
         Ret := E1(Pi_Constants(2 * round + 1), Ret);
         Ret := E2(Pi_Constants(2 * round + 2), Ret);
      end loop;
      return Ret;
   end Pi;

   procedure Dump(State : in State_T) is
   begin
      Put_Line("State:");
      for i in State'Range loop
         Put("    [ ");
         for j in State(i)'Range loop
            Put(To_Hex(State(i)(j)));
            Put(' ');
         end loop;
         Put_Line("]");
      end loop;
   end;

   procedure Initialize(Context : out Context_T; Key : in u8_Array; Public_Nonce : in u8_Array) is
      Scratch : u8_Array(1 .. IS_Bytes) := (others => 0);
      Index : Integer := Scratch'First;
   begin
      if Key'Length + Public_Nonce'Length >= Scratch'Length then
         raise Invalid_Key_Size;
      end if;
      Context.Tag := (others => 0);
      Scratch(1 .. Key'Length) := Key;
      Scratch(Key'Length + 1 .. Key'Length + Public_Nonce'Length) := Public_Nonce;
      Scratch(Key'Length + Public_Nonce'Length + 1) := 1;
      for i in Context.State'Range loop
         Context.State(i) := Load_LE(Scratch(Index .. Index + Chunk_T'Length * Word_T'Size / 8 - 1));
         Index := Index + Chunk_T'Length * Word_T'Size / 8;
      end loop;
      Context.State := pi(Context.State);
      Context.Counter := 0;
      for i in 1 .. 64 / Word_T'Size loop
         Context.Counter := Context.Counter or Shift_Left(u64(Context.State(2)(i)), (i - 1) * Word_T'Size);
      end loop;
   end Initialize;

   function "+" (Context : Context_T; Block_Number : Block_Number_T) return State_T is
      Counter : u64 := Context.Counter + u64(Block_Number);
      Ret : State_T := Context.State;
   begin
      for i in 1 .. 64 / Word_T'Size loop
         Ret(1)(i) := Ret(1)(i) xor Word_T( Counter and (Shift_Left(u64'(1), Word_T'Size) - 1));
         Counter := Shift_Right(Counter, Word_T'Size);
      end loop;
      return Ret;
   end "+";

   function Extract(State : State_T) return Rate_Block is
      Data : Rate_Block;
      Index : Positive := Data'First;
   begin
      for i in 0 .. N / 2 - 1 loop
         Store_LE(Data(Index .. Index + 4 * Word_T'Size / 8 - 1), State(2 * i + 1));
         Index := Index + 4 * Word_T'Size / 8;
      end loop;
      return Data;
   end Extract;

   function Extract(State : State_T) return Tag_Int_T is
      Tag : Tag_Int_T;
   begin
      for i in 0 .. N / 2 - 1 loop
         Tag(Tag'First + i * 4 .. Tag'First + i * 4 + 3) := State(2 * i + 1);
      end loop;
      return Tag;
   end Extract;

   function "xor" (State : State_T; Data : Rate_Block) return State_T is
      Ret : State_T := State;
      Index : Positive := Data'First;
   begin
      for i in 0 .. N / 2 - 1 loop
         Ret(2 * i + 1) := Ret(2 * i + 1) xor Chunk_T'(Load_LE(Data(Index .. Index + 4 * Word_T'Size / 8 - 1)));
         Index := Index + 4 * Word_T'Size / 8;
      end loop;
      return Ret;
   end "xor";

   function "xor" (State : State_T; Tag : Tag_Int_T) return State_T is
      Ret : State_T := State;
      Index : Positive := Tag'First;
   begin
      for i in 0 .. N / 2 - 1 loop
         Ret(2 * i + 1) := Ret(2 * i + 1) xor Tag(Index .. Index + 3);
         Index := Index + 4;
      end loop;
      return Ret;
   end "xor";

   function set (State : State_T; Data : Rate_Block) return State_T is
      Ret : State_T := State;
      Index : Positive := Data'First;
   begin
      for i in 0 .. N / 2 - 1 loop
         Ret(2 * i + 1) := Chunk_T'(Load_LE(Data(Index .. Index + 4 * Word_T'Size / 8 - 1)));
         Index := Index + 4 * Word_T'Size / 8;
      end loop;
      return Ret;
   end set;

   function "+" (Tag : Tag_Int_T) return Tag_T is
      Ret : Tag_T;
   begin
      Store_LE(Ret, Tag);
      return Ret;
   end "+";

   function Pad (Data : u8_Array) return Block_T is
      Ret : Block_T := (others => 0);
   begin
      if Data'Length >= Block_T'Length then
         raise Constraint_Error;
      end if;
      Ret(Ret'First .. Ret'First + Data'Length -1 ) := Data;
      Ret(Ret'First + Data'Length) := 1;
      return Ret;
   end Pad;

   procedure Process_Header_Block (Context : in out Context_T; Block : Block_T; Block_Number : Block_Number_T) is
   begin
      Context.Tag := Context.Tag + Extract(Pi(Pi(Context + Block_Number) xor Block));
   end Process_Header_Block;

   procedure Process_Header_Last_Block (Context : in out Context_T; Block : u8_Array; Block_Number : Block_Number_T) is
      Num : Block_Number_T := Block_Number;
      Index : Integer := Block'First;
   begin
      for i in 1 .. Block'Length / Block_Bytes loop
         Process_Header_Block(Context, Block(Index .. Index + Block_Bytes - 1), Num);
         Num := Num + 1;
         Index := Index + Block_Bytes;
      end loop;
      Process_Header_Block(Context, Pad(Block(Index .. Block'Last)), Num);
      Context.State := Pi(Context.State xor Context.Tag);
      Context.Counter := Context.Counter + u64(Num);
   end Process_Header_Last_Block;

   procedure Encrypt_Secret_Message_Number(Context : in out Context_T; Block : in out Block_T) is
      State : constant State_T := Pi(Context + Block_Number_T'(1)) xor Block;
   begin
      Block := Extract(State);
      Context.State := Pi(State);
      Context.Tag := Context.Tag + Extract(Context.State);
      Context.Counter := Context.Counter + 1;
   end Encrypt_Secret_Message_Number;

   procedure Decrypt_Secret_Message_Number(Context : in out Context_T; Block : in out Block_T) is
      State : constant State_T := Pi(Context + Block_Number_T'(1)) xor Block;
      Block_In : constant Block_T := Block;
   begin
      Block := Extract(State);
      Context.State := Pi(set(State, Block_In));
      Context.Tag := Context.Tag + Extract(Context.State);
      Context.Counter := Context.Counter + 1;
   end Decrypt_Secret_Message_Number;

   procedure Encrypt_Block(Context : in out Context_T; Block : in out Block_T; Block_Number : Block_Number_T) is
      State : State_T := Pi(Context + Block_Number) xor Block;
   begin
      Block := Extract(State);
      State := Pi(State);
      Context.Tag := Context.Tag + Extract(State);
   end Encrypt_Block;

   procedure Decrypt_Block(Context : in out Context_T; Block : in out Block_T; Block_Number : Block_Number_T) is
      State : State_T := Pi(Context + Block_Number) xor Block;
      In_Block : constant Block_T := Block;
   begin
      Block := Extract(State);
      State := Pi(set(State, In_Block));
      Context.Tag := Context.Tag + Extract(State);
   end Decrypt_Block;

   procedure Encrypt_Last_Block(Context : in out Context_T; Block : in out u8_Array; Block_Number : Block_Number_T) is
      State : State_T;
      Index : Integer := Block'First;
      Num : Block_Number_T := Block_Number;
      Temp_Block : Block_T;
   begin
      for i in 1 .. Block'Length / Block_Bytes loop
         Encrypt_Block(Context, Block(Index .. Index + Block_Bytes - 1), Num);
         Index := Index + Block_Bytes;
         Num := Num + 1;
      end loop;
      Temp_Block := Pad(Block(Index .. Block'Last));
      State := Pi(Context + Num) xor Temp_Block;
      Temp_Block := Extract(State);
      Block(Index .. Block'Last) := Temp_Block(Temp_Block'First .. Temp_Block'First + Block'Last - Index);
      State := Pi(State);
      Context.Tag := Context.Tag + Extract(State);
   end Encrypt_Last_Block;

   procedure Decrypt_Last_Block(Context : in out Context_T; Block : in out u8_Array; Block_Number : Block_Number_T) is
      State : State_T;
      Index : Integer := Block'First;
      Num : Block_Number_T := Block_Number;
      Temp_Block : Block_T;
   begin
      for i in 1 .. Block'Length / Block_Bytes loop
         Decrypt_Block(Context, Block(Index .. Index + Block_Bytes - 1), Num);
         Index := Index + Block_Bytes;
         Num := Num + 1;
      end loop;
      Temp_Block := Pad(Block(Index .. Block'Last));
      State := Pi(Context + Num) xor Temp_Block;
      Block(Index .. Block'Last) := Extract(State)(Temp_Block'First .. Temp_Block'First + Block'Last - Index);
      Temp_Block(Temp_Block'First + Block'Last - Index + 1 .. Temp_Block'Last) := Extract(State)(Temp_Block'First + Block'Last - Index + 1 .. Temp_Block'Last);
      State := Pi(set(State, Temp_Block));
      Context.Tag := Context.Tag + Extract(State);
   end Decrypt_Last_Block;

   function Get_Tag(Context : Context_T) return Tag_T is
   begin
      return +Context.Tag;
   end Get_Tag;

   function Is_Valid(Is_Tag : in Tag_T; Should_Tag : in Tag_T) return Boolean is
   begin
      return Is_Tag = Should_Tag;
   end Is_Valid;

   function Is_Valid(Context : in Context_T; Should_Tag : in Tag_T) return Boolean is
   begin
      return Get_Tag(Context) = Should_Tag;
   end Is_Valid;

   function Encrypt(Msg : u8_Array; AD : u8_Array; Public_Nonce : u8_Array; Secret_Nonce : Block_T; Key : u8_Array) return u8_Array is
      Crypt : u8_Array(1 .. Secret_Nonce'Length + Msg'Length + Tag_Bytes);
      Ctx : Context_T;
   begin
      Initialize(Context => Ctx, Key => Key, Public_Nonce => Public_Nonce);
      Process_Header_Last_Block(Context => Ctx, Block => AD, Block_Number => 1);
      Crypt(Crypt'First .. Crypt'First + Secret_Message_Number_Bytes - 1) := Secret_Nonce;
      Crypt(Crypt'First + Secret_Message_Number_Bytes .. Crypt'Last - Tag_Bytes) := Msg;
      Encrypt_Secret_Message_Number(Context => Ctx, Block => Crypt(Crypt'First .. Crypt'First + Secret_Message_Number_Bytes - 1));
      Encrypt_Last_Block(Context => Ctx, Block => Crypt(Crypt'First + Secret_Message_Number_Bytes .. Crypt'Last - Tag_Bytes), Block_Number => 1);
      Crypt(Crypt'Last - Tag_Bytes + 1 .. Crypt'Last) := Get_Tag(Ctx);
      return Crypt;
   end Encrypt;

   procedure Decrypt(Is_Valid : out Boolean; Msg : out u8_Array; Secret_Nonce : out Block_T; Cipher : in u8_Array; AD : in u8_Array; Public_Nonce : in u8_Array; Key : in u8_Array) is
      Ctx : Context_T;
   begin
      Initialize(Context => Ctx, Key => Key, Public_Nonce => Public_Nonce);
      Process_Header_Last_Block(Context => Ctx, Block => AD, Block_Number => 1);
      Secret_Nonce := Cipher(Cipher'First .. Cipher'First + Secret_Message_Number_Bytes - 1);
      Msg := Cipher(Cipher'First + Secret_Message_Number_Bytes .. Cipher'Last - Tag_Bytes);
      Decrypt_Secret_Message_Number(Context => Ctx, Block => Secret_Nonce);
      Decrypt_Last_Block(Context => Ctx, Block => Msg, Block_Number => 1);
      Is_Valid := Pi16Cipher_Spec.Is_Valid(Ctx, Cipher(Cipher'Last - Tag_Bytes + 1 .. Cipher'Last));
   end Decrypt;

end Pi16Cipher_Spec;

