--  Copyright (C) 2017  bg nerilex <bg@nerilex.org>
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

with Unicode.CES;    use Unicode.CES;
with Schema.Readers; use Schema.Readers;
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Strings;    use Ada.Strings;
with Ada.Characters.Handling; use  Ada.Characters.Handling;
with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;
with Ada.Containers.Ordered_Sets;
with Ada.Unchecked_Deallocation;

package body Xml_Kat_Reader is

   type String_Access is access String;

   procedure Free_String is new Ada.Unchecked_Deallocation(Object => String, Name => String_Access);

   function "<" (left, right : String_Access) return Boolean is
   begin
      return left.all < right.all;
   end;

   function "=" (left, right : String_Access) return Boolean is
   begin
      return left.all = right.all;
   end;


   package Strings_Set is new Ada.Containers.Ordered_Sets(Element_Type => String_Access);
   use Strings_Set;

   type Xml_Header_Element is
     (testFile,
      header,
      convertDate,
      originalFilename,
      originalSha256,
      originalSha512,
      comment,
      algorithm,
      kat_vector
     );

   Header_Elements : Strings_Set.Set;
   Kat_Elements : Strings_Set.Set;

   Procedure Print_Result(Result : in Boolean; Test_Index : Positive) is
      Width : constant := 50;
   begin
      if (Integer(Test_Index) - 1) mod Width = 0 then
         Put("[ ");
         Put(Integer(Test_Index - 1), 5);
         Put(" ] : ");
      end if;
      case Result is
--         when Unknown => Put("?");
         when True      => Put("*");
         when False    => Put("!");
      end case;
      if Test_Index mod Width = 0 then
         New_Line;
      end if;
   end;

   procedure Characters
     (Handler : in out Reader;
      Ch      : Unicode.CES.Byte_Sequence) is
   begin
      Handler.Tmp := To_Unbounded_String(Ch);
   end Characters;

   procedure Start_Document (Handler : in out Reader) is
      tmp : String_Access;
   begin
      Handler.Tmp := To_Unbounded_String("");
      Handler.Test_Index := 0;
      for a in Xml_Header_Element'Range loop
         tmp := new String'(To_Upper(Xml_Header_Element'Image(a)));
         Strings_Set.Insert(Container => Header_Elements,
                New_Item  => tmp);
--           Put_Line("Adding " & tmp.all & " to Header_Elements");
      end loop;
      for a in KAT_Entrys_T'Range loop
         tmp := new String'(To_Upper(KAT_Entrys_T'Image(a)));
         Strings_Set.Insert(Container => Kat_Elements,
                            New_Item  => tmp);
--           Put_Line("Adding " & tmp.all & " to Kat_Elements");
      end loop;
   end Start_Document;

   procedure End_Document (Handler : in out Reader) is
      Cur : Strings_Set.Cursor;
      Tmp : String_Access;
   begin
      Cur := Strings_Set.First(Header_Elements);
      while Cur /= Strings_Set.No_Element loop
         Tmp := Strings_Set.Element(Cur);
         Free_String(Tmp);
         Strings_Set.Delete(Header_Elements, Cur);
         Cur := Strings_Set.First(Header_Elements);
      end loop;

      Cur := Strings_Set.First(Kat_Elements);
      while Cur /= Strings_Set.No_Element loop
         Tmp := Strings_Set.Element(Cur);
         Free_String(Tmp);
         Strings_Set.Delete(Kat_Elements, Cur);
         Cur := Strings_Set.First(Kat_Elements);
      end loop;
   end End_Document;

   function Is_Header_Element(Local_Name : Sax.Symbols.Symbol) return Boolean is
      Tmp : String_Access := new String'(To_Upper(Sax.Symbols.Get(Local_Name).all));
      Ret : Boolean;
   begin
      Ret := Header_Elements.Contains(Tmp);
      Free_String(Tmp);
      return Ret;
   end Is_Header_Element;


   function Is_Kat_Element(Local_Name : Sax.Symbols.Symbol) return Boolean is
      Tmp : String_Access := new String'(To_Upper(Sax.Symbols.Get(Local_Name).all));
      Ret : Boolean;
   begin
      Ret := Kat_Elements.Contains(Tmp);
      Free_String(Tmp);
      return Ret;
   end Is_Kat_Element;


   procedure Start_Element
     (Handler    : in out Reader;
      NS         : Sax.Utils.XML_NS;
      Local_Name : Sax.Symbols.Symbol;
      Atts       : Sax_Attribute_List) is
      Element : Xml_Header_Element;
   begin
      if Is_Header_Element(Local_Name) then
         Element := Xml_Header_Element'Value(Sax.Symbols.Get(Local_Name).all);
         case Element is
         when kat_vector =>
            Handler.Kat := Create(Handler.Test_Index);
         when others =>
            null;
         end case;
      end if;
      Handler.Tmp := To_Unbounded_String("");
   end Start_Element;



   procedure End_Element
     (Handler    : in out Reader;
      NS         : Sax.Utils.XML_NS;
      Local_Name : Sax.Symbols.Symbol) is
      Ignore : Positive;
      Result : Boolean;
      Element : Xml_Header_Element;
      Kat_Entry : KAT_Entrys_T;
   begin
      if Is_Header_Element(Local_Name) then
         Element := (Xml_Header_Element'Value(Sax.Symbols.Get(Local_Name).all));
         case Element is
         when others =>
            null;
         end case;
      elsif Is_Kat_Element(Local_Name) then
            Kat_Entry := KAT_Entrys_T'Value(Sax.Symbols.Get(Local_Name).all);
            Set(Handler.Kat, Kat_Entry, To_String(Handler.Tmp));
      elsif KAT_Type_Name = Sax.Symbols.Get(Local_Name).all then
        Result := Test(Handler.Kat);
      if Result then
         Handler.Ok_Tests := Handler.Ok_Tests + 1;
      else
         Handler.Failed_Tests := Handler.Failed_Tests + 1;
      end if;
      Destroy(Handler.Kat);
      Handler.Test_Index := Handler.Test_Index + 1;
      Print_Result(Result, Handler.Test_Index);
   end if;
      Handler.Tmp := To_Unbounded_String("");
   end End_Element;

end Xml_Kat_Reader;
