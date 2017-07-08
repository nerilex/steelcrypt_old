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

with Crypto_Core_Types; use Crypto_Core_Types;
with Block_Cipher_Generic;
with Sax.Readers; use Sax.Readers;
with Sax.Utils;
with Sax.Symbols;
with Schema.Readers;
with Unicode.CES;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

generic
   type KAT_T is private;
   type KAT_Entrys_T is (<>);
   with procedure Set(KAT : in out KAT_T; KAT_Entry : KAT_Entrys_T; value : String);
   with function Test(KAT : KAT_T) return boolean;
   with function Create(index : Natural) return KAT_T;
   with procedure Destroy(KAT : in out KAT_T);

package Xml_Kat_Reader is

   type Reader is new Schema.Readers.Validating_Reader with record
      --     type Reader is new Sax.Readers.Reader with record
      Kat : KAT_T;
      Tmp : Unbounded_String;
      Test_Index : Natural := 0;
      Failed_Tests : Natural := 0;
      Ok_Tests : Natural := 0;
   end record;

--     procedure Start_Document(Handler : in out Reader);
--
--     procedure Start_Element
--       (Handler       : in out Reader;
--        Namespace_URI : Unicode.CES.Byte_Sequence := "";
--        Local_Name    : Unicode.CES.Byte_Sequence := "";
--        Qname         : Unicode.CES.Byte_Sequence := "";
--        Atts          : Sax.Attributes.Attributes'Class);
--
--     procedure End_Element
--       (Handler : in out Reader;
--        Namespace_URI : Unicode.CES.Byte_Sequence := "";
--        Local_Name    : Unicode.CES.Byte_Sequence := "";
--        Qname         : Unicode.CES.Byte_Sequence := "");

   procedure Characters
     (Handler : in out Reader;
      Ch      : Unicode.CES.Byte_Sequence);

   procedure Start_Document (Handler : in out Reader);
   --  Receive notification of the beginning of a document.
   --  This callback is called only once by the parser, before any other
   --  function in this interface except Set_Document_Locator.

   procedure Start_Element
     (Handler    : in out Reader;
      NS         : Sax.Utils.XML_NS;
      Local_Name : Sax.Symbols.Symbol;
      Atts       : Sax_Attribute_List);
   --  Receive notification of the beginning of an element.
   --  There will always be a matching call to End_Element, even for empty
   --  elements.
   --  Up to three name components can be given for each element, depending
   --  on the value of the XML_Reader features.
   --  - Namespace_URI and Local_Name are required when Namespace_Feature is
   --    True, but are optional if False. If one is specified, both must be.
   --  - Qname (qualified name) is required if Namespace_Prefixes_Feature is
   --    True, and optional if False. This is basically of the form "Ns:Name"
   --  The attribute list will only contain attributes with explicit values. It
   --  will contain attributes used for namespace declaration (xmlns*) only if
   --  Namespace_Prefixes_Feature is True.
   --
   --  For users of older versions of XML/Ada, the old profile of Start_Element
   --  is still available if you derive from the "Reader" type (below) instead
   --  of "Sax_Reader". We do encourage you to transition to the new profiles
   --  at your convenience, though, because they provide greater efficiency,
   --  mostly by limiting the number of string comparison and allocations.

   procedure End_Element
     (Handler    : in out Reader;
      NS         : Sax.Utils.XML_NS;
      Local_Name : Sax.Symbols.Symbol);
   --  Receive notification of the end of an element.

   procedure End_Document (Handler : in out Reader);

end Xml_Kat_Reader;
