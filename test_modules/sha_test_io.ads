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

with Ada.Text_IO;       use Ada.Text_IO;
with Crypto_Core_Types; use Crypto_Core_Types;

--
--  #  CAVS 11.0
--  #  "SHA-256 ShortMsg" information
--  #  SHA-256 tests are configured for BIT oriented implementations
--  #  Generated on Tue Mar 15 08:29:11 2011
--
--  [L = 32]
--
--  Len = 0
--  Msg = 00
--  MD = e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
--
--  Len = 1
--  Msg = 00
--  MD = bd4f9e98beb68c6ead3243b1b4c7fed75fa4feaab1f84795cbd8a98676a2a375
--
--  Len = 2
--  Msg = 80
--  MD = 18f331f626210ff9bad6995d8cff6e891adba50eb2fdbddcaa921221cdc333ae
--
--  Len = 3
--  Msg = 60
--  MD = 1f7794d4b0b67d3a6edcd17aba2144a95828032f7943ed26bf0c7c7628945f48
--

package Sha_Test_IO is
   type Next_Type is
     (Digest_Length,
      Message_Length,
      Message_Block,
      Message_Digest,
      Seed,
      Count,
      Finish);

   type Context_T is limited private;

   procedure Open (Context : out Context_T; FileName : in String);
   procedure Get_Next_Type (Context : in out Context_T; next : out Next_Type);
   procedure Get_Integer (Context : in out Context_T; Value : out Integer);
   procedure Get_Data (Context : in out Context_T; Block : out u8_Array);

   generic
      Digest_Size_Bits : Natural;
      with procedure Hash
        (Data   : in     u8_Array;
         Digest :    out u8_Array;
         Bits   : in     Integer := -1);
   procedure Test_With_File (FileName : in String);

private

   type Context_T is record
      File : File_Type;
   end record;

end Sha_Test_IO;
