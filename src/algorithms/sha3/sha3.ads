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

with SHA3_Generic;
with Hash_Generic;

package SHA3 is

   package SHA3_224_Spec is new SHA3_Generic(Capacity_Bits => 448);
   package SHA3_256_Spec is new SHA3_Generic(Capacity_Bits => 512);
   package SHA3_384_Spec is new SHA3_Generic(Capacity_Bits => 768);
   package SHA3_512_Spec is new SHA3_Generic(Capacity_Bits => 1024);

   package SHA3_224 is new Hash_Generic( Name_Intern             => "SHA3-224",
                                         Context_T_Intern        => SHA3_224_Spec.Context_T,
                                         Digest_Size_Bits_Intern => SHA3_224_Spec.Digest_Size_Bits,
                                         Block_Size_Bits_Intern  => SHA3_224_Spec.Block_Size_Bits,
                                         Initialize_Intern       => SHA3_224_Spec.Initialize,
                                         Next_Block_Intern       => SHA3_224_Spec.Next_Block,
                                         Last_Block_Intern       => SHA3_224_Spec.Last_Block,
                                         Get_Digest_Intern       => SHA3_224_Spec.Get_Digest );

   package SHA3_256 is new Hash_Generic( Name_Intern             => "SHA3-256",
                                         Context_T_Intern        => SHA3_256_Spec.Context_T,
                                         Digest_Size_Bits_Intern => SHA3_256_Spec.Digest_Size_Bits,
                                         Block_Size_Bits_Intern  => SHA3_256_Spec.Block_Size_Bits,
                                         Initialize_Intern       => SHA3_256_Spec.Initialize,
                                         Next_Block_Intern       => SHA3_256_Spec.Next_Block,
                                         Last_Block_Intern       => SHA3_256_Spec.Last_Block,
                                         Get_Digest_Intern       => SHA3_256_Spec.Get_Digest );

   package SHA3_384 is new Hash_Generic( Name_Intern             => "SHA3-384",
                                         Context_T_Intern        => SHA3_384_Spec.Context_T,
                                         Digest_Size_Bits_Intern => SHA3_384_Spec.Digest_Size_Bits,
                                         Block_Size_Bits_Intern  => SHA3_384_Spec.Block_Size_Bits,
                                         Initialize_Intern       => SHA3_384_Spec.Initialize,
                                         Next_Block_Intern       => SHA3_384_Spec.Next_Block,
                                         Last_Block_Intern       => SHA3_384_Spec.Last_Block,
                                         Get_Digest_Intern       => SHA3_384_Spec.Get_Digest );

   package SHA3_512 is new Hash_Generic( Name_Intern             => "SHA3-512",
                                         Context_T_Intern        => SHA3_512_Spec.Context_T,
                                         Digest_Size_Bits_Intern => SHA3_512_Spec.Digest_Size_Bits,
                                         Block_Size_Bits_Intern  => SHA3_512_Spec.Block_Size_Bits,
                                         Initialize_Intern       => SHA3_512_Spec.Initialize,
                                         Next_Block_Intern       => SHA3_512_Spec.Next_Block,
                                         Last_Block_Intern       => SHA3_512_Spec.Last_Block,
                                         Get_Digest_Intern       => SHA3_512_Spec.Get_Digest );

end SHA3;
