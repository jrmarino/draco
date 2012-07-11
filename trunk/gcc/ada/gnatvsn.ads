------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              G N A T V S N                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
-- Copyright (C) 2010 John Marino <draco@marino.st>                         --
------------------------------------------------------------------------------

--  This package spec holds version information for the GNAT tools.
--  It is updated whenever the release number is changed.

package Gnatvsn is

   Gnat_Static_Version_String : constant String := "GNAT AUX";
   --  Static string identifying this version, that can be used as an argument
   --  to e.g. pragma Ident.

   function Gnat_Version_String return String;
   --  Version output when GNAT (compiler), or its related tools, including
   --  GNATBIND, GNATCHOP, GNATFIND, GNATLINK, GNATMAKE, GNATXREF, are run
   --  (with appropriate verbose option switch set).

   type Gnat_Build_Type is (FSF, GPL);
   --  See Build_Type below for the meaning of these values.

   Build_Type : constant Gnat_Build_Type := FSF;
   --  Kind of GNAT build:
   --
   --    FSF
   --       GNAT FSF version. This version of GNAT is part of a Free Software
   --       Foundation release of the GNU Compiler Collection (GCC). The bug
   --       box generated by Comperr gives information on how to report bugs
   --       and list the "no warranty" information.
   --
   --    GPL
   --       GNAT GPL Edition. This is a special version of GNAT, released by
   --       Ada Core Technologies and intended for academic users, and free
   --       software developers. The bug box generated by the package Comperr
   --       gives appropriate bug submission instructions that do not reference
   --       customer number etc.

   function Gnat_Free_Software return String;
   --  Text to be displayed by the different GNAT tools when switch --version
   --  is used. This text depends on the GNAT build type.

   function Copyright_Holder return String;
   --  Return the name of the Copyright holder to be displayed by the different
   --  GNAT tools when switch --version is used.

   Ver_Len_Max : constant := 256;
   --  Longest possible length for Gnat_Version_String in this or any
   --  other version of GNAT. This is used by the binder to establish
   --  space to store any possible version string value for checks. This
   --  value should never be decreased in the future, but it would be
   --  OK to increase it if absolutely necessary. If it is increased,
   --  be sure to increase GNAT.Compiler.Version.Ver_Len_Max as well.

   Ver_Prefix : constant String := "GNAT Version: ";
   --  Prefix generated by binder. If it is changed, be sure to change
   --  GNAT.Compiler_Version.Ver_Prefix as well.

   Library_Version : constant String := "4.6";
   --  Library version. This value must be updated when the compiler
   --  version number Gnat_Static_Version_String is updated.
   --
   --  Note: Makefile.in uses the library version string to construct the
   --  soname value.

   Verbose_Library_Version : constant String := "GNAT Lib v" & Library_Version;
   --  Version string stored in e.g. ALI files

   Current_Year : constant String := "2011";
   --  Used in printing copyright messages

end Gnatvsn;
