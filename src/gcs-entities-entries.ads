------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--                 G C S . E N T I T I E S . E N T R I E S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1.1.1 $                              --
--                                                                          --
--                    Copyright (c) 2000 Fraser Wilson                      --
--                                                                          --
-- GCS is free software; you can redistribute it  and/or  modify  it  under --
-- terms  of  the  GNU  General  Public  License  as  published by the Free --
-- Software Foundation; either version 2, or (at  your  option)  any  later --
-- version.  GCS  is  distributed  in  the hope that it will be useful, but --
-- WITHOUTANY WARRANTY; without even the implied warranty of  MERCHANTABIL- --
-- ITY  or  FITNESS  FOR  A  PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details. You should have received a  copy  of  the  GNU --
-- General  Public  License distributed with GCS; see file COPYING. If not, --
-- write to the Free Software Foundation, 59  Temple  Place  -  Suite  330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------

with GCS.Constraints;         use GCS.Constraints;

package GCS.Entities.Entries is

   --  Non-primitive subprograms for manipulating table entries

   --  Base class initialisation
   procedure Initialise
     (T     : in out Root_Table_Entry'Class;
      Name  : in     String;
      Decl  : in     Tree);

   procedure Initialise
     (T     : in out Root_Table_Entry'Class;
      Name  : in     String);

   --  Access functions common to all entries.
   function File_Name
     (T : access Root_Table_Entry'Class)
      return String;

   function File
     (T : access Root_Table_Entry'Class)
      return Source_File_Type;

   function Line
     (T : access Root_Table_Entry'Class)
      return Line_Number;

   function Column
     (T : access Root_Table_Entry'Class)
      return Column_Number;

   function Name
     (T : access Root_Table_Entry'Class)
      return String;

   function Declaration
     (T : access Root_Table_Entry'Class)
      return Tree;

   --  Versions of access functions for things that aren't allocated
   function File_Name
     (T : in Root_Table_Entry'Class)
      return String;

   function File
     (T : in Root_Table_Entry'Class)
      return Source_File_Type;

   function Line
     (T : in Root_Table_Entry'Class)
      return Line_Number;

   function Column
     (T : in Root_Table_Entry'Class)
      return Column_Number;

   function Name
     (T : in Root_Table_Entry'Class)
      return String;

   function Declaration
     (T : in Root_Table_Entry'Class)
      return Tree;

   function  Get_Table (E : access Root_Table_Entry'Class) return Table;
   procedure Set_Table (E        : access Root_Table_Entry'Class;
                        To_Table : in     Table);

end GCS.Entities.Entries;
