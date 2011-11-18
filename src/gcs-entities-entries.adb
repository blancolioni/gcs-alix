------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--                 G C S . E N T I T I E S . E N T R I E S                  --
--                                                                          --
--                                 B o d y                                  --
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

with GCS.Entities.Trees;

package body GCS.Entities.Entries is

   procedure Initialise
     (T     : in out Root_Table_Entry'Class;
      Name  : in     String;
      Decl  : in     Tree)
   is
   begin
      T.Name := To_Entry_Name (Name);
      T.Decl := Decl;
   end Initialise;

   procedure Initialise
     (T     : in out Root_Table_Entry'Class;
      Name  : in     String)
   is
   begin
      T.Name := To_Entry_Name (Name);
      T.Decl := null;
   end Initialise;

   function File_Name
     (T : access Root_Table_Entry'Class)
      return String is
   begin
      return GCS.Entities.Trees.File_Name (T.Decl);
   end File_Name;

   function File
     (T : access Root_Table_Entry'Class)
      return Source_File_Type is
   begin
      return GCS.Entities.Trees.File (T.Decl);
   end File;

   function Line
     (T : access Root_Table_Entry'Class)
      return Line_Number is
   begin
      return GCS.Entities.Trees.Line (T.Decl);
   end Line;

   function Column
     (T : access Root_Table_Entry'Class)
      return Column_Number is
   begin
      return GCS.Entities.Trees.Column (T.Decl);
   end Column;

   function Name
     (T : access Root_Table_Entry'Class)
      return String is
   begin
      return To_String (T.Name);
   end Name;

   function Declaration
     (T : access Root_Table_Entry'Class)
      return Tree is
   begin
      return T.Decl;
   end Declaration;

   function File_Name
     (T : in Root_Table_Entry'Class)
      return String is
   begin
      return GCS.Entities.Trees.File_Name (T.Decl);
   end File_Name;

   function File
     (T : in Root_Table_Entry'Class)
      return Source_File_Type is
   begin
      return GCS.Entities.Trees.File (T.Decl);
   end File;

   function Line
     (T : in Root_Table_Entry'Class)
      return Line_Number is
   begin
      return GCS.Entities.Trees.Line (T.Decl);
   end Line;

   function Column
     (T : in Root_Table_Entry'Class)
      return Column_Number is
   begin
      return GCS.Entities.Trees.Column (T.Decl);
   end Column;

   function Name
     (T : in Root_Table_Entry'Class)
      return String is
   begin
      return To_String (T.Name);
   end Name;

   function Declaration
     (T : in Root_Table_Entry'Class)
      return Tree is
   begin
      return T.Decl;
   end Declaration;

   procedure Set_Table (E : access Root_Table_Entry'Class;
                        T : access Root_Symbol_Table'Class) is
   begin
      E.Parent_Table := Table (T);
   end Set_Table;

   function  Get_Table (E : access Root_Table_Entry'Class) return Table is
   begin
      return E.Parent_Table;
   end Get_Table;

   procedure Set_Table (E        : access Root_Table_Entry'Class;
                        To_Table : in     Table)
   is
   begin
      E.Parent_Table := To_Table;
   end Set_Table;

end GCS.Entities.Entries;
