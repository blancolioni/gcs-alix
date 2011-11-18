------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--                     G C S . S Y M B O L _ T A B L E                      --
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

with GCS.AST;
with GCS.Constraints;             use GCS.Constraints;
with WL.Tables.Hash_Table;
with WL.Strings;

package GCS.Symbol_Table is

   --  Root_Table_Entry, Table_Entry
   type Root_Table_Entry is abstract tagged private;

   type Table_Entry is access all Root_Table_Entry'Class;

   --  How we store names
   type Entry_Name is private;
   function To_Entry_Name (S : String)     return Entry_Name;
   function To_String     (E : Entry_Name) return String;

   --  Base class initialisation
   procedure Initialise
     (T     : in out Root_Table_Entry'Class;
      Name  : in     String;
      Decl  : access AST.Tree_Node'Class);

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
      return AST.Tree;

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
      return AST.Tree;

   --  Three types of results from searches.
   type Search_Result is (Not_Found, Not_Unique, Found);


   --  type Root_Symbol_Table provides basic table operations.
   type Root_Symbol_Table is abstract tagged limited private;
   procedure Enter (T : access Root_Symbol_Table;
                    E : access Root_Table_Entry'Class) is abstract;

   procedure Search (T      : access Root_Symbol_Table;
                     Name   : in     String;
                     E      : out    Table_Entry;
                     Result : out    Search_Result) is abstract;

   function First  (T      : access Root_Symbol_Table;
                    Name   : in     String)
                    return Table_Entry is abstract;

   function Next   (T      : access Root_Symbol_Table;
                    Name   : in     String)
                    return Table_Entry is abstract;

   function First (T : access Root_Symbol_Table)
                   return Table_Entry is abstract;
   function Next  (T : access Root_Symbol_Table)
                   return Table_Entry is abstract;

   procedure Dump (T : access Root_Symbol_Table) is abstract;

   type Table is access all Root_Symbol_Table'Class;

   --  When a search hits parent tables, call this procedure
   --  to tell the child table that the parent
   --  is where calls to Next should continue from.
   procedure Trying_Parent (Child  : in     Table;
                            Parent : in     Table);
   function  Tried_Parent  (T : Table) return Boolean;
   function Parent (T : Table) return Table;
   function Owner  (T : Table) return Table_Entry;
   function Name (T : Table) return String;

   function  Get_Table (E : access Root_Table_Entry'Class) return Table;

   --  type Search_Constraint is a base class for search
   --  constraints, which act like iterators on symbol tables

   type Search_Constraint is abstract tagged private;

   function Match (Constraint : Search_Constraint;
                   E          : Table_Entry) return Boolean is abstract;

   function First  (T          : in     Table;
                    Constraint : in     Search_Constraint)
                    return Table_Entry is abstract;

   function Next   (T          : in     Table;
                    Constraint : in     Search_Constraint)
                    return Table_Entry is abstract;

   --  type Name_Constraint constrains a search by name.
   type Name_Constraint is new Search_Constraint with private;

   function Match (Constraint : in Name_Constraint;
                   E          : in Table_Entry) return Boolean;

   function First  (T          : in     Table;
                    Constraint : in     Name_Constraint)
                    return Table_Entry;

   function Next   (T          : in     Table;
                    Constraint : in     Name_Constraint)
                    return Table_Entry;

   procedure New_Name_Constraint (Constraint  : in out Name_Constraint'Class;
                                  Name        : in     String);

   function Name (Constraint : Name_Constraint'Class) return String;

   --  general constraint-based search procedure.
   procedure Search (T          : in     Table;
                     Constraint : in     Search_Constraint'Class;
                     E          : out    Table_Entry;
                     Result     : out    Search_Result);

private

   type Entry_Name is
      record
         Length  : Natural;
         Text    : String (1 .. Max_Identifier_Length);
      end record;

   type Root_Table_Entry is abstract tagged
      record
         Decl         : AST.Tree;
         Name         : Entry_Name;
         Parent_Table : Table;
      end record;

   procedure Set_Table (E : access Root_Table_Entry'Class;
                        T : access Root_Symbol_Table'Class);

   type Root_Symbol_Table is abstract tagged limited
      record
         Name         : WL.Strings.String_Access;
         Parent       : Table;
         Parent_Entry : Table_Entry;
         Trying_Table : Table;
      end record;

   procedure New_Table (R            : access Root_Symbol_Table'Class;
                        Name         : in     String;
                        Parent       : in     Table;
                        Parent_Entry : in     Table_Entry);

   type Search_Constraint is abstract tagged
      record
         Name : Entry_Name;
      end record;

   type Name_Constraint is new Search_Constraint with null record;

end GCS.Symbol_Table;
