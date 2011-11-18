------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--                         G C S . E N T I T I E S                          --
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

with Ada.Finalization;
with Ada.Strings.Unbounded;

with GCS.Constraints;
with GCS.Positions;

package GCS.Entities is

   pragma Elaborate_Body;

   --  Three types of results from searches.
   type Search_Result is (Not_Found, Not_Unique, Found);

   --  How we store names
   type Entry_Name is private;
   function To_Entry_Name (S : String)     return Entry_Name;
   function To_String     (E : Entry_Name) return String;

   -------------------------------------------------------------
   --  Root_Table_Entry, Table_Entry

   type Root_Table_Entry is abstract tagged private;

   type Table_Entry is access all Root_Table_Entry'Class;

   ------------------------------------------------------------
   --  Root_Tree_Node, Tree

   type Root_Tree_Node is abstract tagged private;
   type Tree is access all Root_Tree_Node'Class;

   ------------------------------------------------------------
   --  Root_Symbol_Table
   --
   --  type Root_Symbol_Table provides basic table operations.

   type Root_Symbol_Table is abstract tagged limited private;

   procedure Enter (T : access Root_Symbol_Table;
                    E : access Root_Table_Entry'Class) is abstract;

   procedure Replace (T         : access Root_Symbol_Table;
                      Old_Entry : access Root_Table_Entry'Class;
                      New_Entry : access Root_Table_Entry'Class)
      is abstract;

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



private

   type Entry_Name is new Ada.Strings.Unbounded.Unbounded_String;

   type Root_Table_Entry is abstract tagged
      record
         Decl         : Tree;
         Name         : Entry_Name;
         Parent_Table : Table;
      end record;

   type Root_Tree_Node is abstract new Ada.Finalization.Controlled with
      record
         Parent    : Tree;
         Pos       : GCS.Positions.File_Position;
         Env       : Table;
      end record;

   procedure Initialize (T : in out Root_Tree_Node);

   type Root_Symbol_Table is abstract tagged limited
      record
         Name         : Ada.Strings.Unbounded.Unbounded_String;
         Parent       : Table;
         Parent_Entry : Table_Entry;
         Trying_Table : Table;
      end record;

   procedure New_Table (R            : access Root_Symbol_Table'Class;
                        Name         : in     String;
                        Parent       : in     Table;
                        Parent_Entry : in     Table_Entry);

end GCS.Entities;
