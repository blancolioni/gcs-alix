------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--                   G C S . E N T I T I E S . T R E E S                    --
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

with GCS.Constraints;                use GCS.Constraints;
with GCS.Errors;

package GCS.Entities.Trees is

   --  report an error using the given node's file, line and
   --  column information.
   procedure Error (Level   : in     GCS.Errors.Error_Level;
                    Node    : access Root_Tree_Node'Class;
                    Message : in     String);

   procedure Error (Node    : access Root_Tree_Node'Class;
                    Message : in     String);

   --  set the error context for error reports to this node.
   procedure Set_Context (T : access Root_Tree_Node'Class);

   --  give the node an explicit position for error reporting
   procedure Set_Position (T     : access Root_Tree_Node'Class;
                           Pos   : in     GCS.Positions.File_Position);

   --  functions for accessing node information
   function File_Name
     (T    : access Root_Tree_Node'Class)
      return String;

   function File
     (T    : access Root_Tree_Node'Class)
      return Source_File_Type;

   function Line
     (T    : access Root_Tree_Node'Class)
      return Line_Number;

   function Column
     (T    : access Root_Tree_Node'Class)
      return Column_Number;

   function Indent
     (T    : access Root_Tree_Node'Class)
      return Column_Number;

   function Parent
     (T    : access Root_Tree_Node'Class)
      return Tree;

   --  versions of accessor functions for unallocated nodes
   function File_Name
     (T    : in Root_Tree_Node'Class)
      return String;

   function File
     (T    : in Root_Tree_Node'Class)
      return Source_File_Type;

   function Line
     (T    : in Root_Tree_Node'Class)
      return Line_Number;

   function Column
     (T    : in Root_Tree_Node'Class)
      return Column_Number;

   function Indent
     (T    : in Root_Tree_Node'Class)
      return Column_Number;

   function Parent
     (T    : in Root_Tree_Node'Class)
      return Tree;

   --  Each node has an associated symbol table, which we call
   --  the environment.  Here's how to get it.
   function Environment (T : access Root_Tree_Node'Class) return Table;
   function Environment (T : in     Root_Tree_Node'Class) return Table;

   --  Here's how to set it.
   procedure Set_Environment (T    : access Root_Tree_Node'Class;
                              Env  : in     Table);

   --  You can also maintain the parent node
   procedure Set_Parent (T      : access Root_Tree_Node'Class;
                         Parent : in     Tree);

end GCS.Entities.Trees;
