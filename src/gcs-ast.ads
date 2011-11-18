------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--                              G C S . A S T                               --
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
with GCS.Constraints;            use GCS.Constraints;

package GCS.AST is

   pragma Elaborate_Body;

   type Tree_Node is abstract tagged private;

   type Tree is access all Tree_Node'Class;

   --  The AST versions of the error procedures call
   --  GCS.Errors with the file name, line and column
   --  contained in the given tree node.
   procedure Warning        (T       : access Tree_Node'Class;
                             Message : String);
   procedure Error          (T       : access Tree_Node'Class;
                             Message : String);
   procedure Fatal_Error    (T       : access Tree_Node'Class;
                             Message : String);
   procedure Internal_Error (T       : access Tree_Node'Class;
                             Message : String);

   procedure Set_Context (T : access Tree_Node'Class);

   function File_Name (T    : access Tree_Node'Class) return String;
   function File (T    : access Tree_Node'Class) return Source_File_Type;
   function Line (T    : access Tree_Node'Class) return Line_Number;
   function Column (T    : access Tree_Node'Class) return Column_Number;
   function Indent (T    : access Tree_Node'Class) return Column_Number;

   function Parent (T    : access Tree_Node'Class) return Tree;

   function File_Name (T    : in Tree_Node'Class) return String;
   function File (T    : in Tree_Node'Class) return Source_File_Type;
   function Line (T    : in Tree_Node'Class) return Line_Number;
   function Column (T    : in Tree_Node'Class) return Column_Number;
   function Indent (T    : in Tree_Node'Class) return Column_Number;

   function Parent (T    : in Tree_Node'Class) return Tree;

   procedure Initialise (T       : access Tree_Node'Class;
                         Parent  : access Tree_Node'Class);

   procedure Initialise (T       : access Tree_Node'Class);

private

   type Tree_Node is abstract new Ada.Finalization.Controlled with
      record
         Parent    : Tree;
         Line      : Line_Number;
         Column    : Column_Number;
         Indent    : Column_Number;
         File      : Source_File_Type;
      end record;

   procedure Initialize (T : in out Tree_Node);

end GCS.AST;
