------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--                   G C S . E N T I T I E S . T R E E S                    --
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

with GCS.File_Manager;

package body GCS.Entities.Trees is

   procedure Set_Context (T : access Root_Tree_Node'Class) is
   begin
      GCS.Errors.Set_Context (GCS.Positions.Get_File (T.Pos),
                              GCS.Positions.Get_Line (T.Pos),
                              GCS.Positions.Get_Column (T.Pos));
   end Set_Context;

   procedure Set_Position (T     : access Root_Tree_Node'Class;
                           Pos   : in     GCS.Positions.File_Position)
   is
   begin
      T.Pos := Pos;
   end Set_Position;

   procedure Error (Level   : in     GCS.Errors.Error_Level;
                    Node    : access Root_Tree_Node'Class;
                    Message : in     String)
   is
   begin
      GCS.Errors.Error (Level, File_Name (Node),
                        GCS.Positions.Get_Line (Node.Pos),
                        GCS.Positions.Get_Column (Node.Pos),
                        Message);
   end Error;

   procedure Error (Node    : access Root_Tree_Node'Class;
                    Message : in     String)
   is
   begin
      Error (GCS.Errors.Error, Node, Message);
   end Error;

   function File_Name (T    : access Root_Tree_Node'Class) return String is
   begin
      return GCS.File_Manager.Get_File_Name (GCS.Positions.Get_File (T.Pos));
   end File_Name;

   function File (T    : access Root_Tree_Node'Class)
                  return Source_File_Type is
   begin
      return GCS.Positions.Get_File (T.Pos);
   end File;

   function Line (T    : access Root_Tree_Node'Class) return Line_Number is
   begin
      return GCS.Positions.Get_Line (T.Pos);
   end Line;

   function Column (T    : access Root_Tree_Node'Class) return Column_Number is
   begin
      return GCS.Positions.Get_Column (T.Pos);
   end Column;

   function Indent (T    : access Root_Tree_Node'Class) return Column_Number is
   begin
      return GCS.Positions.Get_Indent (T.Pos);
   end Indent;

   function Parent (T    : access Root_Tree_Node'Class) return Tree is
   begin
      return T.Parent;
   end Parent;

   function File_Name (T    : in Root_Tree_Node'Class) return String is
   begin
      return GCS.File_Manager.Get_File_Name (GCS.Positions.Get_File (T.Pos));
   end File_Name;

   function File (T    : in Root_Tree_Node'Class) return Source_File_Type is
   begin
      return GCS.Positions.Get_File (T.Pos);
   end File;

   function Line (T    : in Root_Tree_Node'Class) return Line_Number is
   begin
      return GCS.Positions.Get_Line (T.Pos);
   end Line;

   function Column (T    : in Root_Tree_Node'Class) return Column_Number is
   begin
      return GCS.Positions.Get_Column (T.Pos);
   end Column;

   function Indent (T    : in Root_Tree_Node'Class) return Column_Number is
   begin
      return GCS.Positions.Get_Indent (T.Pos);
   end Indent;

   function Parent (T    : in Root_Tree_Node'Class) return Tree is
   begin
      return T.Parent;
   end Parent;

   -----------------
   -- Environment --
   -----------------
   function Environment (T : access Root_Tree_Node'Class) return Table is
   begin
      return T.Env;
   end Environment;

   -----------------
   -- Environment --
   -----------------
   function Environment (T : in     Root_Tree_Node'Class) return Table is
   begin
      return T.Env;
   end Environment;


   ---------------------
   -- Set_Environment --
   ---------------------
   procedure Set_Environment (T    : access Root_Tree_Node'Class;
                              Env  : in     Table)
   is
   begin
      T.Env := Env;
   end Set_Environment;

   ----------------
   -- Set_Parent --
   ----------------
   procedure Set_Parent (T      : access Root_Tree_Node'Class;
                         Parent : in     Tree)
   is
   begin
      T.Parent := Parent;
   end Set_Parent;

end GCS.Entities.Trees;
