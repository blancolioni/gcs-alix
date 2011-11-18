------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--                              G C S . A S T                               --
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

with GCS.Constraints;            use GCS.Constraints;
with GCS.Errors;
with GCS.File_Manager;           use GCS.File_Manager;

package body GCS.AST is

   procedure Set_Context (T : access Tree_Node'Class) is
   begin
      GCS.Errors.Set_Context (T.File, T.Line, T.Column);
   end Set_Context;

   procedure Warning (T       : access Tree_Node'Class;
                      Message : String) is
   begin
      GCS.Errors.Warning (Get_File_Name (T.File),
                          T.Line, T.Column, Message);
   end Warning;

   procedure Error (T       : access Tree_Node'Class;
                    Message : String) is
   begin
      GCS.Errors.Error (Get_File_Name (T.File),
                        T.Line, T.Column, Message);
   end Error;

   procedure Fatal_Error (T       : access Tree_Node'Class;
                          Message : String) is
   begin
      GCS.Errors.Fatal_Error (Get_File_Name (T.File),
                              T.Line, T.Column, Message);
   end Fatal_Error;

   procedure Internal_Error (T       : access Tree_Node'Class;
                             Message : String) is
   begin
      GCS.Errors.Internal_Error (Get_File_Name (T.File),
                                 T.Line, T.Column, Message);
   end Internal_Error;

   function File_Name (T    : access Tree_Node'Class) return String is
   begin
      return Get_File_Name (T.File);
   end File_Name;

   function File (T    : access Tree_Node'Class) return Source_File_Type is
   begin
      return T.File;
   end File;

   function Line (T    : access Tree_Node'Class) return Line_Number is
   begin
      return T.Line;
   end Line;

   function Column (T    : access Tree_Node'Class) return Column_Number is
   begin
      return T.Column;
   end Column;

   function Indent (T    : access Tree_Node'Class) return Column_Number is
   begin
      return T.Indent;
   end Indent;

   function Parent (T    : access Tree_Node'Class) return Tree is
   begin
      return T.Parent;
   end Parent;

   function File_Name (T    : in Tree_Node'Class) return String is
   begin
      return Get_File_Name (T.File);
   end File_Name;

   function File (T    : in Tree_Node'Class) return Source_File_Type is
   begin
      return T.File;
   end File;

   function Line (T    : in Tree_Node'Class) return Line_Number is
   begin
      return T.Line;
   end Line;

   function Column (T    : in Tree_Node'Class) return Column_Number is
   begin
      return T.Column;
   end Column;

   function Indent (T    : in Tree_Node'Class) return Column_Number is
   begin
      return T.Indent;
   end Indent;

   function Parent (T    : in Tree_Node'Class) return Tree is
   begin
      return T.Parent;
   end Parent;

   procedure Initialise (T       : access Tree_Node'Class;
                         Parent  : access Tree_Node'Class)
   is
   begin
      T.Parent := Tree (Parent);
   end Initialise;

   procedure Initialise (T       : access Tree_Node'Class) is
   begin
      null;
   end Initialise;

   procedure Initialize (T : in out Tree_Node) is
   begin
     T.Parent := null;
     Get_Current_Position (T.File, T.Line, T.Column, T.Indent);
   end Initialize;

end GCS.AST;
