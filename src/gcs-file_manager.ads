------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--                     G C S . F I L E _ M A N A G E R                      --
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

with GCS.Constraints;                  use GCS.Constraints;

private package GCS.File_Manager is

   pragma Elaborate_Body;

   procedure Open  (Name : String);
   procedure Open_Standard_Input;

   procedure Close;

   procedure Open_String (Text : String);

   procedure Add_Include_Path (Path   : String);

   function Current_File return Source_File_Type;
   function Current_File_Name return String;
   function Current_File_Title return String;
   function Get_File_Name (F : Source_File_Type) return String;

   function Current_Line return Line_Number;
   function Current_Column return Column_Number;
   function Current_Indent return Column_Number;
   function Current_Line_Text return String;

   function Current_Character return Character;
   function End_Of_Line return Boolean;
   function End_Of_File return Boolean;

   function Match (Text       : String;
                   Skip_Match : Boolean := False) return Boolean;

   procedure Skip;
   procedure Unskip;
   procedure Skip_Spaces;
   procedure Next_Line;

   procedure Set_Current_Position (File   : in Source_File_Type;
                                   Line   : in Line_Number;
                                   Col    : in Column_Number;
                                   Indent : in Column_Number);

   procedure Get_Current_Position (File   : out Source_File_Type;
                                   Line   : out Line_Number;
                                   Col    : out Column_Count;
                                   Indent : out Column_Count);

   procedure Add_Hash (Text : String);
   function Current_Hash return File_Hash;

private

   pragma Inline (Skip);

end GCS.File_Manager;

