------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--                           G C S . E R R O R S                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.2 $                              --
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

with GCS.Constraints;                   use GCS.Constraints;
with GCS.Positions;

package GCS.Errors is

   --  Standard error package.  There are four error levels: warning,
   --  non-fatal error, fatal error and internal error.  Perhaps this
   --  should be an enumerated type with properties.

   Hit_Internal_Error : exception;
   Hit_Fatal_Error    : exception;

   type Error_Level is (Warning, Error, Fatal, Internal);

   function Has_Warnings return Boolean;
   function Has_Errors   return Boolean;
   function Has_Fatal_Error return Boolean;

   procedure Clear_Errors;
   --  Clear all errors and warnings

   procedure Warning (File_Name   : String;
                      Line        : Line_Number;
                      Column      : Column_Number;
                      Message     : String);

   procedure Error (File_Name   : String;
                    Line        : Line_Number;
                    Column      : Column_Count;
                    Message     : String);

   procedure Fatal_Error (File_Name   : String;
                          Line        : Line_Number;
                          Column      : Column_Number;
                          Message     : String);

   procedure Internal_Error (File_Name   : String;
                             Line        : Line_Number;
                             Column      : Column_Number;
                             Message     : String);

   procedure Set_Context (File_Index  : Source_File_Type;
                          Line        : Line_Number;
                          Column      : Column_Number);

   procedure Error (Level    : Error_Level;
                    Message  : String);

   procedure Error (Level       : in Error_Level;
                    File_Name   : in String;
                    Line        : in Line_Number;
                    Column      : in Column_Count;
                    Message     : in String);

   procedure Error (Level    : Error_Level;
                    Location : GCS.Positions.File_Position;
                    Message  : String);

   procedure Error (Location : GCS.Positions.File_Position;
                    Message  : String);

end GCS.Errors;
