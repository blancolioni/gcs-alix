------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--                     G C S . T O K E N _ P A R S E R                      --
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

with WL.Arrays.Dynamic;

generic
package GCS.Token_Parser is

   pragma Elaborate_Body;

   type State_Command is
      (No_Command, Accept_And_Skip, Accept_And_Leave, Skip_And_Change);

   procedure Set_Symbol_Characters (Chars : String);

   function Get_Command (Char    : Character;
                         State   : Positive) return State_Command;

   function Get_Result (Char  : Character;
                        State : Positive) return Positive;

   function New_State return Positive;

   procedure Fill_State (State   : Positive;
                         Cmd     : State_Command;
                         Result  : Positive);

   procedure Fill_State (State   : Positive;
                         Filter  : State_Command;
                         Cmd     : State_Command;
                         Result  : Positive);

   procedure Set_Command (Char   : Character;
                          State  : Positive;
                          Cmd    : State_Command;
                          Result : Positive);

   procedure Dump_State_Table;

private

   type State_Entry is
      record
         Command      : State_Command;
         Result       : Natural;
      end record;

   package List_Of_States is new WL.Arrays.Dynamic (Positive, State_Entry);

end GCS.Token_Parser;
