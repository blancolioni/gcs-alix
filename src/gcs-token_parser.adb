------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--                     G C S . T O K E N _ P A R S E R                      --
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

with Ada.Text_IO;

package body GCS.Token_Parser is

   use List_Of_States;

   type Array_Of_State_Lists is array (Positive range <>) of Dynamic_Array;
   type Array_Of_State_List_Access is access Array_Of_State_Lists;

   type Character_Map is array (Character) of Natural;

   State_Machine : Array_Of_State_List_Access;
   Map           : Character_Map := (others => 0);
   State_Count   : Natural := 0;

   procedure Set_Symbol_Characters (Chars : String) is
      Char_Count    : Natural;
   begin
      Char_Count := 0;
      for I in Chars'Range loop
         if Chars (I) /= ' ' then
            if Map (Chars (I)) = 0 then
               Char_Count := Char_Count + 1;
               Map (Chars (I)) := Char_Count;
            end if;
         end if;
      end loop;

      Char_Count := Char_Count + 1;
      Map (' ') := Char_Count;
      State_Machine := new Array_Of_State_Lists (1 .. Char_Count);
   end Set_Symbol_Characters;

   function Get_Command (Char    : Character;
                         State   : Positive) return State_Command
   is
   begin
      if State > Size (State_Machine (Map (Char))) then
         return No_Command;
      else
         return Get (State_Machine (Map (Char)), State).Command;
      end if;
   end Get_Command;

   function Get_Result (Char  : Character;
                        State : Positive) return Positive is
   begin
      return Get (State_Machine (Map (Char)), State).Result;
   end Get_Result;

   function New_State return Positive is
      E : constant State_Entry := (No_Command, 1);
   begin
      State_Count := State_Count + 1;
      for I in State_Machine.all'Range loop
         Set (State_Machine (I), State_Count, E);
      end loop;
      return State_Count;
   end New_State;

   procedure Fill_State (State   : Positive;
                         Cmd     : State_Command;
                         Result  : Positive)
   is
      E : constant State_Entry := (Cmd, Result);
   begin
      for I in State_Machine.all'Range loop
         Set (State_Machine (I), State, E);
      end loop;
   end Fill_State;


   procedure Fill_State (State   : Positive;
                         Filter  : State_Command;
                         Cmd     : State_Command;
                         Result  : Positive)
   is
      E : constant State_Entry := (Cmd, Result);
   begin
      for I in State_Machine.all'Range loop
         if Get (State_Machine (I), State).Command = Filter then
            Set (State_Machine (I), State, E);
         end if;
      end loop;
   end Fill_State;


   procedure Set_Command (Char   : Character;
                          State  : Positive;
                          Cmd    : State_Command;
                          Result : Positive)
   is
   begin
      Set (State_Machine (Map (Char)), State, (Cmd, Result));
   end Set_Command;

   procedure Dump_State_Table is
      use Ada.Text_IO;
      Ch : Character;
   begin
      for I in 1 .. Positive_Count (State_Count) loop
         Set_Col (I * 6);
         Put (Positive_Count'Image (I));
      end loop;
      New_Line;

      for I in State_Machine.all'Range loop
         for J in Character loop
            if Map (J) = I then
               Ch := J;
               Put (Ch);
               exit;
            end if;
         end loop;

         for Index in 1 .. State_Count loop
            Set_Col (Positive_Count (Index * 6));
            case Get_Command (Ch, Index) is
               when No_Command =>
                  Put ("X");
               when Accept_And_Skip =>
                  Put ("AS" & Integer'Image (-Get_Result (Ch, Index)));
               when Accept_And_Leave =>
                  Put ("AL" & Integer'Image (-Get_Result (Ch, Index)));
               when Skip_And_Change =>
                  Put ("SC" & Integer'Image (-Get_Result (Ch, Index)));
            end case;
         end loop;
         New_Line;
      end loop;

   end Dump_State_Table;

end GCS.Token_Parser;
