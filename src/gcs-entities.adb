------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--                         G C S . E N T I T I E S                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1.1.1 $                          --
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

with GCS.Positions;

package body GCS.Entities is

   -------------------
   -- To_Entry_Name --
   -------------------

   function To_Entry_Name (S : String) return Entry_Name is
   begin
      return To_Unbounded_String (S);
   end To_Entry_Name;

   ---------------
   -- To_String --
   ---------------

   function To_String (E : Entry_Name) return String is
   begin
      return Ada.Strings.Unbounded.To_String
        (Ada.Strings.Unbounded.Unbounded_String (E));
   end To_String;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (T : in out Root_Tree_Node) is
   begin
      T.Parent := null;
      T.Pos := GCS.Positions.Get_Current_Position;
   end Initialize;

   ---------------
   -- New_Table --
   ---------------
   procedure New_Table (R            : access Root_Symbol_Table'Class;
                        Name         : in     String;
                        Parent       : in     Table;
                        Parent_Entry : in     Table_Entry)
   is
   begin
      R.Name         := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      R.Parent       := Parent;
      R.Parent_Entry := Parent_Entry;
   end New_Table;

end GCS.Entities;

