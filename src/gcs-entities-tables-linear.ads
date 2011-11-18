------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--           G C S . E N T I T I E S . T A B L E S . L I N E A R            --
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

with WL.Lists.Generic_List;

package GCS.Entities.Tables.Linear is

   pragma Elaborate_Body;

   function New_Table (Name   : in String;
                       Parent : in Table       := null;
                       Size   : in Natural     := Default_Table_Size;
                       Owner  : in Table_Entry := null) return Table;

private

   --  Use a list to store entries
   package List_Of_Entries is new WL.Lists.Generic_List (Table_Entry);

   type Linear_Symbol_Table is new Root_Symbol_Table with
      record
         Store   : List_Of_Entries.List;
         Pos     : List_Of_Entries.Iterator;
         Tried_Parent : Boolean;
      end record;

   procedure Enter (T : access Linear_Symbol_Table;
                    E : access Root_Table_Entry'Class);

   procedure Replace (T         : access Linear_Symbol_Table;
                      Old_Entry : access Root_Table_Entry'Class;
                      New_Entry : access Root_Table_Entry'Class);

   procedure Search (T      : access Linear_Symbol_Table;
                     Name   : in     String;
                     E      : out    Table_Entry;
                     Result : out    Search_Result);

   function First  (T      : access Linear_Symbol_Table;
                    Name   : in     String)
                    return Table_Entry;

   function Next   (T      : access Linear_Symbol_Table;
                    Name   : in     String)
                    return Table_Entry;

   function First (T : access Linear_Symbol_Table)
                   return Table_Entry;
   function Next  (T : access Linear_Symbol_Table)
                   return Table_Entry;

   procedure Dump (T : access Linear_Symbol_Table);

end GCS.Entities.Tables.Linear;
