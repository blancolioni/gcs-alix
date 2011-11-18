------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--              G C S . S Y M B O L _ T A B L E . H A S H E D               --
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

with WL.Tables.Hash_Table;
pragma Elaborate_All (WL.Tables.Hash_Table);

package GCS.Symbol_Table.Hashed is

   pragma Elaborate_Body;

   function New_Table (Name   : in String;
                       Parent : in Table       := null;
                       Size   : in Natural     := 0;
                       Owner  : in Table_Entry := null) return Table;

private

   --  Use a hash table to store entries
   function Get_Name (T : Table_Entry) return String;
   package Simple_Hash_Table is
      new WL.Tables.Hash_Table (Element_Type     => Table_Entry,
                                Get_Key          => Get_Name);

   type Hashed_Symbol_Table is new Root_Symbol_Table with
      record
         Store   : Simple_Hash_Table.Table;
         Pos     : Simple_Hash_Table.Position;
      end record;

   procedure Enter (T : access Hashed_Symbol_Table;
                    E : access Root_Table_Entry'Class);

   procedure Search (T      : access Hashed_Symbol_Table;
                     Name   : in     String;
                     E      : out    Table_Entry;
                     Result : out    Search_Result);

   function First  (T      : access Hashed_Symbol_Table;
                    Name   : in     String)
                    return Table_Entry;

   function Next   (T      : access Hashed_Symbol_Table;
                    Name   : in     String)
                    return Table_Entry;

   function First (T : access Hashed_Symbol_Table)
                   return Table_Entry;
   function Next  (T : access Hashed_Symbol_Table)
                   return Table_Entry;

   procedure Dump (T : access Hashed_Symbol_Table);

end GCS.Symbol_Table.Hashed;
