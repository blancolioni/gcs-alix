------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--                  G C S . E N T I T I E S . T A B L E S                   --
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

with GCS.Entities.Searches;

package GCS.Entities.Tables is

   --  Default table size
   --  Not all tables will use this
   Default_Table_Size : Natural := 709;

   --  Non-primitive table operations

   --  When a search hits parent tables, call this procedure
   --  to tell the child table that the parent
   --  is where calls to Next should continue from.
   procedure Trying_Parent (Child  : in     Table;
                            Parent : in     Table);

   function Tried_Parent  (T : Table) return Boolean;

   function Parent (T : Table) return Table;
   function Owner  (T : Table) return Table_Entry;
   function Name   (T : Table) return String;

   procedure Set_Owner (T        : Table;
                        To_Entry : Table_Entry);

   --  general constraint-based search procedure.
   procedure Search
     (T          : in     Table;
      Constraint : in     GCS.Entities.Searches.Search_Constraint'Class;
      E          : out    Table_Entry;
      Result     : out    Search_Result);

end GCS.Entities.Tables;
