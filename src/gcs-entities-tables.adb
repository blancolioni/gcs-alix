------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--                  G C S . E N T I T I E S . T A B L E S                   --
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

package body GCS.Entities.Tables is

   procedure Trying_Parent (Child  : in     Table;
                            Parent : in     Table)
   is
   begin
      --  very trying
      Child.Trying_Table := Parent;
   end Trying_Parent;

   function  Tried_Parent  (T : Table) return Boolean is
   begin
      return T.Trying_Table /= null;
   end Tried_Parent;

   procedure Set_Owner (T        : Table;
                        To_Entry : Table_Entry)
   is
   begin
      T.Parent_Entry := To_Entry;
   end Set_Owner;

   function Parent (T : Table) return Table is
   begin
      return T.Parent;
   end Parent;

   function Owner  (T : Table) return Table_Entry is
   begin
      return T.Parent_Entry;
   end Owner;

   function Name (T : Table) return String is
   begin
      return Ada.Strings.Unbounded.To_String (T.Name);
   end Name;

   procedure Search
     (T          : in     Table;
      Constraint : in     GCS.Entities.Searches.Search_Constraint'Class;
      E          : out    Table_Entry;
      Result     : out    Search_Result)
   is
      use GCS.Entities.Searches;
      E2 : Table_Entry;
   begin
      Result := Not_Found;
      E := First (T, Constraint);
      if E /= null then
         Result := Found;
         E2 := Next (T, Constraint);
         if E2 /= null then
            Result := Not_Unique;
         end if;
      end if;

      if Result = Not_Found and T.Parent /= null then
         Search (T.Parent, Constraint, E, Result);
      end if;

   end Search;

end GCS.Entities.Tables;
