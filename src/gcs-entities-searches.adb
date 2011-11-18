------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--                G C S . E N T I T I E S . S E A R C H E S                 --
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

with GCS.Entities.Entries;

package body GCS.Entities.Searches is

   function Name (Constraint : Name_Constraint'Class) return String is
   begin
      return To_String (Constraint.Name);
   end Name;

   function Match (Constraint : Name_Constraint;
                   E          : Table_Entry) return Boolean is
   begin
      return GCS.Entities.Entries.Name (E) = Name (Constraint);
   end Match;

   function First  (T          : in     Table;
                    Constraint : in     Name_Constraint)
                    return Table_Entry
   is
   begin
      return First (T, Name (Constraint));
   end First;

   function Next   (T          : in     Table;
                    Constraint : in     Name_Constraint)
                    return Table_Entry
   is
   begin
      return Next (T, Name (Constraint));
   end Next;

   procedure Initialise (Constraint  : in out Name_Constraint'Class;
                         Name        : in     String) is
   begin
      Constraint.Name := To_Entry_Name (Name);
   end Initialise;

end GCS.Entities.Searches;
