------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--                G C S . E N T I T I E S . S E A R C H E S                 --
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

package GCS.Entities.Searches is

   --  This package contains basic search constraints

   ------------------------------------------------------------
   --  Search_Constraint

   --  type Search_Constraint is a base class for search
   --  constraints, which act like iterators on symbol tables

   type Search_Constraint is abstract tagged private;

   function Match (Constraint : Search_Constraint;
                   E          : Table_Entry) return Boolean is abstract;

   function First  (T          : in     Table;
                    Constraint : in     Search_Constraint)
                    return Table_Entry is abstract;

   function Next   (T          : in     Table;
                    Constraint : in     Search_Constraint)
                    return Table_Entry is abstract;

   --  type Name_Constraint constrains a search by name.
   --  I expect that most constraints will be derived from this one.
   type Name_Constraint is new Search_Constraint with private;

   function Match (Constraint : in Name_Constraint;
                   E          : in Table_Entry) return Boolean;

   function First  (T          : in     Table;
                    Constraint : in     Name_Constraint)
                    return Table_Entry;

   function Next   (T          : in     Table;
                    Constraint : in     Name_Constraint)
                    return Table_Entry;

   procedure Initialise (Constraint  : in out Name_Constraint'Class;
                         Name        : in     String);

   function Name (Constraint : Name_Constraint'Class) return String;

private

   type Search_Constraint is abstract tagged
      record
         Name : Entry_Name;
      end record;

   type Name_Constraint is new Search_Constraint with null record;

end GCS.Entities.Searches;
