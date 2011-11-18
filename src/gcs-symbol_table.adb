------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--                     G C S . S Y M B O L _ T A B L E                      --
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

with GCS.AST;
with GCS.File_Manager;
with WL.Tables.Hash_Table;
with WL.Trace;

package body GCS.Symbol_Table is

   function To_Entry_Name (S : String)     return Entry_Name is
      Name : Entry_Name := (Text   => (others => Character'Val (0)),
                            Length => S'Length);
   begin
      Name.Text (1 .. Name.Length) := S;
      return Name;
   end To_Entry_Name;

   function To_String     (E : Entry_Name) return String is
   begin
      return E.Text (1 .. E.Length);
   end To_String;

   procedure Initialise
     (T     : in out Root_Table_Entry'Class;
      Name  : in     String;
      Decl  : access GCS.AST.Tree_Node'Class)
   is
   begin
      T.Name := To_Entry_Name (Name);
      T.Decl := AST.Tree (Decl);
   end Initialise;

   procedure Initialise
     (T     : in out Root_Table_Entry'Class;
      Name  : in     String)
   is
   begin
      T.Name := To_Entry_Name (Name);
      T.Decl := null;
   end Initialise;

   function File_Name
     (T : access Root_Table_Entry'Class)
      return String is
   begin
      return GCS.AST.File_Name (T.Decl);
   end File_Name;

   function File
     (T : access Root_Table_Entry'Class)
      return Source_File_Type is
   begin
      return GCS.AST.File (T.Decl);
   end File;

   function Line
     (T : access Root_Table_Entry'Class)
      return Line_Number is
   begin
      return GCS.AST.Line (T.Decl);
   end Line;

   function Column
     (T : access Root_Table_Entry'Class)
      return Column_Number is
   begin
      return GCS.AST.Column (T.Decl);
   end Column;

   function Name
     (T : access Root_Table_Entry'Class)
      return String is
   begin
      return To_String (T.Name);
   end Name;

   function Declaration
     (T : access Root_Table_Entry'Class)
      return AST.Tree is
   begin
      return T.Decl;
   end Declaration;

   function File_Name
     (T : in Root_Table_Entry'Class)
      return String is
   begin
      return GCS.AST.File_Name (T.Decl);
   end File_Name;

   function File
     (T : in Root_Table_Entry'Class)
      return Source_File_Type is
   begin
      return GCS.AST.File (T.Decl);
   end File;

   function Line
     (T : in Root_Table_Entry'Class)
      return Line_Number is
   begin
      return GCS.AST.Line (T.Decl);
   end Line;

   function Column
     (T : in Root_Table_Entry'Class)
      return Column_Number is
   begin
      return GCS.AST.Column (T.Decl);
   end Column;

   function Name
     (T : in Root_Table_Entry'Class)
      return String is
   begin
      return To_String (T.Name);
   end Name;

   function Declaration
     (T : in Root_Table_Entry'Class)
      return AST.Tree is
   begin
      return T.Decl;
   end Declaration;

   procedure Set_Table (E : access Root_Table_Entry'Class;
                        T : access Root_Symbol_Table'Class) is
   begin
      E.Parent_Table := Table (T);
   end Set_Table;

   function  Get_Table (E : access Root_Table_Entry'Class) return Table is
   begin
      return E.Parent_Table;
   end Get_Table;

   procedure New_Table (R            : access Root_Symbol_Table'Class;
                        Name         : in     String;
                        Parent       : in     Table;
                        Parent_Entry : in     Table_Entry)
   is
   begin
      R.Name         := new String'(Name);
      R.Parent       := Parent;
      R.Parent_Entry := Parent_Entry;
   end New_Table;

   procedure Trying_Parent (Child  : in     Table;
                            Parent : in     Table)
   is
   begin
      --  very trying
      if Parent = null then
         WL.Trace.Put_Line (Child.Name.all & "Trying parent called with null");
      else
         WL.Trace.Put_Line ("Trying parent: " & Child.Name.all &
                            " --> " & Parent.Name.all);
      end if;
      Child.Trying_Table := Parent;
   end Trying_Parent;

   function  Tried_Parent  (T : Table) return Boolean is
   begin
      WL.Trace.Put_Line (T.Name.all & ": Tried_Parent returns " &
                         Boolean'Image (T.Trying_Table /= null));

      return T.Trying_Table /= null;
   end Tried_Parent;

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
      return T.Name.all;
   end Name;

   function Name (Constraint : Name_Constraint'Class) return String is
   begin
      return Constraint.Name.Text (1 .. Constraint.Name.Length);
   end Name;

   function Match (Constraint : Name_Constraint;
                   E          : Table_Entry) return Boolean is
   begin
      return Name (E) = Name (Constraint);
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

   procedure New_Name_Constraint (Constraint  : in out Name_Constraint'Class;
                                  Name        : in     String) is
   begin
      Constraint.Name := (Text   => (others => Character'Val (0)),
                          Length => Name'Length);
      Constraint.Name.Text (1 .. Name'Length) := Name;
   end New_Name_Constraint;

   --  general constraint-based search procedure.
   procedure Search (T          : in     Table;
                     Constraint : in     Search_Constraint'Class;
                     E          : out    Table_Entry;
                     Result     : out    Search_Result) is
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

end GCS.Symbol_Table;
