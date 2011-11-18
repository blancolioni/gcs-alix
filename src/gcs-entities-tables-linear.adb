------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--           G C S . E N T I T I E S . T A B L E S . L I N E A R            --
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

with WL.Trace;

with GCS.Entities.Entries;
with GCS.Exceptions;

package body GCS.Entities.Tables.Linear is

   type Table_Access is access all Linear_Symbol_Table'Class;

   procedure Enter (T : access Linear_Symbol_Table;
                    E : access Root_Table_Entry'Class) is
   begin
      List_Of_Entries.Append (T.Store, Table_Entry (E));
      GCS.Entities.Entries.Set_Table (E, Table (T));
   end Enter;

   procedure Replace (T         : access Linear_Symbol_Table;
                      Old_Entry : access Root_Table_Entry'Class;
                      New_Entry : access Root_Table_Entry'Class)
   is
      use List_Of_Entries;
      It : Iterator := Get_Start (T.Store);
   begin

      while not Off_Right (It) and then
        Current (It) /= Table_Entry (Old_Entry)
      loop
         Next (It);
      end loop;

      if Off_Right (It) then
         raise GCS.Exceptions.Replace_Target_Not_Found;
      else
         Replace (It, Table_Entry (New_Entry));
      end if;

   end Replace;


   procedure Search (T      : access Linear_Symbol_Table;
                     Name   : in     String;
                     E      : out    Table_Entry;
                     Result : out    Search_Result) is
      use List_Of_Entries;
      It : Iterator;
   begin
      It := Get_Start (T.Store);
      E := null;
      Result := Not_Found;
      while not Off_Right (It) loop
         if GCS.Entities.Entries.Name (Current (It)) = Name then
            if E = null then
               E := Current (It);
               Result := Found;
            else
               Result := Not_Unique;
               return;
            end if;
         end if;
         Next (It);
      end loop;

--       if Result = Not_Found and T.Parent /= null then
--          Search (T.Parent, Name, E, Result);
--       end if;
   end Search;

   function First  (T      : access Linear_Symbol_Table;
                    Name   : in     String)
                    return Table_Entry
   is
      use List_Of_Entries;
   begin
      T.Trying_Table := null;
      T.Tried_Parent := False;
      T.Pos := Get_Start (T.Store);
      while not Off_Right (T.Pos) loop
         if GCS.Entities.Entries.Name (Current (T.Pos)) = Name then
            return Current (T.Pos);
         end if;
         Next (T.Pos);
      end loop;

--       if T.Parent /= null then
--          T.Tried_Parent := True;
--          return First (T.Parent, Name);
--       end if;

      return null;

   end First;

   function Next   (T      : access Linear_Symbol_Table;
                    Name   : in     String)
                    return Table_Entry
   is
      use List_Of_Entries;
   begin
      if T.Trying_Table /= null then
         return Next (T.Trying_Table, Name);
      end if;

--       if Off_Right (T.Pos) then
--          if T.Tried_Parent then
--             return Next (T.Parent, Name);
--          end if;
--       end if;

      if Off_Right (T.Pos) then
         return null;
      end if;

      Next (T.Pos);
      while not Off_Right (T.Pos) loop
         if GCS.Entities.Entries.Name (Current (T.Pos)) = Name then
            return Current (T.Pos);
         end if;
         Next (T.Pos);
      end loop;

      return null;
   end Next;

   function First (T : access Linear_Symbol_Table)
                   return Table_Entry
   is
      use List_Of_Entries;
   begin
      T.Pos := Get_Start (T.Store);
      if not Off_Right (T.Pos) then
         return Current (T.Pos);
      else
         return null;
      end if;
   end First;

   function Next  (T : access Linear_Symbol_Table)
                   return Table_Entry is
      use List_Of_Entries;
   begin
      Next (T.Pos);
      if not Off_Right (T.Pos) then
         return Current (T.Pos);
      else
         return null;
      end if;
   end Next;

   function New_Table (Name   : in String;
                       Parent : in Table       := null;
                       Size   : in Natural     := Default_Table_Size;
                       Owner  : in Table_Entry := null) return Table is
      T : Table_Access;
   begin
      T := new Linear_Symbol_Table;
      New_Table (T, Name, Parent, Owner);
      return Table (T);
   end New_Table;

   procedure Dump (T : access Linear_Symbol_Table) is
      use List_Of_Entries;
      It : Iterator;
   begin
      WL.Trace.Put_Line ("Dumping linear table");
      It := Get_Start (T.Store);
      for I in 1 .. Length (T.Store) loop
         WL.Trace.Put_Line ("    " & Integer'Image (I) &
                            ": " & GCS.Entities.Entries.Name (Current (It)));
         Next (It);
      end loop;

      if T.Parent /= null then
         WL.Trace.Put_Line ("Dumping parent");
         Dump (T.Parent);
      end if;
   end Dump;

end GCS.Entities.Tables.Linear;
