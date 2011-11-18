------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--              G C S . S Y M B O L _ T A B L E . H A S H E D               --
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

package body GCS.Symbol_Table.Hashed is

   type Table_Access is access all Hashed_Symbol_Table'Class;

   function Get_Name (T : Table_Entry) return String is
   begin
      return Name (T);
   end Get_Name;

   procedure Enter (T : access Hashed_Symbol_Table;
                    E : access Root_Table_Entry'Class) is
   begin
      Simple_Hash_Table.Insert (T.Store, Table_Entry (E));
      Set_Table (E, T);
   end Enter;


   procedure Search (T      : access Hashed_Symbol_Table;
                     Name   : in     String;
                     E      : out    Table_Entry;
                     Result : out    Search_Result) is
      use type Simple_Hash_Table.Position;
      Pos : Simple_Hash_Table.Position;
   begin
      Pos := Simple_Hash_Table.First (T.Store, Name);
      if Simple_Hash_Table.Not_Found (Pos) then
         --  I'm just taking out the bits that search the parent ...
         --  I think it might be better to leave that under the
         --  control of the application.

--          if T.Parent /= null then
--             Search (T.Parent, Name, E, Result);
--          else
            Result := Not_Found;
--          end if;
      else
         E := Simple_Hash_Table.Contents (Pos);
         Pos := Simple_Hash_Table.Next (Pos, Name);
         if Simple_Hash_Table.Found (Pos) then
            Result := Not_Unique;
         else
            Result := Found;
         end if;
      end if;
   end Search;

   function First  (T      : access Hashed_Symbol_Table;
                    Name   : in     String)
                    return Table_Entry
   is
      use type Simple_Hash_Table.Position;
   begin
      T.Trying_Table := null;
      T.Pos := Simple_Hash_Table.First (T.Store, Name);
      if Simple_Hash_Table.Not_Found (T.Pos) then
         return null;
      else
         return Simple_Hash_Table.Contents (T.Pos);
      end if;
   end First;

   --  next doesn't do table parents very well ...
   --  (but that's OK, because we're probably not looking
   --  at parent tables anyway).
   function Next   (T      : access Hashed_Symbol_Table;
                    Name   : in     String)
                    return Table_Entry
   is
      use type Simple_Hash_Table.Position;
   begin
      if T.Trying_Table /= null then
         return Next (T.Trying_Table, Name);
      end if;

      T.Pos := Simple_Hash_Table.Next (T.Pos, Name);
      if Simple_Hash_Table.Not_Found (T.Pos) then
         return null;
      else
         return Simple_Hash_Table.Contents (T.Pos);
      end if;
   end Next;

   function First (T : access Hashed_Symbol_Table)
                   return Table_Entry is
      use Simple_Hash_Table;
   begin
      T.Pos := First (T.Store);
      if Simple_Hash_Table.Not_Found (T.Pos) then
         return null;
      else
         return Contents (T.Pos);
      end if;
   end First;

   function Next  (T : access Hashed_Symbol_Table)
                   return Table_Entry is
      use Simple_Hash_Table;
   begin
      T.Pos := Next (T.Pos);
      if Simple_Hash_Table.Not_Found (T.Pos) then
         return null;
      else
         return Contents (T.Pos);
      end if;
   end Next;

   function New_Table (Name   : in String;
                       Parent : in Table       := null;
                       Size   : in Natural     := 0;
                       Owner  : in Table_Entry := null) return Table is
      Hashed_Table : Hashed_Symbol_Table;
      T : Table_Access;
   begin
      T := new Hashed_Symbol_Table;
      if Size /= 0 then
         Simple_Hash_Table.Set_Size (T.Store, Size);
      end if;
      New_Table (T, Name, Parent, Owner);
      return Table (T);
   end New_Table;

   procedure Dump (T : access Hashed_Symbol_Table) is
      use Simple_Hash_Table;
      Pos : Position;
      Count : Positive := 1;
   begin
      WL.Trace.Put_Line ("Dumping hashed table");
      Pos := First (T.Store);
      while Simple_Hash_Table.Found (Pos) loop
         WL.Trace.Put_Line ("    " & Integer'Image (Count) &
                            ": " & Name (Contents (Pos)));
         Count := Count + 1;
         Pos := Next (Pos);
      end loop;

      if T.Parent /= null then
         WL.Trace.Put_Line ("Dumping parent");
         Dump (T.Parent);
      end if;
   end Dump;

end GCS.Symbol_Table.Hashed;
