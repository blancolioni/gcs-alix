------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--               G C S . S Y M B O L _ T A B L E . T R E E S                --
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

package body GCS.Symbol_Table.Trees is

   function Environment (T : access Tree_Node'Class) return Table is
   begin
      return T.Env;
   end Environment;

   function Environment (T : in     Tree_Node'Class) return Table is
   begin
      return T.Env;
   end Environment;

   procedure Set_Environment (T    : access Tree_Node'Class;
                              Env  : in     Table)
   is
   begin
      T.Env := Env;
   end Set_Environment;

   procedure Initialise (T       : access Tree_Node'Class;
                         Env     : in     Table;
                         Parent  : access Tree_Node'Class) is
   begin
      GCS.AST.Initialise (T, GCS.AST.Tree (Parent));
      Set_Environment (T, Env);
   end Initialise;

   procedure Initialise (T       : access Tree_Node'Class;
                         Env     : in     Table) is
   begin
      GCS.AST.Initialise (T);
      Set_Environment (T, Env);
   end Initialise;

end GCS.Symbol_Table.Trees;

