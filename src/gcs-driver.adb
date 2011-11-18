------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--                           G C S . D R I V E R                            --
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

--  Small file to test that GCS compiles.  We do this by simply
--  with'ing the lot, which will force gnatmake to recompile
--  everything (partly because the build script removes all
--  intermediate files for us).

pragma Warnings (Off);

with GCS_Test;
with GCS.AST;
with GCS.Constraints;
with GCS.Errors;
with GCS.Exceptions;
with GCS.Lexer;
with GCS.Styles;
with GCS.Symbol_Table.Hashed;
with GCS.Symbol_Table.Linear;
with GCS.Symbol_Table.Trees;
with GCS.Symbol_Table;
with GCS.Token_Parser;
with GCS.Version;
with GCS;

with GCS.Entities;
with GCS.Entities.Entries;
with GCS.Entities.Trees;
with GCS.Entities.Tables;
with GCS.Entities.Tables.Linear;
with GCS.Entities.Tables.Hashed;
with GCS.Entities.Searches;
with GCS.Ada_Tokens;
with GCS.Ada_Lexical;

procedure GCS.Driver is

begin
   null;
end GCS.Driver;


