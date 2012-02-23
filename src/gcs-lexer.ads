------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--                            G C S . L E X E R                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.3 $                              --
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

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Bounded.Hash;

with WL.Sets.Bounded;

with GCS.Constraints;
with GCS.Positions;
with GCS.Styles;
with GCS.Token_Parser;
pragma Elaborate_All (GCS.Token_Parser);

generic
   type Token is (<>);

   Tok_None            : Token;
   Tok_End_Of_File     : Token;
   Tok_Bad_Character   : Token;
   Tok_Identifier      : Token;
   Tok_String          : Token;
   Tok_Character       : Token;
   Tok_Integer         : Token;
   Tok_Float           : Token;

   First_Keyword       : Token;
   Keywords            : String;      -- space-separated, Token order

   First_Symbol        : Token;
   Symbols             : String;      -- space-separated, Token order

   Identifier_Start    : String;
   Identifier_Body     : String;

   Identifier_Group    : String := "";  --  characters that form a
                                        --  distinct set of identifiers

   Line_Comment_Start  : String;

   Block_Comment_Start : String := "";
   Block_Comment_End   : String := "";

   Escape_Character    : Character := Character'First;

   Properties          : GCS.Styles.Property_List;
   Hooks               : GCS.Styles.Hook_Function_List :=
     GCS.Styles.Default_Hook_Function_List;

package GCS.Lexer is

   pragma Elaborate_Body;

   Max_Lookahead : constant := 4;
   type Lookahead is mod Max_Lookahead;

   procedure Open (Name : String);
   procedure Open_Standard_Input;
   procedure Open_String (Text : String);

   procedure Close;

   function Current_Hash return File_Hash;

   function Tok return Token;
   function Tok_Text return String;
   function Tok_Raw_Text return String;
   function Tok_Character_Value return Character;

   function Prev_Tok return Token;
   function Next_Tok (Ahead : Lookahead := 1) return Token;

   function Tok_Text (Ahead : Lookahead) return String;

   function Tok_File return GCS.Constraints.Source_File_Type;
   function Tok_File_Name return String;
   function Tok_Line return GCS.Constraints.Line_Number;
   function Tok_Column return GCS.Constraints.Column_Count;
   function Tok_Indent return GCS.Constraints.Column_Count;

   function Current_Line return String;

   procedure Scan;

   procedure Warning        (Message : String);
   procedure Error          (Message : String);
   procedure Fatal_Error    (Message : String);
   procedure Internal_Error (Message : String);

   package Set_Of_Tokens is new WL.Sets.Bounded (Token);

   procedure Expect (T          : Token;
                     Skip_Up_To : Token);

   procedure Expect (T          : Token;
                     Skip_Up_To : Set_Of_Tokens.Element_List);

   procedure Skip_To (Skip_Up_To : Token);
   procedure Skip_To (Tok_1, Tok_2 : Token);
   procedure Skip_To (Tok_1, Tok_2, Tok_3 : Token);
   procedure Skip_To (Tok_1, Tok_2, Tok_3, Tok_4 : Token);

   procedure Skip_To (Skip_Up_To : Set_Of_Tokens.Element_List);

   procedure Skip_To (Skip_To_And_Parse : Set_Of_Tokens.Element_List;
                      Skip_To_And_Stop  : Set_Of_Tokens.Element_List);

   type Trap_Handler is access procedure (Line_No   : Positive;
                                          Line_Text : String);

   procedure Set_New_Line_Trap (Handler : Trap_Handler);

   --  Get_Current_Position: returns the current position of the parse
   --  (file, line, column, indent).
   function Get_Current_Position return GCS.Positions.File_Position;

   --  Overloads so that errors can be issued for different places
   --  in the source files.
   procedure Warning        (Location   : in GCS.Positions.File_Position;
                             Message    : in String);

   procedure Error          (Location   : in GCS.Positions.File_Position;
                             Message    : in String);

   procedure Fatal_Error    (Location   : in GCS.Positions.File_Position;
                             Message    : in String);

   procedure Internal_Error (Location   : in GCS.Positions.File_Position;
                             Message    : in String);


   procedure Debug (Enabled : Boolean);

private

   package Tokeniser is new GCS.Token_Parser;

   ------------------------------------------------------------------
   --  Keyword storage

   package Keyword_Strings is
     new Ada.Strings.Bounded.Generic_Bounded_Length (24);

   function Hash is
     new Ada.Strings.Bounded.Hash (Keyword_Strings);

   package Keyword_Table is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Keyword_Strings.Bounded_String,
        Element_Type    => Token,
        Hash            => Hash,
        Equivalent_Keys => Keyword_Strings."=");

end GCS.Lexer;
