------------------------------------------------------------------------------
--                                                                          --
--                         GENERIC COMPILER SYSTEM                          --
--                                                                          --
--                             G C S _ T E S T                              --
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

with GCS.Lexer;
with GCS.Styles;

package GCS_Test is

   type Token is
      (Tok_None, Tok_End_Of_File, Tok_Bad_Character,
       Tok_Identifier, Tok_String, Tok_Character, Tok_Integer, Tok_Float,
       Tok_Demon, Tok_Demons, Tok_Comment, Tok_Test, Tok_Act,
       Tok_Pact, Tok_Mact, Tok_Word, Tok_Variable,
       Tok_If, Tok_Then, Tok_Else, Tok_CD, Tok_Function,
       Tok_Left_Paren, Tok_Right_Paren,
       Tok_Equal, Tok_Not_Equal, Tok_Asterisk,
       Tok_Becomes, Tok_Dform, Tok_Arrow, Tok_And, Tok_Or, Tok_Not,
       Tok_Colon, Tok_Lambda
       );

   package Test_Lex is
      new GCS.Lexer (Token              => Token,
                     Tok_None           => Tok_None,
                     Tok_End_Of_File    => Tok_End_Of_File,
                     Tok_Bad_Character  => Tok_Bad_Character,
                     Tok_Identifier     => Tok_Identifier,
                     Tok_String         => Tok_String,
                     Tok_Character      => Tok_Character,
                     Tok_Integer        => Tok_Integer,
                     Tok_Float          => Tok_Float,
                     First_Keyword      => Tok_Demon,
                     Keywords           => "demon demons comment test act " &
                                           "+act -act word variable " &
                                           "if then else cd function",
                     First_Symbol       => Tok_Left_Paren,
                     Symbols            => "( ) = /= * := <- => & | ~ : \",
                     Identifier_Start   => "abcdefghijklmnopqrstuvwxyz" &
                                           "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
                                           "!@@#$%^_-+?'.",
                     Identifier_Body    => "abcdefghijklmnopqrstuvwxyz" &
                                          "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
                                          "!@@#$%^_-+?.",
                     Line_Comment_Start => "--",
                     Properties         => GCS.Styles.Ada_Property_List);


end GCS_Test;
