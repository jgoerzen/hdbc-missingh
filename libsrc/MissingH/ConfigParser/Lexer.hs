{- arch-tag: ConfigParser lexer support
Copyright (C) 2004 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module     : MissingH.ConfigParser.Lexer
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen, 
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Lexer support for "MissingH.ConfigParser".  This module is not intended to be
used directly by your programs.

Copyright (c) 2004 John Goerzen, jgoerzen\@complete.org
-}
module MissingH.ConfigParser.Lexer 
(
       -- -- * Temporary for testing
       --comment_chars, eol, optionsep, whitespace_chars, comment_line,
       --empty_line, sectheader_chars, sectheader, oname_chars, value_chars,
       --extension_line, optionkey, optionvalue, optionpair
       loken,
       CPTok(..)
) where

import Text.ParserCombinators.Parsec
import MissingH.Parsec

data CPTok = EOFTOK
           | NEWSECTION String
           | NEWSECTION_EOF String
           | EXTENSIONLINE String
           | NEWOPTION (String, String)
             deriving (Eq, Show, Ord)

comment_chars = oneOf "#;"
eol = string "\n" <|> string "\r\n" <|> string "\r" <?> "End of line"
optionsep = oneOf ":=" <?> "Option separator"
whitespace_chars = oneOf " \t" <?> "Whitespace"
comment_line = do skipMany whitespace_chars <?> "whitespace in comment"
                  comment_chars             <?> "start of comment"
                  (many1 $ noneOf "\r\n")   <?> "content of comment"
empty_line = many1 whitespace_chars         <?> "empty line"
sectheader_chars = noneOf "]\r\n"
sectheader = do char '['
                sname <- many1 $ sectheader_chars
                char ']'
                return sname
oname_chars = noneOf ":=\r\n"
value_chars = noneOf "\r\n"
extension_line = do
                 many1 whitespace_chars
                 c1 <- noneOf "\r\n#;"
                 remainder <- many value_chars
                 return (c1 : remainder)

optionkey = many1 oname_chars
optionvalue = many1 value_chars
optionpair = do
             key <- optionkey
             optionsep
             value <- optionvalue
             return (key, value)

loken :: Parser [CPTok]
loken =
    -- Ignore these things
    do {eol; loken}                     
    <|> try (do {comment_line; loken})
    <|> try (do {empty_line; loken})
    
    -- Real stuff
    <|> (do {sname <- sectheader; next <- loken; return $ NEWSECTION sname : next})
    <|> try (do {pair <- optionpair; next <- loken; return $ NEWOPTION pair : next})
    <|> (do {extension <- extension_line; next <- loken; return $ EXTENSIONLINE extension : next})
    <|> do {eof; return [EOFTOK]}
    <?> "Invalid syntax in configuration file"
        
