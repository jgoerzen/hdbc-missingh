{- arch-tag: MIMETypes tests main file
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

module MIMETypestest(tests) where
import HUnit
import Data.List
import MissingH.MIMETypes

test_readMIMETypes =
    do
    mtd <- readMIMETypes defaultmtd True "testsrc/mime.types.test"
    let f = \strict inp exp -> exp @=? guessType mtd strict inp
    let fe = \strict inp exp -> (sort exp) @=? sort (guessAllExtensions mtd strict inp)
    f True "foo.bar.baz" (Nothing, Nothing)
    f True "" (Nothing, Nothing)
    f True "foo.ez" (Just "application/andrew-inset", Nothing)
    fe True "application/andrew-inset" [".ez"]
    f True "foo.dv" (Just "video/x-dv", Nothing)
    fe True "video/x-dv" [".dif", ".dv"]
    f True "test.h++" (Just "text/x-c++hdr", Nothing)
    fe True "text/x-c++hdr" [".h++", ".hpp", ".hxx", ".hh"]
    f True "foo.tgz" (Just "application/x-tar", Just "gzip")


test_guessAllExtensions =
    let f strict inp exp = (sort exp) @=? sort (guessAllExtensions defaultmtd strict inp) in
        do
        f True "" []
        f True "foo" []
        f True "application/octet-stream" [".obj", ".so", ".bin", ".a", ".dll", ".exe", ".o"]
        f True "text/plain" [".pl", ".ksh", ".bat", ".c", ".h", ".txt"]
        f True "application/rtf" []
        f False "application/rtf" [".rtf"]

test_guessType =
    let f strict inp exp = exp @=? guessType defaultmtd strict inp in 
        do
        f True "" (Nothing, Nothing)
        f True "foo" (Nothing, Nothing)
        f True "foo.txt" (Just "text/plain", Nothing)
        f True "foo.txt.gz" (Just "text/plain", Just "gzip")
        f True "foo.txt.blah" (Nothing, Nothing)
        f True "foo.tar" (Just "application/x-tar", Nothing)
        f True "foo.tar.gz" (Just "application/x-tar", Just "gzip")
        f True "foo.tgz" (Just "application/x-tar", Just "gzip")
        f True "http://foo/test.dir/blah.rtf" (Nothing, Nothing)
        f False "http://foo/test.dir/blah.rtf" (Just "application/rtf", Nothing)
        f True "foo.pict" (Nothing, Nothing)
        f False "foo.pict" (Just "image/pict", Nothing)

tests = TestList [TestLabel "guessType" (TestCase test_guessType),
                  TestLabel "guessAllExtensions" (TestCase test_guessAllExtensions),
                  TestLabel "readMIMETypes" (TestCase test_readMIMETypes)
                 ]
