{- arch-tag: HVIO tests main file
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

module HVIOtest(tests) where
import HUnit
import MissingH.IO.HVIO

ioeq :: (Show a, Eq a) => a -> IO a -> Assertion
ioeq exp inp = do x <- inp
                  exp @=? x

test_StreamReader =
    let f inp testfunc = TestLabel inp $ TestCase $ do x <- newStreamReader inp
                                                       testfunc x
        in 
        [
         f "" (\x -> do True `ioeq` vIsEOF x
                        True `ioeq` vIsOpen x
                        vClose x
                        False `ioeq` vIsOpen x
              )
        ]

tests = TestList [TestLabel "streamReader" (TestList test_StreamReader)
                 ]