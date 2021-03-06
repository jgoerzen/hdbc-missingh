{- arch-tag: AnyDBM tests main file
Copyright (C) 2004-2005 John Goerzen <jgoerzen@complete.org>

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

module AnyDBMtest(mf, generic_persist_test, generic_test, tests) where
import Test.HUnit
import MissingH.List
import MissingH.IO.HVFS
import MissingH.IO.HVFS.InstanceHelpers
import MissingH.AnyDBM
import MissingH.AnyDBM.StringDBM
import MissingH.AnyDBM.FiniteMapDBM
import MissingH.AnyDBM.MapDBM
import MissingH.AnyDBM.HDBCDBM
import Database.HDBC.Sqlite3
import Database.HDBC.PostgreSQL
import System.Directory
import MissingH.IO.HVFS.Utils
import MissingH.Path.FilePath
import Data.HashTable
import Data.List(sort)
import Control.Exception(finally)

mf :: AnyDBM a => IO b -> (b -> IO a) -> String -> (a -> Assertion) -> Test
mf initfunc openfunc msg code =
    TestLabel msg $ TestCase $ do i <- initfunc
                                  h <- openfunc i
                                  finally (code h) (closeA h)
        
infix 1 @>=?
(@>=?) :: (Eq a, Show a) => a -> IO a -> Assertion
(@>=?) exp res = do r <- res
                    exp @=? r

deleteall h = do k <- keysA h
                 mapM_ (deleteA h) k
                 [] @>=? keysA h

weirdl = sort $ [("", "empty"), 
                 ("foo\nbar", "v1\0v2"),
                 ("v3\x01v4", ""),
                 ("k\0ey", "\xFF")]

weirdl2 = sort $ [("", "empty"),
                  ("foo\nbar", "v1\x01v2"),
                  ("v3\x02v4", ""),
                  ("key", "\xFF")]

createdir = TestCase $ createDirectory "testtmp"
removedir = TestCase $ recursiveRemove SystemFS "testtmp"

generic_test wl initfunc openfunc =
    let f = mf initfunc openfunc in
        [
         createdir
        ,f "empty" $ \h -> do [] @>=? keysA h
                              [] @>=? valuesA h
                              [] @>=? toListA h
                              Nothing @>=? lookupA h "foo"
                     
        ,f "basic" $ \h -> do insertA h "key" "value"
                              (Just "value") @>=? lookupA h "key"
                              [("key", "value")] @>=? toListA h
                              insertA h "key" "v2"
                              [("key", "v2")] @>=? toListA h
                              deleteA h "key"
                              [] @>=? toListA h
        ,f "mult" $ \h -> do insertListA h [("1", "2"), ("3", "4"), ("5", "6")]
                             [("1", "2"), ("3", "4"), ("5", "6")] @>=? 
                                (toListA h >>= return . sort)
                             ["1", "3", "5"] @>=? (keysA h >>= return . sort)
                             ["2", "4", "6"] @>=? (valuesA h >>= return . sort)
                             deleteall h
        ,f "weirdchars" $ \h -> do insertListA h wl
                                   wl @>=? (toListA h >>= return . sort)
                                   deleteall h
        ,removedir
        ]

generic_persist_test wl initfunc openfunc  =
    let f = mf initfunc openfunc in
        [
         createdir
        ,f "empty" deleteall 
        ,f "weirdpop" $ \h -> insertListA h wl
        ,f "weirdcheck" $ \h -> do wl @>=? (toListA h >>= return . sort)
                                   deleteall h
                                   insertA h "key" "value"
        ,f "step3" $ \h -> do [("key", "value")] @>=? (toListA h >>= return . sort)
                              insertA h "key" "v2"
                              insertA h "z" "y"
        ,f "step4" $ \h -> do [("key", "v2"), ("z", "y")] @>=?
                                 (toListA h >>= return . sort)
        ,f "cleanupdb" deleteall
        ,removedir
        ]

test_hashtable = generic_test weirdl (return ())
                  (\_ -> ((new (==) hashString)::IO (HashTable String String)))

test_finitemap = generic_test weirdl (return ())
                  (\_ -> newFiniteMapDBM)
test_mapdbm = generic_test weirdl (return ())
                  (\_ -> newMapDBM)
test_stringdbm = generic_persist_test weirdl (return SystemFS)
                   (\f -> openStringVDBM f (joinPaths "testtmp" "StringDBM") ReadWriteMode)
                 ++
                 generic_test weirdl (return SystemFS)
                   (\f -> openStringVDBM f (joinPaths "testtmp" "StringDBM") ReadWriteMode)

test_hdbcdbm cf = generic_persist_test weirdl2 (cf)
                  (\f -> openSimpleHDBCDBM "hdbcdbmtest" f)
               ++
               generic_test weirdl2 (cf)
                  (\f -> openSimpleHDBCDBM "hdbcdbmtest" f)
    where connsql3 = connectSqlite3 (joinPaths "testtmp" "HDBCDBM")

tests = TestList [TestLabel "HashTable" (TestList test_hashtable),
                  TestLabel "StringDBM" (TestList test_stringdbm),
                  TestLabel "FiniteMap" (TestList test_finitemap),
                  TestLabel "MapDBM" (TestList test_mapdbm),
                  TestLabel "HDBCDBM-sqllite3" 
                                (TestList (test_hdbcdbm (connectSqlite3 (joinPaths "testtmp" "HDBCDBM")))),
                  TestLabel "HDBCDBM-psql" (TestList (test_hdbcdbm $ connectPostgreSQL ""))
                 ]
