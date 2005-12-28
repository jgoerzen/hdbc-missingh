{-
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

-}

{- |
   Module     : MissingH.AnyDBM.HDBCDBM
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU LGPL, version 2.1 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

HDBC backend for DBM databases

Written by John Goerzen, jgoerzen\@complete.org
-}

module MissingH.AnyDBM.HDBCDBM where

import MissingH.AnyDBM
import Database.HDBC
import System.IO(IOMode)

data HDBCDBM = HDBCDBM {conn :: Connection,
                        tablename :: String,
                        keycolname :: String,
                        valcolname :: String}

{- | Opens a DBM connection to the specified table and database.

If the given table does not already exist, it will be created for you
automatically.

The column names @dbmkey@ and @dbmval@ will be used.

The 'Database.HDBC.clone' method will be used to obtain a new handle to 
the database for internal use.  That will help prevent conflicts with things
like commits from occuring with the main application handle.  You should not
have to worry about side-effects to your main application as a result of
using this function.

If you need more control, see 'openHDBCDBM' -}
openSimpleHDBCDBM :: String -> Connection -> IO HDBCDBM
openSimpleHDBCDBM itablename iconn =
    do mydbh <- clone iconn
       tablelist <- getTables mydbh
       when (not ((map toLower) itablename `elem` tablelist))
            (withTransaction mydbh (createtable (map toLower tablename)))
       return $ HDBCDBM {connection = mydbh,
                         tablename = (map toLower) itablename,
                         keycolname = "dbmkey",
                         valcolname = "dbmval"}
    where createtable tablename dbh =
              run dbh $ "CREATE TABLE " ++ tablename ++ 
                        "(dbmkey text NOT NULL PRIMARY KEY, " ++
                        "dbmval text NOT NULL)"

{- | Opens a DBM connection to the specified table, column name for key,
column name for value, and database.

It is an error if the given table does not exist, lacks the specified columns,
or has an incorrect type definition for the specified columns.  However,
it is not guaranteed that this error will be detected at object creation time.

This function does not create a table for you if it doesn't exist.

You can use the table created and used by 'openSimpleHDBCDBM' like this:

>openHDBCDBM "tablename" "dbmkey" "dbmval" conn

Incidentally, such a table could be created with:

>CREATE TABLE tablename (dbmkey text NOT NULL PRIMARY KEY, dmval text NOT NULL)

The 'Database.HDBC.clone' method will be used to obtain a new handle to 
the database for internal use.  That will help prevent conflicts with things
like commits from occuring with the main application handle.  You should not
have to worry about side-effects to your main application as a result of
using this function. -}
openHDBCDBM :: String           -- ^ Name of table to use
            -> String           -- ^ Name of column with keys
            -> String           -- ^ Name of column with values
            -> Connection       -- ^ HDBC connection object to clone
            -> IO HDBCDBM       -- ^ Returned DBM object
openHDBCDBM itablename ikeyname ivalname iconn =
    do mydbh <- clone iconn
       tablelist <- getTables mydbh
       let mytablename = map toLower itablename
       when (not (mytablename `elem` tablelist))
            (fail $ "Table " ++ mytablename ++ " not in database.")
       return $ HDBCDBM {connection = mydbh,
                         tablename = mytablename,
                         keycolname = ikeyname,
                         valcolname = ivalname}

basequery dbm = " FROM " ++ (tablename dbm) ++
               "WHERE " ++ (keycolname dbm) ++ " = ?"

querykey :: HDBCDBM -> String
querykey dbm = "SELECT " ++ (keycolname dbm) ++ ", " ++
               (valcolname dbm) ++ basequery dbm

deletequery :: HDBCDBM -> String
deletequery dbm = "DELETE " ++ basequery dbm

insertquery dbm = "INSERT INTO " ++ (tablename dbm) ++
                  " (" ++ (keycolname dbm) ++ ", " ++ (valcolname dbm) ++ 
                  ") VALUES (?, ?)"

updatequery dbm = "UPDATE " ++ (tablename dbm) ++ " SET " ++ 
                  (valcolname dbm) ++ " = ? WHERE " ++ (keycolname dbm) ++
                  " = ?"
        
instance HDBCDBM AnyDBM where
    closeA dbm = do commit (conn dbm)
                    disconnect (conn dbm)

    flushA dbm = commit (conn dbm)

    insertA dbm key value = withTransaction (conn dbm) $ \dbh ->
            do count <- run dbh updatequery [toSql value, toSql key]
               case count of
                 0 -> -- No change, need to insert it.
                      run dbh insertquery [toSql key, toSql value]
                 1 -> return () -- We tweaked 1 row
                 x -> fail $ "HDBC insertA: unexpected number of rows updated: " ++ show x

    deleteA dbm key = withTransaction (conn dbm) $ \dbh ->
            run dbh deletequery [toSql key]

    lookupA dbm key = 
        do res <- quickQuery (conn dbm) querykey [toSql key]
           case res of
             [] -> return Nothing
             [[_, value]] -> return (Just value)
             x -> fail $ "lookupA: unexpected return value " ++ show x

    toListA dbm =
        do res <- quickQuery (conn dbm) 
                  ("SELECT " ++ keycolname dbm ++ ", " ++ valcolname dbm ++
                   " FROM " ++ tablename dbm) []
           mapM convrow res
        where convrow [k, v] = (k, v)
              convrow x = error $ "toListA: unexpected row " ++ show x

