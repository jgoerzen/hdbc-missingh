{- arch-tag: GZip file support in Haskell
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
   Module     : MissingH.FileArchive.GZip
   Copyright  : Copyright (C) 2004 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen,
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

GZip file decompression

Copyright (c) 2004 John Goerzen, jgoerzen\@complete.org

The GZip format is described in RFC1952.
-}
module MissingH.FileArchive.GZip (
                                  decompress,
                                  read_header,
                                  Header(..), Section, GZipError, Footer(..),
                                  read_section,
                                  read_sections
                                 )
where

import MissingH.Compression.Inflate
import MissingH.Checksum.CRC32.GZip
import Data.List
import Data.Bits
import Control.Monad.Error
import Data.Char
import Data.Word
import MissingH.Bits

type GZipError = String

-- | First two bytes of file
magic = "\x1f\x8b"

-- | Flags
fFTEXT = 1::Int
fFHCRC = 2::Int
fFEXTRA = 4::Int
fFNAME = 8::Int
fFCOMMENT = 16::Int

data Header = Header {
                      method :: Int,
                      flags :: Int,
                      extra :: Maybe String,
                      filename :: Maybe String,
                      comment :: Maybe String,
                      mtime :: Word32,
                      xfl :: Int,
                      os :: Int
                     } deriving (Eq, Show)

data Footer = Footer {
                      size :: Word32,
                      crc32 :: Word32,
                      crc32valid :: Bool}

type Section = (Header, String, Footer)

split1 :: String -> (Char, String)
split1 s = (head s, tail s)

{- | Read a GZip file.
-}

decompress :: String -> Either GZipError (String, Bool)
{-
decompress s = 
    do x <- read_header s
       let rem = snd x
       return $ inflate_string rem
-}

decompress s = 
    let procs :: [Section] -> (String, Bool)
        procs [] = ([], True)
        procs ((_, content, foot):xs) = 
            let (nexth, nextb) = procs xs in
                (content ++ nexth, (crc32valid foot) && nextb)
        in
        do x <- read_sections s
           return $ procs x

{-
decompress s = do x <- read_sections s
                  return $ concatMap (\(_, x, _) -> x) x
-}

-- | Read all sections.  
read_sections :: String -> Either GZipError [Section]
read_sections [] = Right []
read_sections s =
    do x <- read_section s
       case x of
           (sect, remain) ->
               do next <- read_sections remain
                  return $ sect : next

parseword :: String -> Word32
parseword s = fromBytes $ map (fromIntegral . ord) $ reverse s

-- | Read one section, returning (ThisSection, Remainder)
read_section :: String -> Either GZipError (Section, String)
read_section s =
        do x <- read_header s
           let headerrem = snd x
           let (decompressed, crc32, remainder) = read_data headerrem
           let (crc32str, rem) = splitAt 4 remainder
           let (sizestr, rem2) = splitAt 4 rem
           let filecrc32 = parseword crc32str
           let filesize = parseword sizestr
           return ((fst x, decompressed,
                   Footer {size = filesize, crc32 = filecrc32,
                           crc32valid = filecrc32 == crc32})
                   ,rem2)

-- | Read the file's compressed data, returning
-- (Decompressed, Calculated CRC32, Remainder)
read_data :: String -> (String, Word32, String)
read_data x = 
    let (decompressed1, remainder) = inflate_string_remainder x
        (decompressed, crc32) = read_data_internal decompressed1 0
        in
        (decompressed, crc32, remainder)
    where
    read_data_internal [] ck = ([], ck)
    read_data_internal (x:xs) ck =
        let n = read_data_internal xs (update_crc ck x)
            in
            (x : fst n, snd n)
    


{- | Read the GZip header.  Return (Header, Remainder).
-}
read_header :: String -> Either GZipError (Header, String)
read_header s =
    let ok = Right "ok" in
    do let (mag, rem) = splitAt 2 s
       if mag /= magic
          then throwError "Not a GZip file"
          else ok
       let (method, rem2) = split1 rem
       if (ord(method) /= 8)
          then throwError "Unknown compression method"
          else ok
       let (flag_S, rem3) = split1 rem2
       let flag = ord flag_S
       let (mtimea, rem3a) = splitAt 4 rem3
       let mtime = parseword mtimea
       let (xfla, rem3b) = split1 rem3a
       let xfl = ord xfla
       let (osa, rem3c) = split1 rem3b
       let os = ord osa
       -- skip modtime (4), extraflag (1), and os (1)
       let rem4 = drop 6 rem3
       
       let (extra, rem5) = 
               if (flag .&. fFEXTRA /= 0)
               -- Skip past the extra field if we have it.
                  then let (xlen_S, rem4a) = split1 rem4
                           (xlen2_S, rem4b) = split1 rem4
                           xlen = (ord xlen_S) + 256 * (ord xlen2_S)
                           (ex, rrem) = splitAt xlen rem4b
                           in (Just ex, rrem)
                  else (Nothing, rem4)
       
       let (filename, rem6) = 
               if (flag .&. fFNAME /= 0)
               -- Skip past the null-terminated filename
                  then let fn = takeWhile (/= '\x00') rem5
                                in (Just fn, drop ((length fn) + 1) rem5)
                  else (Nothing, rem5)

       let (comment, rem7) =
               if (flag .&. fFCOMMENT /= 0)
                  -- Skip past the null-terminated comment
                  then let cm = takeWhile (/= '\x00') rem6
                           in (Just cm, drop ((length cm) + 1) rem6)
                  else (Nothing, rem6)
       
       rem8 <- if (flag .&. fFHCRC /= 0)
                  -- Skip past the header CRC
                  then return $ drop 2 rem7
                  else return rem7
                  
       return (Header {method = ord method,
                      flags = flag,
                      extra = extra,
                      filename = filename,
                      comment = comment,
                      mtime = mtime,
                      xfl = xfl,
                      os = os}, rem8)
