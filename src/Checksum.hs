{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module Main where

import Prelude hiding (take, interact, readFile, writeFile, putStrLn)
import Control.Applicative
import Control.Monad (guard)
import qualified Options.Applicative as O
import System.Exit
import Data.Word
import Data.Bits (complement)
import Data.Monoid
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (interact, readFile)
import Data.ByteString.Lazy.Char8 (writeFile, putStrLn, pack)
import Data.ByteString.Builder

data Options = Check FilePath
    | Fix
    { output :: Maybe FilePath
    , input :: FilePath
    } deriving Show

options :: O.Parser Options
options = Check <$> O.strOption ( O.long "check" 
                                <> O.short 'c' 
                                <> O.help "Instead of fixing, just check a file"
                                <> O.metavar "INPUT"
                                )
       <|> Fix <$> optional ( O.strOption ( O.long "out" 
                                            <> O.short 'o' 
                                            <> O.help "The output file, stdout if not provided"
                                            <> O.metavar "OUTPUT"))
               <*> O.argument O.str (O.metavar "INPUT")
    
newtype Record = Record {unRecord :: Word8} deriving Show

data IHexLine = IHexLine
    { address    :: Word16
    , recordType :: Record
    , bytes      :: [Word8]
    } deriving Show

split16 :: Word16 -> (Word8, Word8)
split16 w = (fromIntegral w, fromIntegral $ byteSwap16 w)

checksum :: IHexLine -> Word8
checksum IHexLine{bytes, recordType, address}
    = 1 + complement ( fromIntegral (length bytes)
                       + uncurry (+) (split16 address)
                       + unRecord recordType
                       + sum bytes
                       )



encode = toLazyByteString . renderIHex
decode check = parseOnly (parseIHex check)

renderIHex :: [IHexLine] -> Builder
renderIHex = mconcat . map (\x -> renderIHexLine x <> char7 '\n')

renderIHexLine :: IHexLine -> Builder
renderIHexLine ihex@IHexLine{bytes, address, recordType}
    = char7 ':'
      <> word8HexFixed byteCount
      <> word16HexFixed address
      <> word8HexFixed (unRecord recordType)
      <> mconcat (map word8HexFixed bytes)
      <> word8HexFixed (checksum ihex)
  where
    byteCount = fromIntegral $ length bytes


parseIHex :: Bool -> Parser [IHexLine]
parseIHex check = sepBy (parseIHexLine check) endOfLine <* skipSpace <* endOfInput

parseIHexLine :: Bool -> Parser IHexLine
parseIHexLine check = do
    char ':'
    byteCount <- fromIntegral <$> parseWord8
    ihex <- IHexLine <$> parseWord16
                     <*> (Record <$> parseWord8)
                     <*> count byteCount parseWord8
    cs <- parseWord8

    guard $ not check || checksum ihex == cs
    return ihex
    

parseWord8 :: Parser Word8
parseWord8 = do
    bs <- take 2
    case parseOnly (hexadecimal <* endOfInput) bs of
        Right i -> return i
        _ -> fail "invalid character"

    
parseWord16 :: Parser Word16
parseWord16 = do
    bs <- take 4
    case parseOnly (hexadecimal <* endOfInput) bs of
        Right i -> return i
        _ -> fail "invalid character"

main = do
    os <- O.execParser opts
    ihex <- getIHex os
    case os of
        Check _ -> putStrLn "File OK" >> exitSuccess
        Fix o _ -> let ihexEnc = encode ihex
                   in maybe (putStrLn ihexEnc) (`writeFile` ihexEnc) o
  where
    opts = O.info (O.helper <*> options) O.fullDesc
    getIHex (Check i) = decodeWithExit True i
    getIHex (Fix _ i) = decodeWithExit False i
    decodeWithExit check i = do
        bs <- readFile i
        let ehex = decode check bs
        either (const $ putStrLn "Corrupt file" >> exitFailure)
               return
               ehex