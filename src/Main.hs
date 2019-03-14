module Main where

import EDFParse
import System.Environment
import System.FilePath
import System.IO
import Data.Scientific
  
parsedFileName :: FilePath -> FilePath
parsedFileName fp =
  addExtension (fst . splitExtension $ fp) ".parsedDat"

main :: IO ()
main = do
  gas <- getArgs 
  fc <- readFile $ head gas
  pfh <- openFile (parsedFileName (head gas)) WriteMode
  let myparse = parseEdf fc
  mapM_ ((hPutStr pfh) . edfShow) myparse
  hClose pfh
  
