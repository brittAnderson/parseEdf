{-# LANGUAGE OverloadedStrings, TemplateHaskell ,FlexibleInstances #-}

module TrialJSON (
  module TrialJSON
  ) where

import qualified Data.Aeson as DA
import qualified Data.ByteString.Lazy as DBL
import Data.List.Utils (replace)
import Data.List.Split (splitOn)
import EDFParse
import Control.Lens.Extras (is)
import GHC.Exts (fromList,toList)
import Data.Either (rights)
import Text.Read (readEither)
import qualified Data.Text.Lazy.IO as I (writeFile)
import qualified Data.Aeson.Text as DAT (encodeToLazyText)
import System.FilePath
import System.Environment
import System.IO

instance DA.ToJSON EDF where
  toJSON (Trial t tn)  = DA.object ["trialTime" DA..= t, "trialNumber" DA..= tn]
  toJSON (FixOn t)     = DA.object ["fixSpotOnTime" DA..= t]
  toJSON (FixReturn t) = DA.object ["returnFixTime" DA..= t]
  toJSON (RewStart t)  = DA.object ["rewStartTime" DA..= t]
  toJSON (RewEnd t)    = DA.object ["rewEndTime" DA..= t]
  toJSON (Cue t)       = DA.object ["cueTime" DA..= t]
  toJSON (Stimulus t)  = DA.object ["stimTime" DA..= t]
  toJSON (RespMade t)  = DA.object ["respTime" DA..= t]
  -- toJSON (Fix t fid)   = DA.object ["fixTime" DA..= t, "fixID" DA..= fid]
  toJSON (EDFL e)
    | (is _Esacc (e!!0)) = DA.object [
        "sacEyeL" DA..= map eye e
        , "sacStartT" DA..= map start e
        , "sacStopT"  DA..= map stop e
        , "sacDur"    DA..= map dur e
        , "sacStartX" DA..= map xs e
        , "sacStartY" DA..= map ys e
        , "sacEndX"   DA..= map xe e
        , "sacEndY"   DA..= map ye e
        , "sacAmp"    DA..= map amp e
        , "sacPeakVel"DA..= map pv e
        ]
    | (is _Efix (e!!0)) = DA.object [
        "fixEyeL" DA..= map eye e
        , "fixStartT" DA..= map start e
        , "fixStopT"  DA..= map stop e
        , "fixDur"    DA..= map dur e
        , "fixAvgX"   DA..= map xavg e
        , "fixAvgY"   DA..= map yavg e
        , "fixAvgPupil" DA..= map pupilAvg e
        ]
    | otherwise = DA.object ["mistake" DA..= True]

parsedFileName :: FilePath -> String -> FilePath
parsedFileName fp ext =
  addExtension (fst . splitExtension $ fp) ext

cnvrtAsc2ParsedDat ifp ofp = do
  fc <- readFile ifp
  pfh <- openFile ofp WriteMode
  let myparse = parseEdf fc
  mapM_ ((hPutStr pfh) . edfShow) myparse
  hClose pfh

getParsedDatFile fp = do
  d <- readFile  fp
  return $ filter (\l -> (length l) > 1) $ lines  d

strings2DAs :: [String] -> [[DA.Value]]
strings2DAs di = map f trre
  where
    f inpt = map DA.toJSON (nonem $ inpt) ++ (map DA.toJSON (combSsFs' (get_saccs' inpt) (get_fixes' inpt)))
    nonem edfitem = [x | x <- edfitem, not (is _Efix x), not (is _Esacc x)]
    newtemp = map (splitOn "|") di
    trre = map (rights . (map Text.Read.readEither)) newtemp :: [[EDF]]

concatDAs inlist =
  DA.object $ Prelude.map (\(x,y) -> x DA..= y) $ concat $ Prelude.map (\(DA.Object x) -> GHC.Exts.toList x) $ inlist

edfViaAsc2Json fp = do
  let ofn = makeFN fp
  putStrLn $"Converting " ++ fp
  cnvrtAsc2ParsedDat fp ofn
  putStrLn $ "Reading in " ++ ofn ++ " for conversion to json"
  d <- getParsedDatFile ofn
  let davs = map concatDAs $ strings2DAs d
  I.writeFile (parsedFileName ofn ".json") $ DAT.encodeToLazyText $ davs
  

makeFN fp = 
  let p_and_n@(fpath,fname) = splitFileName fp
      fppd = parsedFileName fname ".parsedDat"
      outfile = fpath ++ "parseDat/" ++ fppd
  in outfile
  
-- filter (\x -> isInfixOf ".asc" x)  dirL
-- dirL <- System.Directory.getDirectoryContents ".."
