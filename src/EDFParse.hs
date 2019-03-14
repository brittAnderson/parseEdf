{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module EDFParse (
  module EDFParse,
  module Text.ParserCombinators.ReadP
  ) where


import Data.Char as DC
import Data.List as DL
import Text.ParserCombinators.ReadP
import Control.Applicative hiding (many,optional)
import Control.Lens
import Control.Lens.Extras (is)
import Data.Scientific

data EDF = Msg String
         | Trial {time :: Integer, trialNum :: Integer}
         | Cue   {time :: Integer}
         | FixOn   {time :: Integer}
         | FixReturn {time :: Integer}
         | Stimulus {time :: Integer}
         | RespMade {time :: Integer}
         | RewStart {time :: Integer}
         | RewEnd {time :: Integer}
         | Sfix  {eye    :: Char
                 ,start  :: Integer}
         | Ssacc  {eye    :: Char
                 ,start  :: Integer}
         | Sblink {eye    :: Char
                 ,start  :: Integer}
         | Eblink {eye   :: Char
                  ,start :: Integer
                  ,end   :: Integer
                  ,dur   :: Integer
                  }
         | Efix  {eye    :: Char
                 , start :: Integer
                 , stop  :: Integer
                 , dur   :: Integer
                 , xavg  :: Float
                 , yavg  :: Float
                 , pupilAvg :: Integer
                 }
         | Esacc {eye    :: Char
                 , start :: Integer
                 , stop  :: Integer
                 , dur   :: Integer
                 , xs    :: Float
                 , ys    :: Float
                 , xe    :: Float
                 , ye    :: Float
                 , amp   :: Float
                 , pv    :: Integer
                   }
         | EDFL [EDF]
         deriving (Read, Show)


type Trials = [EDF]
  

msgFilterList = ["TRIALID"
                , "CUE"
                , "FIX1"
                , "FIX2"
                , "EFIX"
                , "ESACC"
                --, "BLINK"
                , "STIMULUS"
                --, "RESPONSE"
                , "RESP MADE"
                , "REW"]

msgP :: ReadP String
msgP = string "MSG"

esaccP :: ReadP String
esaccP = string "ESACC"

eyeP :: ReadP Char
eyeP = char 'R' <|> char 'L'

numS :: ReadS Integer
numS s = let (s1,s2) = break (not . DC.isDigit) s
         in [(read s1,s2)]

numP :: ReadP Integer
numP = readS_to_P numS

floatP ::ReadP Float
floatP  = do
  predot <- many1 $ satisfy (\c -> (DC.isDigit c)|| ((==) '-') c )
  dot <- char '.'
  postdot <- many1 $ satisfy DC.isDigit
  return (read $ predot ++ [dot] ++ postdot)

sciP :: ReadP Float
sciP = do
  predot <- many1 $ satisfy (\c -> (DC.isDigit c)|| ((==) '-') c )
  dot <- char '.'
  postdot <- (many1 (satisfy DC.isDigit))
  es <- (string "e+") <++ (string "e-")
  nz <- many1 $ satisfy DC.isDigit
  return $ (toRealFloat (read (predot ++ [dot] ++ postdot ++ es ++ nz) :: Scientific))

dotP :: ReadP Float
dotP = do
  char '.'
  return 999.999
--added to handle missing data case

floatOrDotP :: ReadP Float
floatOrDotP = floatP <|> dotP

trialidP :: ReadP String
trialidP = string "TRIALID"

rewStartParse :: ReadP EDF
rewStartParse = do
  msgP >> char '\t'
  time <- numP <* (skipSpaces >> (string "REW START"))
  return (RewStart time)

rewEndParse :: ReadP EDF
rewEndParse = do
  msgP >> char '\t'
  time <- numP <* (skipSpaces >> (string "REW END"))
  return (RewEnd time)

respMadeParse :: ReadP EDF
respMadeParse = do
  msgP >> char '\t'
  time <- numP <* (skipSpaces >> (string "RESP MADE"))
  return (RespMade time)
  
trialParse :: ReadP EDF
trialParse = do
  msgP >> char '\t'
  time <- numP <* (skipSpaces >> trialidP >> skipSpaces)
  tid <- numP
  return (Trial time tid)
  
fix1Parse :: ReadP EDF
fix1Parse = do
  msgP >> skipSpaces
  time <- numP <* (skipSpaces >> string "FIX1")
  return (FixOn time)

fix2Parse :: ReadP EDF
fix2Parse = do
  msgP >> skipSpaces
  time <- numP <* (skipSpaces >> string "FIX2")
  return (FixReturn time)

stimulusParse :: ReadP EDF
stimulusParse = do
  msgP >> char '\t'
  time <- numP <* (skipSpaces >> string "STIMULUS")
  return (Stimulus time)

cueParse :: ReadP EDF
cueParse = do
  msgP >> char '\t'
  time <- numP <* (skipSpaces >> string "CUE")
  return (Cue time)

startBlinkParse :: ReadP EDF
startBlinkParse = do
  string "SBLINK" >> skipSpaces
  eye <- eyeP <* skipSpaces
  start <- numP
  return (Sblink eye start)
  
endBlinkParse :: ReadP EDF
endBlinkParse = do
  string "EBLINK" >> skipSpaces
  eye <- eyeP <* skipSpaces
  start <- numP <* char '\t'
  end   <- numP <* char '\t'
  dur   <- numP
  return (Eblink eye start end dur)
  
startSaccParse :: ReadP EDF
startSaccParse = do
  string "SSACC" >> skipSpaces
  eye <- eyeP <* skipSpaces
  start <- numP
  return (Ssacc eye start)
  
startFixParse :: ReadP EDF
startFixParse = do
  string "SFIX" >> skipSpaces
  eye <- eyeP <* skipSpaces
  start <- numP
  return (Sfix eye start)

  
endSaccParse :: ReadP EDF
endSaccParse = do
  esaccP >> skipSpaces
  eye   <- eyeP        <* skipSpaces
  start <- numP        <* char '\t'
  stop  <- numP        <* char '\t'
  dur   <- numP        <* (char '\t'  >> skipSpaces)
  xs    <- floatP      <* (char '\t'  >> skipSpaces)
  ys    <- floatP      <* (char '\t'  >> skipSpaces)
  xe    <- floatOrDotP <* (char '\t'  >> skipSpaces)
  ye    <- floatOrDotP <* (char '\t'  >> skipSpaces)
  amp   <- (floatP <|> sciP)     <* (char '\t'  >> skipSpaces)
  pv    <- numP  
  return (Esacc eye start stop dur xs ys xe ye amp pv)

endFixParse :: ReadP EDF
endFixParse = do
  string "EFIX" >> skipSpaces
  eye   <- eyeP      <* skipSpaces
  start <- numP      <* char '\t'
  stop  <- numP      <* char '\t'
  dur   <- numP      <* (char '\t' >> skipSpaces)
  xavg    <- floatP  <* (char '\t'  >> skipSpaces)
  yavg    <- floatP  <* (char '\t'  >> skipSpaces)
  pupilAvg <- numP
  return (Efix eye start stop dur xavg yavg pupilAvg)
  

filtEDFFile :: [String] -> String -> Bool
filtEDFFile mfl l = or $ DL.isInfixOf <$> mfl <*> [l]

edfParserList = [
  trialParse
  ,fix1Parse
  ,fix2Parse
  ,startFixParse
  ,endFixParse
  ,cueParse
  ,stimulusParse
  ,respMadeParse
  ,rewStartParse
  ,rewEndParse
  ,startSaccParse
  ,endSaccParse
  ,startBlinkParse
  ,endBlinkParse
  ]

parseEDFWMsgList :: [String] -> String -> [EDF]
parseEDFWMsgList ml s = fst .last $ readP_to_S (sepBy (choice edfParserList) (char '\n')) $ unlines$ filter (filtEDFFile ml) $ lines s

parseEdf :: String -> [EDF]
parseEdf s = parseEDFWMsgList msgFilterList s



--could just create a custom show for the
--showable typeclass
edfShow :: EDF -> String
edfShow edf =
  case edf of
    (Trial tt tno) -> "\n" ++ (show edf) ++ "|"
    _          -> (show edf) ++ "|"
    
makePrisms ''EDF

get_fixes :: [EDF] -> [EDF]
get_fixes = filter (is _Efix)

get_fixes' :: [EDF] -> EDF
get_fixes' edfs = EDFL (filter (is _Efix) edfs)

get_saccs :: [EDF] -> [EDF]
get_saccs = filter (is _Esacc)

get_saccs' :: [EDF] -> EDF
get_saccs' edfs = EDFL (filter (is _Esacc) edfs)

combSsFs' :: EDF -> EDF -> [EDF]
combSsFs' ss fs =
  case (ss,fs) of
    ((EDFL []),(EDFL []))     ->  []
    ((EDFL []), _       )     ->  [fs]
    (_        ,(EDFL []))     ->  [ss]
    (_        , _       )     ->  [fs,ss]


grabTrials :: ([Trials],[EDF]) -> ([Trials],[EDF])
grabTrials e@(_,[]) = e
grabTrials e@([],y) =
  let (a,b) = grabTrials'([],y)
  in grabTrials([a],b)
grabTrials e@(x:[],y:[]) = ([x++[y]],[])
grabTrials (x,y:[]) = ((init x) ++ [last(x) ++ [y]],[])
grabTrials (a,b) =
  let(a',b') = grabTrials'([head(b)],tail(b))
  in
    grabTrials(a++[a'],b')


    
grabTrials' :: ([EDF],[EDF]) -> ([EDF],[EDF])
grabTrials' e@(_,[]) = e
grabTrials' (ps,qs) =
  let
    gtp qs
      | length qs == 0 = []
      | length qs == 1 = qs
      | otherwise      = tail qs
    t = (head qs) in
    case t of
      (RewEnd a) -> (ps ++ [t],gtp(qs))
      _          -> (grabTrials'(ps++[t],gtp(qs)))

procSaccXY :: String -> [Trials] -> [[(Float,Float)]]
procSaccXY sore trials = map ((procSaccXY1 sore) . get_saccs) trials
  where procSaccXY1 sore ledf = map (procSaccXY' sore) ledf
        procSaccXY' sore (Esacc _ start stop dur xs ys xe ye amp pv)
          |sore == "start" = (xs,ys)
          |sore == "end"   = (xe,ye)

procSaccTime :: String -> [Trials] -> [[Integer]]
procSaccTime sore trials = map ((procSaccTime1 sore) . get_saccs) trials
  where procSaccTime1 sore ledf = map (procSaccTime' sore) ledf
        procSaccTime' sore (Esacc _ start stop dur xs ys xe ye amp pv)
          |sore == "start" = start
          |sore == "end"   = stop


procFixXY :: [Trials] -> [[(Float,Float)]]
procFixXY trials = map (procFixXY1 . get_fixes) trials
  where procFixXY1 ledf = map procFixXY' ledf
        procFixXY' (Efix _ start stop _ xavg yavg pavg) = (xavg,yavg)

procFixDurPup :: String -> [Trials] -> [[Integer]]
procFixDurPup pupdur trials = map ((procFixDurPup1 pupdur) . get_fixes) trials
  where procFixDurPup1 pupdur ledf = map (procFixDurPup' pupdur) ledf
        procFixDurPup' pupdur (Efix _ start stop dur xavg yavg pavg)
          | pupdur == "pupil"    = pavg
          | pupdur == "duration" = dur
