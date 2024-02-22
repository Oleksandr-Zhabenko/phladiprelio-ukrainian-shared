-- |
-- Module      :  Phladiprelio.Ukrainian.ReadDurations
-- Copyright   :  (c) OleksandrZhabenko 2021-2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Functions to read the properties data from the files with the special Haskell-like syntaxis.

{-# LANGUAGE BangPatterns, NoImplicitPrelude #-}

module Phladiprelio.Ukrainian.ReadDurations where

import GHC.Base
import CaseBi.Arr (getBFstLSorted')
import Phladiprelio.Ukrainian.SyllableDouble
import Text.Read (readMaybe)
import Data.Maybe
import System.IO
import GHC.List
import Data.List (unlines,lines)
import System.Directory (doesFileExist)
import Phladiprelio.Ukrainian.Melodics


{-| For more information on implementation, please refer to the link:
 
<https://oleksandr-zhabenko.github.io/uk/rhythmicity/PhLADiPreLiO.Eng.21.html#ability-to-use-your-own-durations-of-representations-of-sounds-or-phonetic-phenomena> 
-}
sound8s :: FlowSound
sound8s = [1,2,3,4,5,6,7,8,9,10,11,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,
   43,44,45,46,47,48,49,50,51,52,53,54,66,101]



{-|

The first number is the default value that corresponds usually to the word gap duration (and here is not important).
The next 52 'Double' numbers become the durations of the above specified 'Sound8' values respectively, the order
must be preserved (if you consider it important, well, it should be!). If some number in the file cannot be read
as a 'Double' number the function uses the first one that can be instead (the default value). If no such is specified
at all, then the default number is 1.0 for all the 'Sound8' sound representations that is hardly correct.

-}
readSound8ToDouble :: String -> (Double,[(Sound8, Double)])
readSound8ToDouble xs
 | null xs = (1.0,zip sound8s . replicate 10000 $ 1.0)
 | otherwise =
    let wws = lines xs
        dbls = map (\ks -> readMaybe ks::Maybe Double) wws
        dbH
         | null dbls || all isNothing dbls = 1.0
         | otherwise = fromJust . head . filter isJust $ dbls
        dbSs = map (fromMaybe dbH) dbls
        (firstD,lsts)
          | null dbls = (1.0,zip sound8s . replicate 10000 $ 1.0)
          | otherwise = (dbH,zip sound8s (dbSs `mappend` replicate 10000 1.0))
            in (firstD,lsts)

divide2SDDs :: String -> [String]
divide2SDDs ys
 | null tss = [unlines kss]
 | otherwise = unlines kss : divide2SDDs (unlines rss)
     where wwss = lines ys
           (kss,tss) = break (any (=='*')) wwss
           rss = dropWhile (any (== '*')) tss

readSyllableDurations :: FilePath -> IO [[[[Sound8]]] -> [[Double]]]
readSyllableDurations file = do
  exists <- doesFileExist file
  if exists then do 
   xs <- readFile file
   let yss = take 9 . divide2SDDs $ xs
       readData = map readSound8ToDouble yss
   return . map (\(d,zs) -> syllableDurationsGDc (getBFstLSorted' d zs)) $ readData
  else return []

