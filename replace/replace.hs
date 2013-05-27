#!/usr/bin/env runghc

{-# OPTIONS_GHC -O -Wall #-}

import System.IO (openTempFile, hClose, hPutStr)
import System.Directory (renameFile)
import System.Environment (getArgs)
import Data.List (isPrefixOf)

evenOrd :: [a] -> [a]
evenOrd (x:y:ys) = x:(evenOrd ys)
evenOrd [x] = [x]
evenOrd [] = []

oddOrd :: [a] -> [a]
oddOrd (x:xs) = evenOrd xs
oddOrd [] = []

parseArgs :: [String] -> ([(String, String)], [String])
parseArgs args =
  let Just(index) = lookup "--" (zip args [1..])
  in let (words, files) = splitAt index args
     in (zip (evenOrd words) (oddOrd words), files)

replaceAll :: String -> String -> String -> String
replaceAll _ _ [] = []
replaceAll from to src@(s:ss)
  | isPrefixOf from src = to ++ replaceAll from to (drop (length from) src)
  | otherwise = s : replaceAll from to ss

replaceAllWords :: [(String, String)] -> String -> String
replaceAllWords targets src = foldl repl src targets
  where repl src (from, to) = replaceAll from to src

doReplaceWordsOfFile :: String -> [(String, String)] -> IO ()
doReplaceWordsOfFile file words =
  do src <- readFile file
     let dst = replaceAllWords words src
     (tempfile, temph) <- openTempFile "." "backup"
     hPutStr temph dst
     hClose temph
     renameFile tempfile file

doReplaceWordsOfFiles :: [String] -> [(String, String)] -> IO()
doReplaceWordsOfFiles files words = mapM_ doReplace files where
  doReplace file = doReplaceWordsOfFile file words

main = do 
    args <- getArgs
    let (words, files) = parseArgs args
       in doReplaceWordsOfFiles files words
