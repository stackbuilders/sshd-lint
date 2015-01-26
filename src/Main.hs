module Main where


import System.Console.GetOpt

import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..), exitWith, exitSuccess)
import System.IO.Error (ioError)

import Paths_sshd_lint (version)
import Data.Maybe (fromMaybe)

import Data.Version (showVersion)

import Data.KeywordArgs.Parse
import System.SshdLint.Check

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad (mapM_, unless)

import Text.Parsec

import Text.ParserCombinators.Parsec.Error(ParseError, Message,
                                           errorMessages, messageEq)


{-# ANN module "HLint: ignore Use string literal" #-}

data Options = Options
    { optShowVersion :: Bool
    , optShowHelp    :: Bool
    , optFile        :: FilePath
    } deriving Show

defaultOptions = Options
    { optShowVersion = False
    , optShowHelp    = False
    , optFile        = "/etc/ssh/sshd_config"
    }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['V'] ["version"] (NoArg (\opts -> opts { optShowVersion = True }))
    "show version number"

  , Option ['f'] ["file"]
    (ReqArg (\f opts -> opts { optFile = f })
     "FILE") "input FILE"

  , Option ['h'] ["help"]    (NoArg (\opts -> opts { optShowHelp = True }))
    "show help"
  ]

showHelp = do
  prg <- getProgName
  putStrLn (usageInfo prg options)
  exitSuccess

headerMessage :: String -> String
headerMessage progName = "Usage: " ++ progName ++ " [OPTION...]"

lintOpts :: [String] -> IO (Options, [String])
lintOpts argv = do
  name <- getProgName

  case getOpt Permute options argv of
    (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
    (_, _, errs) ->
      ioError (userError (concat errs ++ usageInfo (headerMessage name) options))

parseConfig :: String -> Either ParseError [(String, String)]
parseConfig = parse configParser "(unknown)"

printResult :: String -> [String] -> IO ()
printResult msg errs =
  unless (null errs) $
    do
      name <- getProgName
      putStrLn $ name ++ " encountered the following " ++ msg ++ ":\n"
      mapM_ (\err -> putStrLn $ "\t* " ++ err) errs
      putStr "\n"

runCheck :: FilePath -> IO ()
runCheck file = do

  f <- readFile file

  case parseConfig f of
    Left e -> putStrLn $ "Parse error: " ++ show e
    Right config -> do
      let dupVals = Set.toList $ duplicatedValues $ map fst config
          recs = recommendations (Map.fromList defaultAcceptedValues) $
                 activeSettings config

      printResult "insecure settings" recs
      printResult "duplicate values" dupVals

      if (not . null) dupVals || (not . null) recs
        then
          do
            putStrLn $ show (length (recs ++ dupVals)) ++ " suggestions"
            exitWith (ExitFailure 1)
        else
          putStrLn "No suggestions"


main :: IO ()
main = do
  opts <- getArgs >>= lintOpts

  name <- getProgName

  if optShowHelp $ fst opts
  then
    putStrLn $ usageInfo (headerMessage name) options
  else
    if optShowVersion $ fst opts then
      putStrLn $ name ++ " " ++ showVersion version ++
      " (C) 2015 Stack Builders Inc."
    else
      runCheck $ optFile $ fst opts
