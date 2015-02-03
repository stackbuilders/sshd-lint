{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Console.GetOpt

import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..), exitWith, exitSuccess)

import Paths_sshd_lint (version)

import Data.Version (showVersion)

import Data.KeywordArgs.Parse
import System.SshdLint.Check

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad (unless, when)
import System.Nagios.Plugin

import Text.Parsec (ParseError, parse)

data Options = Options
    { optShowVersion :: Bool
    , optShowHelp    :: Bool
    , optNagios      :: Bool
    , optFile        :: FilePath
    } deriving Show

defaultOptions :: Options
defaultOptions = Options
    { optShowVersion = False
    , optShowHelp    = False
    , optNagios      = False
    , optFile        = "/etc/ssh/sshd_config"
    }

options :: [OptDescr (Options -> Options)]
options =
  [ Option "V" ["version"] (NoArg (\opts -> opts { optShowVersion = True }))
    "show version number"

  , Option "f" ["file"]
    (ReqArg (\f opts -> opts { optFile = f })
     "FILE") "input FILE"

  , Option "h" ["help"]    (NoArg (\opts -> opts { optShowHelp = True }))
    "show help"

  , Option "n" ["nagios"]    (NoArg (\opts -> opts { optNagios = True }))
    "display output in Nagios-consumable format"
  ]

showHelp :: IO ()
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

parseConfig :: String -> Either ParseError [(String, [String])]
parseConfig = parse configParser "(unknown)"

printResult :: String -> [String] -> IO ()
printResult msg errs =
  unless (null errs) $
    do
      name <- getProgName
      putStrLn $ name ++ " encountered the following " ++ msg ++ ":\n"
      mapM_ (\err -> putStrLn $ "\t* " ++ err) errs
      putStr "\n"

runCheck :: Options -> IO ()
runCheck opts = do

  f <- readFile $ optFile opts

  unless (optNagios opts) $ putStrLn ("Checking " ++ optFile opts ++ ":\n")

  case parseConfig f of
    Left e -> putStrLn $ "Parse error: " ++ show e
    Right config -> do
      let dupVals = Set.toList $ duplicatedValues $ map fst config
          recs = recommendations (Map.fromList defaultAcceptedValues) $
                 activeSettings config

      unless (optNagios opts) $ do
        printResult "insecure settings" recs
        printResult "duplicate values" dupVals

      if (not . null) dupVals || (not . null) recs
        then if optNagios opts
             then
               runNagiosPlugin (sshdSafeCheck $ length $ recs ++ dupVals)

             else do
               putStrLn $ show (length (recs ++ dupVals)) ++ " suggestions"
               exitWith (ExitFailure 1)

        else
          putStrLn "No suggestions"

sshdSafeCheck :: Int -> NagiosPlugin ()
sshdSafeCheck errorCount =
  when (errorCount > 0) $
    addResult Critical "Security warnings found in sshd_config"

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
      runCheck $ fst opts
