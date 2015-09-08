module System.SshdLint.Check
       ( duplicatedValues
       , activeSettings
       , recommendations
       , defaultAcceptedValues
       , checkSafeSetting ) where

import Data.Char (toLower)

import qualified Data.Map as Map
import qualified Data.Set as Set

type Configuration = [String]
type RecommendedSettings = Map.Map String [Configuration]
type ActiveSettings      = Map.Map String Configuration


defaultAcceptedValues :: [(String, [Configuration])]
defaultAcceptedValues =
  [ ("PermitEmptyPasswords", [["no"]])
  , ("PasswordAuthentication", [["no"]])
  , ("HostbasedAuthentication", [["no"]])
  , ("PermitRootLogin", [["no"], ["without-password"]])
  , ("IgnoreRhosts", [["yes"]])
  , ("Protocol", [["2"]])
  , ("StrictModes", [["yes"]])
  , ("UsePrivilegeSeparation", [["yes"]]) ]

duplicatedValues :: [String] -> Set.Set String
duplicatedValues values =
  snd $ foldr checkDuplicate (Set.empty, Set.empty) values

activeSettings :: [(String, Configuration)] -> ActiveSettings
activeSettings =
  foldr registerSetting Map.empty

  where registerSetting (configOption, values) =
          Map.insert (map toLower configOption) values

-- | Given a collection of recommendation strings and a current
-- setting, returns a new list of recommendations with new
-- recommendations added depending on the setting of the given
-- configOption and value.
checkSafeSetting :: Map.Map String [[String]]
                 -> String
                 -> [Configuration]
                 -> Configuration
                 -> [String]
checkSafeSetting recs configOption values previousRecommendations =
  let recommendation = Map.lookup configOption recs in

  case recommendation of
    Nothing -> previousRecommendations
    Just r  -> do
      let config = (Set.fromList . head) values
      let allowedConfigurations = map Set.fromList r
      if config `elem` allowedConfigurations then
        previousRecommendations
      else
        previousRecommendations ++ [ configOption ++ " should be "
                                     ++ show r ++ ", found " ++
                                     (show . head) values ]


recommendations :: RecommendedSettings -> ActiveSettings -> [String]
recommendations recommendedSettings settings =
  Map.foldWithKey (checkSafeSetting normalizedRecommendedSettings) []
                  normalizedSettings

  where normalizedRecommendedSettings = lowerCaseMapKeys recommendedSettings
        normalizedSettings = Map.map return $ lowerCaseMapKeys settings


-- | Some values legitimately appear multiple times in the configuration.
allowedDuplicates :: Set.Set String
allowedDuplicates = Set.fromList ["hostkey"]

checkDuplicate :: String
               -> (Set.Set String, Set.Set String)
               -> (Set.Set String, Set.Set String)

checkDuplicate aValue (allValues, dupes) =
  if Set.notMember loweredValue allowedDuplicates &&
     Set.member loweredValue allValues then

    (allValues, Set.insert loweredValue dupes)
  else
    (Set.insert loweredValue allValues, dupes)

  where loweredValue = map toLower aValue

lowerCaseMapKeys :: Map.Map String a -> Map.Map String a
lowerCaseMapKeys = Map.mapKeys (map toLower)
