module System.SshdLint.Check
       ( duplicatedValues
       , activeSettings
       , recommendations
       , defaultAcceptedValues, checkSafeSetting ) where

import Data.Char (toLower)

import qualified Data.Map as Map
import qualified Data.Set as Set

type RecommendedSettings = Map.Map String String
type ActiveSettings      = Map.Map String String


defaultAcceptedValues :: [(String, String)]
defaultAcceptedValues =
  [ ("PermitEmptyPasswords", "no")
  , ("PasswordAuthentication", "no")
  , ("HostbasedAuthentication", "no")
  , ("PermitRootLogin", "no")
  , ("IgnoreRhosts", "yes")
  , ("Protocol", "2")
  , ("StrictModes", "yes")
  , ("UsePrivilegeSeparation", "yes") ]

duplicatedValues :: [String] -> Set.Set String
duplicatedValues values =
  snd $ foldr checkDuplicate (Set.empty, Set.empty) values

activeSettings :: [(String, String)] -> ActiveSettings
activeSettings =
  foldr registerSetting Map.empty

  where registerSetting (configOption, value) =
          Map.insert (map toLower configOption) value

-- | Given a collection of recommendation strings and a current
-- setting, returns a new list of recommendations with new
-- recommendations added depending on the setting of the given
-- configOption and value.
checkSafeSetting :: Map.Map String String
                 -> String
                 -> String
                 -> [String]
                 -> [String]
checkSafeSetting recs configOption value previousRecommendations =
  let recommendation = Map.lookup configOption recs in

  case recommendation of
    Nothing -> previousRecommendations
    Just r -> if r == value then
                previousRecommendations
              else
                previousRecommendations ++ [ configOption ++ " should be "
                                             ++ r ++ ", found " ++ value ]


recommendations :: RecommendedSettings -> ActiveSettings -> [String]
recommendations recommendedSettings settings =
  Map.foldWithKey (checkSafeSetting normalizedRecommendedSettings) []
                  normalizedSettings

  where normalizedRecommendedSettings = lowerCaseMapKeys recommendedSettings
        normalizedSettings = lowerCaseMapKeys settings


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

lowerCaseMapKeys :: Map.Map String String -> Map.Map String String
lowerCaseMapKeys = Map.mapKeys (map toLower)
