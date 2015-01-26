module System.SshdLint.Check
       ( duplicatedValues
       , activeSettings
       , recommendations
       , defaultAcceptedValues ) where

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

recommendations :: RecommendedSettings -> ActiveSettings -> [String]
recommendations recommendedSettings activeSettings =
  Map.foldWithKey (checkSafeSetting normalizedRecommendedSettings) []
                  normalizedActiveSettings

  where checkSafeSetting recs configOption value recommendations =
          let recommendation = Map.lookup configOption recs in

          case recommendation of
            Nothing -> recommendations
            Just r -> if r == value then
                        recommendations
                      else
                        recommendations ++
                        [ configOption ++ " should be " ++ r ++ ", found " ++
                          value ]

        normalizedRecommendedSettings = lowerCaseMapKeys recommendedSettings
        normalizedActiveSettings = lowerCaseMapKeys activeSettings


-- | Some values legitimately appear multiple times in the configuration.
allowedDuplicates :: Set.Set String
allowedDuplicates = Set.fromList ["hostkey"]

checkDuplicate :: String
               -> (Set.Set String, Set.Set String)
               -> (Set.Set String, Set.Set String)

checkDuplicate aValue (allValues, duplicatedValues) =
  if Set.notMember loweredValue allowedDuplicates &&
     Set.member loweredValue allValues then

    (allValues, Set.insert loweredValue duplicatedValues)
  else
    (Set.insert loweredValue allValues, duplicatedValues)

  where loweredValue = map toLower aValue

lowerCaseMapKeys :: Map.Map String String -> Map.Map String String
lowerCaseMapKeys = Map.mapKeys (map toLower)
