module System.SshdLint.CheckSpec (spec) where

import Test.Hspec

import System.SshdLint.Check

import qualified Data.Set as Set
import qualified Data.Map as Map


spec :: Spec
spec =
  describe "normalizing and validating settings" $ do
    describe "duplicatedValues" $ do

      it "returns a list of keys found multiple times" $
        duplicatedValues [ "PermitEmptyPasswords"
                         , "PermitEmptyPasswords" ] `shouldBe`
        Set.fromList ["permitemptypasswords"]

      it "finds duplicated elements regardless of case" $
        duplicatedValues [ "PermitEmptyPasswords"
                         , "permitemptypasswords" ] `shouldBe`
        Set.fromList ["permitemptypasswords"]

      it "allows duplicate HostKey values" $
        duplicatedValues [ "HostKey"
                         , "hostkey" ] `shouldBe`
        Set.empty

    describe "activeSettings" $
      it "returns settings based on their first occurrence in the list" $
        activeSettings [ ("PermitEmptyPasswords", ["yes"])
                       , ("PermitEmptyPasswords", ["no"]) ]
        `shouldBe` Map.fromList [ ("permitemptypasswords", ["yes"]) ]

    describe "recommendations" $ do
      it "returns all settings that are unsafe, along with better alternatives" $
        recommendations (Map.fromList [("permitemptypasswords", [["no"]])])
          (Map.fromList [("permitemptypasswords", ["yes"])])
          `shouldBe` ["permitemptypasswords should be [[\"no\"]], found [[\"yes\"]]"]

      it "finds recommendations even when there are case differences" $
        recommendations (Map.fromList [("PermitEmptyPasswords", [["no"]])])
          (Map.fromList [("permitemptypasswords", ["yes"])])
          `shouldBe` ["permitemptypasswords should be [[\"no\"]], found [[\"yes\"]]"]

      it "passes over config options for which we have no recommendations" $
        recommendations Map.empty
          (Map.fromList [("permitemptypasswords", ["yes"])])
          `shouldBe` [ ]

      it "doesn't care about ordering of values" $
        recommendations (Map.fromList [("AcceptEnv", [["LANG", "LC_*"]])])
          (Map.fromList [("acceptenv", ["LC_*", "LANG"])])
          `shouldBe` [ ]

      it "allows to have more than one allowed default value in the defaultAcceptedValues" $ do
        recommendations (Map.fromList [("PermitRootLogin", [["no"], ["without-password"]])])
          (Map.fromList [("PermitRootLogin", ["no"])])
          `shouldBe` [ ]
        recommendations (Map.fromList [("PermitRootLogin", [["no"], ["without-password"]])])
          (Map.fromList [("PermitRootLogin", ["without-password"])])
          `shouldBe` [ ]

      it "doesn't allow an invalid value if it's not in the allowed values" $
        recommendations (Map.fromList [("PermitRootLogin", [["no"], ["without-password"]])])
          (Map.fromList [("PermitRootLogin", ["yes"])])
          `shouldBe` ["permitrootlogin should be [[\"no\"],[\"without-password\"]], found [[\"yes\"]]"]
