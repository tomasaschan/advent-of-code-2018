module Computer.ComputerSpec where

import           Computer

import           Test.Hspec
import           Test.Hspec.QuickCheck

newtype TestState = S Int deriving (Show, Eq)

instance State TestState where
  ip (S p) = p
  memory _ = undefined
  register _ = undefined

  withIp p _ = S p
  withMemory _ _ = undefined

spec :: Spec
spec = do
  describe "incIp" $ do
    prop "increments by 1" $ \p -> do
      let s  = S p
      let s' = incIp s

      s' `shouldBe` S (p + 1)
