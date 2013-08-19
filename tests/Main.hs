import           Test.Tasty

import qualified Modifiers

main :: IO ()
main = defaultMain $ testGroup "VPlan"
  [ Modifiers.properties
  ]
