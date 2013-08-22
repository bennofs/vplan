import           Test.Tasty

import qualified Combinators
import qualified Modifiers

main :: IO ()
main = defaultMain $ testGroup "VPlan"
  [ Modifiers.properties
  , Combinators.properties
  ]
