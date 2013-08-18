import           Test.Tasty

import qualified Modifiers

main :: IO ()
main = defaultMain $ testGroup ""
  [ Modifiers.properties
  ]
