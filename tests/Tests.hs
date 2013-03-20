import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Core.Modifier.Constant
import Core.Schedule

main = defaultMain tests

tests = [
  testGroup "Schedule modifiers" modifiers
  ]

modifiers = [
  testProperty prop_constant
  ]

prop_constant x =
