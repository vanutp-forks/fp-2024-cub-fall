
import Test.Tasty (defaultMain, testGroup)
import Tests.Dictionary
import Tests.Spellcheck
import Tests.Utils

main :: IO ()
main =
  defaultMain $ testGroup "Tests" [testDictionary, testSpellcheck, testUtils]
