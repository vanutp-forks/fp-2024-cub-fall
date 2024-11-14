import Test.Tasty

import qualified Test.Props

main :: IO ()
main = do
  defaultMain (testGroup "All Tests"
                [ testGroup "Props" Test.Props.props
                ])