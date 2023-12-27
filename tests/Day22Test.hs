module Main where

import Day22
import Test.HUnit
import qualified System.Exit as Exit


-- Well I planned to try adding some testing to a broken bit of code, but haven't gotten round to it yet. Might do in the future, idk how much more effort I can put into this thing though...

testGroundTiles :: Test
testGroundTiles = TestCase (assertEqual "Should have length 42" 42 (length (groundTiles (Block (1,1,4) (6,7,6) 4))))


tests :: Test
tests = TestList [TestLabel "testGroundTiles" testGroundTiles]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess