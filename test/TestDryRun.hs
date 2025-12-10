module TestDryRun (dryRunTests) where

import Test.Tasty
import Test.Tasty.Golden
import System.FilePath ((</>))
import System.Process (readProcess)
import System.Directory (setCurrentDirectory, getCurrentDirectory)
import qualified Data.ByteString as BS

dryRunTests :: [TestTree]
dryRunTests =
  [ goldenVsString
      "dry-run output"
      "test/golden/dry-run.txt"
      runDryRunTest
  ]

runDryRunTest :: IO BS.ByteString
runDryRunTest = do
  cwd <- getCurrentDirectory
  setCurrentDirectory "test/tests"
  output <- readProcess "stack-snapshots" ["dry-run"] ""
  setCurrentDirectory cwd
  return $ BS.pack $ map (fromIntegral . fromEnum) output
