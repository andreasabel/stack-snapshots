module TestDryRun (dryRunTests) where

import Test.Tasty
import Test.Tasty.Golden
import System.Process (readProcess)
import System.Directory (setCurrentDirectory, getCurrentDirectory)
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

dryRunTests :: [TestTree]
dryRunTests =
  [ goldenVsString
      "dry-run output"
      "test/golden/dry-run.golden"
      runDryRunTest
  , goldenVsString
      "dry-run with specific files"
      "test/golden/dry-run-files.golden"
      runDryRunWithFilesTest
  ]

runDryRunTest :: IO BSL.ByteString
runDryRunTest = runDryRun ["dry-run", "--color=never"]

runDryRunWithFilesTest :: IO BSL.ByteString
runDryRunWithFilesTest = runDryRun ["dry-run", "--color=never", "stack-9.6.yaml", "stack-9.8.yaml"]

-- | Helper to run dry-run with specified arguments
runDryRun :: [String] -> IO BSL.ByteString
runDryRun args = do
  cwd <- getCurrentDirectory
  setCurrentDirectory "test/tests"
  output <- readProcess "stacker" args ""
  setCurrentDirectory cwd
  -- Properly encode as UTF-8
  return $ BSL.fromStrict $ TE.encodeUtf8 $ T.pack output
