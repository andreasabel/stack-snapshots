module TestBump (bumpTestsIO) where

import Test.Tasty ( TestTree )
import Test.Tasty.Golden ( goldenVsFileDiff )
import System.FilePath ((</>), takeFileName)
import System.Process (callProcess)
import System.Directory (listDirectory, copyFile, setCurrentDirectory, getCurrentDirectory)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Control.Monad (forM_)
import Data.List (isSuffixOf, sort, isPrefixOf)

-- | Generate bump tests - all files are bumped in one temp directory
bumpTestsIO :: IO [TestTree]
bumpTestsIO = do
  -- Create a temp directory for this test run
  sysTempDir <- getCanonicalTemporaryDirectory
  tempDir <- createTempDirectory sysTempDir "stack-snapshots-test"

  -- Find all stack*.yaml files
  -- Note: This filter is also in StackYaml.isStackYaml but we duplicate it here
  -- to keep the test suite independent from internal modules
  allFiles <- listDirectory "test/tests"
  let stackYamlFiles = sort $ filter isStackYaml allFiles
      isStackYaml name =
        let fname = takeFileName name
        in "stack" `isPrefixOf` fname && ".yaml" `isSuffixOf` fname

  -- Copy all files to temp directory
  forM_ stackYamlFiles $ \file -> do
    copyFile ("test/tests" </> file) (tempDir </> file)

  -- Run bump once in the temp directory
  cwd <- getCurrentDirectory
  setCurrentDirectory tempDir
  callProcess "stack-snapshots" ["bump"]
  setCurrentDirectory cwd

  -- Create a golden test for each file
  return $ map (makeGoldenTest tempDir) stackYamlFiles
  where

    makeGoldenTest tempDir file = goldenVsFileDiff
      ("bump " ++ file)
      (\ref new -> ["diff", "-u", ref, new])
      ("test/golden/tests" </> file)
      (tempDir </> file)
      (return ())  -- Action is empty since bump already ran
