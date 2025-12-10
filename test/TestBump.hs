module TestBump (bumpTests) where

import Test.Tasty
import Test.Tasty.Golden
import System.FilePath ((</>))
import System.Process (callProcess)
import System.Directory (setCurrentDirectory, getCurrentDirectory, copyFile, removeFile)
import Control.Exception (bracket_)

bumpTests :: [TestTree]
bumpTests =
  [ goldenVsFileDiff
      "bump stack-9.0.yaml"
      (\ref new -> ["diff", "-u", ref, new])
      "test/golden/tests/stack-9.0.yaml"
      "test/tests/stack-9.0.yaml"
      (runBumpTest "stack-9.0.yaml")
  , goldenVsFileDiff
      "bump stack-9.2.yaml"
      (\ref new -> ["diff", "-u", ref, new])
      "test/golden/tests/stack-9.2.yaml"
      "test/tests/stack-9.2.yaml"
      (runBumpTest "stack-9.2.yaml")
  , goldenVsFileDiff
      "bump stack-9.4.yaml"
      (\ref new -> ["diff", "-u", ref, new])
      "test/golden/tests/stack-9.4.yaml"
      "test/tests/stack-9.4.yaml"
      (runBumpTest "stack-9.4.yaml")
  , goldenVsFileDiff
      "bump stack-9.6.yaml"
      (\ref new -> ["diff", "-u", ref, new])
      "test/golden/tests/stack-9.6.yaml"
      "test/tests/stack-9.6.yaml"
      (runBumpTest "stack-9.6.yaml")
  ]

runBumpTest :: FilePath -> IO ()
runBumpTest file = do
  cwd <- getCurrentDirectory
  let testFile = "test/tests" </> file
  let backupFile = testFile ++ ".backup"
  
  -- Backup the file first
  copyFile testFile backupFile
  
  -- Run bump in test directory
  bracket_
    (setCurrentDirectory "test/tests")
    (setCurrentDirectory cwd >> copyFile backupFile testFile >> removeFile backupFile)
    (callProcess "stack-snapshots" ["bump"])
