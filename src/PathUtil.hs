-- | Path utilities for cross-platform compatibility
-- 
-- This module provides path utilities that use forward slash (/) as the 
-- path separator on all platforms, including Windows. While Windows primarily
-- uses backslash (\), it also accepts forward slash for relative paths,
-- which is what this tool deals with.
--
-- This approach ensures consistent path handling and output across all platforms,
-- avoiding the path separator issues that can occur when using System.FilePath's
-- platform-specific path combinators.
module PathUtil
  ( (</>)
  , normalizeFilePath
  ) where

import Data.List (stripPrefix)

-- | Cross-platform path combination using forward slash
-- This ensures consistent behavior across Linux, macOS, and Windows
-- for relative paths (which is what this tool processes).
--
-- Note: This is specifically designed for combining relative path components.
-- For absolute paths or complex path manipulations, System.FilePath should be used.
(</>) :: FilePath -> FilePath -> FilePath
"" </> b = b
a </> "" = a
a </> b = a ++ "/" ++ b

-- | Normalize a file path by removing "./" prefix
-- This is used to ensure consistent output format where paths
-- don't have a leading "./" component.
normalizeFilePath :: FilePath -> FilePath
normalizeFilePath path =
  case stripPrefix "./" path of
    Just rest -> rest
    Nothing -> path
