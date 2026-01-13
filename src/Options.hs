{-# LANGUAGE OverloadedStrings #-}

module Options
  ( parseOptions
  , optionsParserInfo
  , Options(..)
  ) where

import Data.Char (toLower)
import Options.Applicative
import Types

-- | Parse command-line options
parseOptions :: IO Options
parseOptions = customExecParser (prefs showHelpOnEmpty) optionsParserInfo

-- | Parser info (exported for help rendering)
optionsParserInfo :: ParserInfo Options
optionsParserInfo = info (optionsParser <**> helper)
  ( fullDesc
 <> progDesc "A tool to bump snapshots (resolvers) in stack*.yaml files"
 <> header ("stacker version " ++ appVersion)
 <> footer "For more information, see the README" )

-- | Options parser
-- The parser tries subcommand parsing first (which includes color options),
-- then falls back to top-level options (for version/help flags and default).
-- This allows --color to be specified either before the subcommand
-- (e.g., "stacker --color=never dry-run") or within the subcommand
-- (e.g., "stacker dry-run --color=never file.yaml").
optionsParser :: Parser Options
optionsParser = 
  -- Try to parse as subcommand with options first
  subcommandWithOptions
  -- Fall back to top-level options
  <|> topLevelOptions
  where
    -- Subcommands that include color option
    subcommandWithOptions = subparser
      ( command "bump" (info (Options <$> bumpParser <*> colorOption) (progDesc "Update stack*.yaml files (optionally specify files)"))
     <> command "dry-run" (info (Options <$> dryRunParser <*> colorOption) (progDesc "Show what would be updated (default, optionally specify files)"))
     <> command "update" (info (Options <$> pure Update <*> colorOption) (progDesc "Update stackage snapshots database"))
     <> command "info" (info (Options <$> pure Info <*> colorOption) (progDesc "Print GHC version to snapshot mapping"))
     <> command "config" (info (Options <$> configParser <*> colorOption) (progDesc "Configure stacker"))
     <> command "version" (info (Options <$> pure Version <*> colorOption) (progDesc "Print version information (also: -V, --version)"))
     <> command "numeric-version" (info (Options <$> pure NumericVersion <*> colorOption) (progDesc "Print version number (also: --numeric-version)"))
     <> command "license" (info (Options <$> pure PrintLicense <*> colorOption) (progDesc "Print license text (also: --license)"))
     <> command "help" (info (Options <$> pure Help <*> colorOption) (progDesc "Print this help (also: -h, --help)"))
      )
    
    -- Top level parsing (for flags and default)
    topLevelOptions = Options
      <$> commandParserTopLevel
      <*> colorOption
    
    -- Top-level command parser (for flags)
    commandParserTopLevel = 
      flag' Version (long "version" <> short 'V' <> help "Print version information")
      <|> flag' NumericVersion (long "numeric-version" <> help "Print version number")
      <|> flag' PrintLicense (long "license" <> help "Print license text")
      <|> flag' Help (long "help" <> short 'h' <> help "Print help")
      <|> pure (DryRun [])

-- | Config command parser
configParser :: Parser Command
configParser = Config <$> subparser
  ( command "repo" (info repoParser (progDesc "Set repository path"))
  )

-- | Repo config parser
repoParser :: Parser ConfigCmd
repoParser = SetRepo <$> argument str (metavar "PATH")

-- | Helper to parse file arguments
filesParser :: ([FilePath] -> Command) -> Parser Command
filesParser cmd = cmd <$> many (argument str (metavar "FILES..." <> action "file"))

-- | Bump command parser
bumpParser :: Parser Command
bumpParser = filesParser Bump

-- | Dry-run command parser
dryRunParser :: Parser Command
dryRunParser = filesParser DryRun

-- | Color option parser
colorOption :: Parser ColorWhen
colorOption = option readColorWhen
  ( long "color"
 <> metavar "WHEN"
 <> value Auto
 <> help "Use colored output (always, never, auto)" )
  where
    readColorWhen = maybeReader $ \s ->
      case map toLower s of
        "always" -> Just Always
        "never" -> Just Never
        "auto" -> Just Auto
        _ -> Nothing
