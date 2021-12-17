module Cli
  ( pubSpecFile,
    pubSpecLockFile,
    flutterNixLockFile,
    noCache,
    quiet,
    verbose,
  )
where

import Options.Applicative
  ( Parser,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    progDesc,
    short,
    strOption,
    switch,
    value,
  )

data Options = Options
  { _pubSpecFile :: String,
    _pubSpecLockFile :: String,
    _flutterNixLockFile :: String,
    _noCache :: Bool,
    _quiet :: Bool,
    _verbose :: Bool
  }

opts :: Parser Options
opts =
  Options
    <$> strOption
      ( long "pubspec-file"
          <> metavar "FILE"
          <> value "pubspec.yaml"
          <> help
            ( concat
                [ "Path to the pubspec file to read ",
                  "(defaults to: pubspec.yaml)"
                ]
            )
      )
    <*> strOption
      ( long "pubspec-lock-file"
          <> metavar "FILE"
          <> value "pubspec.lock"
          <> help
            ( concat
                [ "Path to the pubspec lock file to read ",
                  "(defaults to: pubspec.lock)"
                ]
            )
      )
    <*> strOption
      ( long "flutter-nix-lock-file"
          <> metavar "FILE"
          <> value "flutter-nix-lock.json"
          <> help
            ( concat
                [ "Path to the flutter-nix lock file to write ",
                  "(defaults to: flutter-nix-lock.json)"
                ]
            )
      )
    <*> switch
      ( long "no-cache"
          <> help "Don't use the old lock file as a cache"
      )
    <*> switch
      ( long "quiet"
          <> short 'q'
          <> help "Don't print output to stdout"
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Print additional output to stdout"
      )

options :: IO Options
options =
  execParser $
    info
      (helper <*> opts)
      ( fullDesc
          <> progDesc
            ( concat
                [ "Creates a flutter-nix lock file in JSON necessary to build ",
                  "the Flutter app in Nix sandbox.\n",
                  "Pubspec file is used to get the name and the version of ",
                  "the Flutter app.\n",
                  "Pubspec lock file is used to calculate the URL of the Pub ",
                  "packages and prefetch them to calculate their hashes.\n",
                  "A file in JSON format is used to calculate the URLs of the ",
                  "Flutter SDK dependencies. This file is considered an ",
                  "implementation detail and passed to the translator with an ",
                  "environment variable."
                ]
            )
          <> header "flutter-nix translator"
      )

pubSpecFile :: IO String
pubSpecFile = _pubSpecFile <$> options

pubSpecLockFile :: IO String
pubSpecLockFile = _pubSpecLockFile <$> options

flutterNixLockFile :: IO String
flutterNixLockFile = _flutterNixLockFile <$> options

noCache :: IO Bool
noCache = _noCache <$> options

quiet :: IO Bool
quiet = _quiet <$> options

verbose :: IO Bool
verbose = _verbose <$> options
