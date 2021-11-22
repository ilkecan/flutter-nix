module Cli
  ( Options (Options),
    opts,
  )
where

import Options.Applicative

data Options = Options
  { _pubspecLockFile :: !String,
    _flutterNixLockFile :: !String
  }

options :: Parser Options
options =
  Options
    <$> strOption
      ( long "input"
          <> short 'i'
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
      ( long "output"
          <> short 'o'
          <> metavar "FILE"
          <> value "flutter-nix-lock.json"
          <> help
            ( concat
                [ "Path to the flutter-nix lock file to write ",
                  "(defaults to: flutter-nix-lock.json)"
                ]
            )
      )

opts :: ParserInfo Options
opts =
  info
    (helper <*> options)
    ( fullDesc
        <> progDesc
          ( concat
              [ "Translate the pubspec lock file to flutter-nix lock file by ",
                "prefetching the hosted packages and calculating their hashes."
              ]
          )
        <> header "flutter-nix translator"
    )
