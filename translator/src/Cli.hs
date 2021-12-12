module Cli
  ( Options (Options),
    opts,
  )
where

import Options.Applicative
  ( Parser,
    ParserInfo,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    progDesc,
    strOption,
    value,
  )

data Options = Options
  { _pubspecFile :: !String,
    _pubspecLockFile :: !String,
    _flutterNixLockFile :: !String
  }

options :: Parser Options
options =
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

opts :: ParserInfo Options
opts =
  info
    (helper <*> options)
    ( fullDesc
        <> progDesc
          ( concat
              [ "Creates a flutter-nix lock file in JSON necessary to build ",
                "the Flutter app in Nix sandbox.\n",
                "Pubspec file is used to get the name and the version of the ",
                "Flutter app.\n",
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
