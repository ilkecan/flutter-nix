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
    short,
    strOption,
    value,
  )

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
              [ "Creates a flutter-nix lock file by prefetching the hosted ",
                "Pub packages and Flutter SDK dependencies and calculating ",
                "their hashes.\n",
                "Uses the pubspec lock file to calculate the URL of the Pub ",
                "packages and a file in JSON format to calculate the URLs of ",
                "the Flutter SDK dependencies.\n",
                "The pubspec lock file is passed with a command-line argument ",
                "while the JSON file is passed with ",
                "`FLUTTER_SDK_DEPENDENCIES_JSON` environment variable."
              ]
          )
        <> header "flutter-nix translator"
    )
