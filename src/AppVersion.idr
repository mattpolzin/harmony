module AppVersion

%default total

export
appVersion : String
appVersion = "3.3.0"

export
printVersion : HasIO io => io ()
printVersion =
  putStrLn "Harmony v\{appVersion}"
