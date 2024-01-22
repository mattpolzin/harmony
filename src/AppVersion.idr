module AppVersion

%default total

export
appVersion : String
appVersion = "4.0.1"

export
printVersion : HasIO io => io ()
printVersion =
  putStrLn "Harmony v\{appVersion}"
