module AppVersion

%default total

export
appVersion : String
appVersion = "5.2.0"

export
printVersion : HasIO io => io ()
printVersion =
  putStrLn "Harmony v\{appVersion}"
