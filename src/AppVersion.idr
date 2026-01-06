module AppVersion

%default total

export
appVersion : String
appVersion = "6.1.0"

export
printVersion : HasIO io => io ()
printVersion =
  putStrLn "Harmony v\{appVersion}"
