module AppVersion

%default total

export
appVersion : String
appVersion = "1.2.0"

export
printVersion : HasIO io => io ()
printVersion =
  putStrLn "Harmony v\{appVersion}"
