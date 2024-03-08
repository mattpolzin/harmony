module AppVersion

%default total

export
appVersion : String
appVersion = "4.1.0"

export
printVersion : HasIO io => io ()
printVersion =
  putStrLn "Harmony v\{appVersion}"
