module AppVersion

%default total

export
appVersion : String
appVersion = "5.5.1"

export
printVersion : HasIO io => io ()
printVersion =
  putStrLn "Harmony v\{appVersion}"
