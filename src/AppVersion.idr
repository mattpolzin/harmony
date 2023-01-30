module AppVersion

%default total

export
appVersion : String
appVersion = "2.3.0"

export
printVersion : HasIO io => io ()
printVersion =
  putStrLn "Harmony v\{appVersion}"
