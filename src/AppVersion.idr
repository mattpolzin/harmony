module AppVersion

%default total

export
appVersion : String
appVersion = "6.3.1"

export
printVersion : HasIO io => io ()
printVersion =
  putStrLn "Harmony v\{appVersion}"
