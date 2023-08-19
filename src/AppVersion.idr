module AppVersion

%default total

export
appVersion : String
appVersion = "2.6.1"

export
printVersion : HasIO io => io ()
printVersion =
  putStrLn "Harmony v\{appVersion}"
