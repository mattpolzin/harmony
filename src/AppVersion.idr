module AppVersion

%default total

export
appVersion : String
appVersion = "2.4.1"

export
printVersion : HasIO io => io ()
printVersion =
  putStrLn "Harmony v\{appVersion}"
