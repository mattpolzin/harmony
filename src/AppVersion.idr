module AppVersion

export
appVersion : String
appVersion = "1.1.1"

export
printVersion : HasIO io => io ()
printVersion =
  putStrLn "Harmony v\{appVersion}"
