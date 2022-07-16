module AppVersion

export
appVersion : String
appVersion = "1.0.0"

export
printVersion : HasIO io => io ()
printVersion =
  putStrLn "Harmony v\{appVersion}"
