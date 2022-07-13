module AppVersion

export
appVersion : String
appVersion = "0.6.0"

export
printVersion : HasIO io => io ()
printVersion =
  putStrLn "Harmony v\{appVersion}"
