module AppVersion

export
appVersion : String
appVersion = "0.7.1"

export
printVersion : HasIO io => io ()
printVersion =
  putStrLn "Harmony v\{appVersion}"
