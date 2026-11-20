module AppVersion

%default total

export
appVersion : String
appVersion = "8.0.0"

export
printVersion : HasIO io => (long : Bool) -> io ()
printVersion True =
  putStrLn "Harmony v\{appVersion}"
printVersion False =
  putStrLn appVersion
