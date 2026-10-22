module AppVersion

%default total

export
appVersion : String
appVersion = "7.11.1"

export
printVersion : HasIO io => (long : Bool) -> io ()
printVersion True =
  putStrLn "Harmony v\{appVersion}"
printVersion False =
  putStrLn appVersion
