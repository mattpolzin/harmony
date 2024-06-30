module Data.Theme

%default total

public export
data Theme = Light
           | Dark

export
Show Theme where
  show Light = "light"
  show Dark = "dark"

export
parseString : String -> Maybe Theme
parseString "light" = Just Light
parseString "dark" = Just Dark
parseString _ = Nothing
