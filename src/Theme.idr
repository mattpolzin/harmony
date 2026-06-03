module Theme

import Data.Config
import public Data.Theme
import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.Terminal

%default total

record Colors where
  constructor MkCs
  foreground : Maybe Color
  background : Maybe Color

cs : (colors : List Color) -> Either (length colors === 1) (length colors === 2) => Colors
cs [foreground] = MkCs (Just foreground) Nothing
cs [foreground, background] = MkCs (Just foreground) (Just background)
cs [] @{(Left Refl)} impossible
cs [] @{(Right Refl)} impossible
cs (_ :: _ :: _ :: _) @{(Left Refl)} impossible
cs (_ :: _ :: _ :: _) @{(Right Refl)} impossible

||| The prime variants where they exist are for situations where the
||| character being drawn is small so a different color choice might
||| be useful for visibility.
public export
data SemanticColor : (darkTheme : Colors) -> (lightTheme : Colors) -> Type where
  -- desirability
  Good       : SemanticColor (cs [Green  ]) (cs [Green])
  NotGreat   : SemanticColor (cs [Yellow ]) (cs [Black])
  Bad        : SemanticColor (cs [Red    ]) (cs [Red])
  -- completion
  Completed  : SemanticColor (cs [Green  ]) (cs [Green])
  Completed' : SemanticColor (cs [Green  ]) (cs [Black])
  Pending    : SemanticColor (cs [Yellow ]) (cs [Yellow])
  Pending'   : SemanticColor (cs [Yellow ]) (cs [Black])
  Missed     : SemanticColor (cs [Red    ]) (cs [Red])
  -- categorization
  Data       : SemanticColor (cs [Green  ]) (cs [Blue])
  Special    : SemanticColor (cs [Magenta]) (cs [Magenta])
  -- marking
  Current    : SemanticColor (cs [Green  ]) (cs [Green])

public export
theme' : Theme => {d, l : _} -> SemanticColor d l -> Doc AnsiStyle -> Doc AnsiStyle
theme' @{theme} = go theme
  where
    maybeAnnotate : (Color -> AnsiStyle) -> Maybe Color -> Doc AnsiStyle -> Doc AnsiStyle
    maybeAnnotate s c = maybe id (annotate . s) c

    colorsAnn : Colors -> Doc AnsiStyle -> Doc AnsiStyle
    colorsAnn (MkCs fg bg) = maybeAnnotate color fg . maybeAnnotate bgColor bg

    go : Theme -> {dark, light : _} -> SemanticColor dark light -> Doc AnsiStyle -> Doc AnsiStyle
    go Dark _  = colorsAnn dark
    go Light _ = colorsAnn light

public export
theme : Config => {d, l : _} -> SemanticColor d l -> Doc AnsiStyle -> Doc AnsiStyle
theme @{config} = theme' @{config.theme}

