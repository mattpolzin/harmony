module Theme

import Data.Config
import Data.Theme
import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.Terminal

%default total

record Colors where
  constructor MkCs
  foreground : Maybe Color
  background : Maybe Color

cs : List Color -> Colors
cs [foreground] = MkCs (Just foreground) Nothing
cs [foreground, background] = MkCs (Just foreground) (Just background)
cs _ = MkCs Nothing Nothing

public export
data SemanticColor : Colors -> Colors -> Type where
  Good      : SemanticColor (cs [Green ]) (cs [Green])
  NotGreat  : SemanticColor (cs [Yellow]) (cs [Black])
  Bad       : SemanticColor (cs [Red   ]) (cs [Red])
  Completed : SemanticColor (cs [Green ]) (cs [Green])
  Pending   : SemanticColor (cs [Yellow]) (cs [Black])
  Missed    : SemanticColor (cs [Red   ]) (cs [Red])

public export
theme : Config => {d, l : _} -> SemanticColor d l -> Doc AnsiStyle -> Doc AnsiStyle
theme @{config} = go configTheme
  where
    configTheme : Theme
    configTheme = maybe Dark id config.theme

    maybeAnnotate : (Color -> AnsiStyle) -> Maybe Color -> Doc AnsiStyle -> Doc AnsiStyle
    maybeAnnotate s c = maybe id (annotate . s) c

    colorsAnn : Colors -> Doc AnsiStyle -> Doc AnsiStyle
    colorsAnn (MkCs fg bg) = maybeAnnotate color fg . maybeAnnotate bgColor bg

    go : Theme -> {dark, light : _} -> SemanticColor dark light -> Doc AnsiStyle -> Doc AnsiStyle
    go Dark _  = colorsAnn dark
    go Light _ = colorsAnn light

