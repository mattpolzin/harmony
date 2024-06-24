module Theme

import Data.Theme
import Text.PrettyPrint.Prettyprinter.Render.Terminal

%default total

public export
data SemanticColor = Good
                   | NotGreat
                   | Bad
                   | Completed
                   | Pending
                   | Missed

%inline
public export
color : SemanticColor -> AnsiStyle
color = Terminal.color . go
  where
    go : SemanticColor -> Color
    go Good     = Green
    go NotGreat = Yellow
    go Bad      = Red

    go Completed = Green
    go Pending   = Yellow
    go Missed    = Red

