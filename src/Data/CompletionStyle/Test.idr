module Data.CompletionStyle.Test

import Data.CompletionStyle

namespace EscapeColons
  emptyString : escapeColons "" === ""
  emptyString = Refl

  noColons : escapeColons "hello world" === "hello world"
  noColons = Refl

  colon : escapeColons "Hello: A World Story" === "Hello\\: A World Story"
  colon = Refl

