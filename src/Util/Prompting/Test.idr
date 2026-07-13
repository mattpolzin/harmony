module Util.Prompting.Test

import Util.Prompting

namespace OrIfEmpty
  noFallbackEmpty : orIfEmpty Nothing "" === ""
  noFallbackEmpty = Refl

  noFallbackNotEmpty : orIfEmpty Nothing "hi" === "hi"
  noFallbackNotEmpty = Refl

  fallbackEmpty : orIfEmpty (Just "hi") "" === "hi"
  fallbackEmpty = Refl

  fallbackNotEmpty : orIfEmpty (Just "hi") "hello" === "hello"
  fallbackNotEmpty = Refl
