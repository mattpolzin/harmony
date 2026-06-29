module Commands.Label.Test

import Util.ShellCompletion
import Commands.Label

import Data.String

namespace UnslugifyLabel
  test1 : unslugifyLabel ["hello", "world"] "hello" = "hello"
  test1 = Refl

  test2 : unslugifyLabel ["hello", "world"] "#world" = "world"
  test2 = Refl

  test3 : unslugifyLabel ["hello", "world"] "\\#hello" = "hello"
  test3 = Refl

  test4 : unslugifyLabel ["hello world"] "hello world" = "hello world"
  test4 = Refl

  test5 : unslugifyLabel ["hello world"] "#hello world" = "hello world"
  test5 = Refl

  test6 : unslugifyLabel ["hello world"] "\\#hello world" = "hello world"
  test6 = Refl

  test7 : unslugifyLabel ["hello world"] "hello◌world" = "hello world"
  test7 = Refl

  test8 : unslugifyLabel ["hello world"] "#hello◌world" = "hello world"
  test8 = Refl

  test9 : unslugifyLabel ["hello world"] "\\#hello◌world" = "hello world"
  test9 = Refl
