module Data.CompletionStyle

import Data.String
import Data.String.Extra

%default total

export
data CompletionStyle = Cmds | CmdsAndDescriptions

public export
Cmd : CompletionStyle
Cmd = Cmds

public export
CmdAndDescription : CompletionStyle
CmdAndDescription = CmdsAndDescriptions

public export
CompletionResult : CompletionStyle => Type
CompletionResult @{Cmds} = String
CompletionResult @{CmdsAndDescriptions} = (String, String)

public export
completionResult : (s : CompletionStyle) => (String, String) -> CompletionResult
completionResult @{Cmds} (n, _) = n
completionResult @{CmdsAndDescriptions} (n, d) = (n, d)

public export
name : (s : CompletionStyle) => CompletionResult @{s} -> String
name @{Cmds} n = n
name @{CmdsAndDescriptions} (n, _) = n

public export
describe : (s : CompletionStyle) => (description : String) -> (name : String) -> CompletionResult @{s}
describe @{Cmds} description name = name
describe @{CmdsAndDescriptions} description name = (name, description)

public export
escapeColons : String -> String
escapeColons = pack . go . unpack
  where
    go : List Char -> List Char
    go [] = []
    go (':' :: xs) = '\\' :: ':' :: go xs
    go (x :: xs) = x :: go xs

public export
stringify : (s : CompletionStyle) => CompletionResult @{s} -> String
stringify @{Cmds} name = name
stringify @{CmdsAndDescriptions} (name, desc) = "\{escapeColons name}:\{desc}"

export
encodeForShell : (s : CompletionStyle) -> List String -> String
encodeForShell Cmds = unlines
encodeForShell CmdsAndDescriptions = join "~"
