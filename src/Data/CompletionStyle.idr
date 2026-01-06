module Data.CompletionStyle

%default total

public export
data CompletionStyle = Cmds | CmdsAndDescriptions

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
