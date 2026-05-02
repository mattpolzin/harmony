module ShellCompletion.Util

import Data.CompletionStyle
import Data.String

%default total

export
stringify : (s : CompletionStyle) => CompletionResult @{s} -> String
stringify @{Cmds} name = name
stringify @{CmdsAndDescriptions} (name, desc) = "\{name}:\{desc}"

export
stringify' : (s : CompletionStyle) => List CompletionResult -> List String
stringify' = map stringify

||| Turn spaces into '+' so that multi-word phrases can be used with tab-completion.
export
slugify : String -> String
slugify = pack . replaceOn ' ' '◌' . unpack

||| Take a slugified phrase and undo the transformation to get the original phrase back.
public export
unslugify : String -> String
unslugify = pack . replaceOn '◌' ' ' . unpack

public export
hashify : String -> String
hashify = strCons '#'

public export
unhashify : String -> String
unhashify str = case strM str of
                     StrNil => ""
                     (StrCons '#' str') => str'
                     (StrCons '\\' str') =>
                       case strM str' of
                            StrNil => str
                            (StrCons '#' str'') => str''
                            (StrCons _ _) => str
                     (StrCons _ _) => str

public export
isPrefixOf : (s : CompletionStyle) => String -> CompletionResult -> Bool
isPrefixOf str = isPrefixOf str . name

export
matches : (s : CompletionStyle) => String -> CompletionResult -> Bool
matches str = (== str) . name

export
doesNotMatch : (s : CompletionStyle) => String -> CompletionResult -> Bool
doesNotMatch str = (/= str) . name

export
withPrefix : (s : CompletionStyle) => (prefixStr : String) -> List (CompletionResult @{s}) -> List String
withPrefix prefixStr = stringify' . filter (isPrefixOf prefixStr)

export
someWithPrefix : (s : CompletionStyle) => (prefixStr : String) -> List (CompletionResult @{s}) -> Maybe (List String)
someWithPrefix prefixStr = Just . stringify' . filter (isPrefixOf prefixStr)

||| Like someWithPrefix except returns Nothing if it would have otherwise
||| returned Just [].
export
someWithPrefixOrNothing : (s : CompletionStyle) => (prefixStr : String) -> List (CompletionResult @{s}) -> Maybe (List String)
someWithPrefixOrNothing prefixStr opts =
  case (stringify' $ filter (isPrefixOf prefixStr) opts) of
       [] => Nothing
       opts => Just opts

export
all : (s : CompletionStyle) => List (CompletionResult @{s}) -> Maybe (List String)
all = Just . stringify'

export
someFrom : (s : CompletionStyle) => List String -> List (CompletionResult @{s}) -> Maybe (List String)
someFrom opts = Just . stringify' . filter (\o => isInfixOfBy (flip matches) [o] opts)
