module Util.OptionParsing

import Data.List

||| Parse all of a given option and return any remaining unparsed arguments.
||| 
||| @flags@ The flags that can be used to identify this option (e.g. --flag value).
||| @parse@ The function that parses potential values for this option.
||| @args@ The arguments to be parsed.
public export
parseAllOfOpt : (flags : List String) -> (parse : String -> Maybe a) -> (args : List String) -> (List a, List String)
parseAllOfOpt _ _ [] = ([], [])
parseAllOfOpt flags parse (arg :: moreArgs) =
  case find (== arg) flags of
       Nothing   => mapSnd (arg ::) (parseAllOfOpt flags parse moreArgs)
       Just flag => parseOptForFlag flag moreArgs

  where
    parseOpt : (flag : String) -> (maybeOpt : String) -> (rest : List String) -> (List a, List String)
    parseOpt flag maybeOpt rest =
      case parse maybeOpt of
           Just x  => mapFst (x ::) (parseAllOfOpt flags parse rest)
           Nothing => mapSnd (\xs' => flag :: maybeOpt :: xs') (parseAllOfOpt flags parse rest)

    parseOptForFlag : (flag : String) -> (remainingArgs : List String) -> (List a, List String)
    parseOptForFlag flag remainingArgs =
      case remainingArgs of
           []            => ([], [flag])
           (opt :: rest) => parseOpt flag opt rest

||| Chain alternative parsers of the shape (String -> Maybe a) together.
public export
(<||>) : Alternative t => (a -> t b) -> (a -> t b) -> a -> t b
(<||>) f g x = f x <|> g x

export infixr 2 <||>
