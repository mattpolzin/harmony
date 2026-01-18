module Util

import Data.Config
import Data.Fuel
import Data.List
import Data.Promise
import Data.String
import Data.So

import System
import System.File
import System.Git

import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.Terminal

%default total

namespace String
  public export
  data NonEmpty : String -> Type where
    IsNonEmpty : (cs : String) -> So (cs /= "") => NonEmpty cs

  export
  value : String.NonEmpty _ -> String
  value (IsNonEmpty cs) = cs

  -- We implement this with believe_me even though it can be implemented with
  -- `choose` because doing it this way and public exporting it allows us to
  -- reduce expressions containing `nonEmpty` at compile time.
  public export
  nonEmpty : (cs : String) -> Maybe (NonEmpty cs)
  nonEmpty "" = Nothing
  nonEmpty cs = Just $ IsNonEmpty cs @{believe_me Oh}

||| Run the given applicative when the input is @Nothing@.
||| The dual of @whenJust@.
export
whenNothing : Applicative f => Maybe a -> f () -> f ()
whenNothing Nothing x = x
whenNothing (Just _) _ = pure ()

minimumLayoutWidth : Nat
minimumLayoutWidth = 40

export
maximumLayoutWidth : Nat
maximumLayoutWidth = 80

export
optionsWithBestWidth : (terminalColumns : Nat) -> LayoutOptions
optionsWithBestWidth terminalColumns =
  let clipLower = max minimumLayoutWidth terminalColumns
      clipped = min clipLower maximumLayoutWidth
  in
  MkLayoutOptions $
    AvailablePerLine (cast clipped) 1

layoutOptions : Config => LayoutOptions
layoutOptions @{config} = optionsWithBestWidth config.columns

colorize : Config => Doc AnsiStyle -> Doc AnsiStyle
colorize @{config} = if config.colors then id else unAnnotate

||| Render with or without color based on configuration
export
renderString : Config => Doc AnsiStyle -> String
renderString @{config} =
  renderString . layoutPretty layoutOptions . colorize

||| Render with or without color based on configuration
export
renderIO : Config => HasIO io => Doc AnsiStyle -> io ()
renderIO @{config} =
  liftIO . Terminal.renderIO . layoutPretty layoutOptions . colorize

||| Get lines from stdin until either the Fuel runs out or
||| two empty lines are encountered.
export
getManyLines : HasIO io => Fuel -> io (List String)
getManyLines = getMoreLines []
  where
    getMoreLines : (acc : List String) -> Fuel -> io (List String)
    getMoreLines acc Dry = pure (reverse acc)
    getMoreLines acc (More fuel) =
      do line <- trim <$> getLine
         -- stop collecting lines on second blank line.
         case (acc, line) of
              ("" :: rest, "") => pure (reverse rest)
              _                => getMoreLines (line :: acc) fuel

namespace Prompting
  ||| Ask a question and receive a yes/no response with yes being the default
  ||| unless you specify a `defaultAnswer` argument.
  |||
  ||| You probably want your question String to end with a question mark;
  ||| @yesNoPrompt@ will append "[Y/n]" to the end of your question for you.
  export
  yesNoPrompt : HasIO io => 
                {default True defaultAnswer : Bool}
             -> (question : String)
             -> io Bool
  yesNoPrompt question = do
    putStr "\{question} [\{defaultAnswerStr}] "
    yesUnlessNo . trim <$> getLine
      where
        yesUnlessNo : String -> Bool
        yesUnlessNo answer with (toLower answer)
          _ | "n"    = False
          _ | "no"   = False
          _ | "y"    = True
          _ | "yes"  = True
          _ | _      = defaultAnswer
        
        defaultAnswerStr : String
        defaultAnswerStr = case defaultAnswer of
                                False => "y/N"
                                True => "Y/n"

  ||| Provide a default value to use if the second arg is an empty string. If
  ||| you pass Nothing as the first arg, you are choosing not to provide a
  ||| default.
  export
  orIfEmpty : (fallback : Maybe String) -> (input : String) -> String
  orIfEmpty Nothing  x  = x
  orIfEmpty (Just y) "" = y
  orIfEmpty (Just _) x  = x

  export
  enterForDefaultStr : String -> String
  enterForDefaultStr str = " (ENTER for default: \{str})"

  public export
  data PromptWithoutQuestionMark : String -> Type where
    DoesNotEndInQuestionMark : (prompt : String)
                            -> So (not $ "?" `isSuffixOf` prompt) =>
                               PromptWithoutQuestionMark prompt

  export
  getLineEnterForDefault : HasIO io =>
                           (prompt : String)
                        -> PromptWithoutQuestionMark prompt =>
                           (defaultAnswer : Maybe String)
                        -> io String
  getLineEnterForDefault prompt defaultAnswer = do
    putStrLn "\{prompt}\{defaultStr}?"
    orIfEmpty defaultAnswer . trim <$> getLine
      where
        defaultStr : String
        defaultStr = maybe "" enterForDefaultStr defaultAnswer

  export
  inlineDescription : HasIO io => (promptMsg : String) -> (bodyPrefix : String) -> io String
  inlineDescription promptMsg bodyPrefix = do
    putStrLn promptMsg
    putStrLn bodyPrefix
    unlines . (bodyPrefix ::) <$> getManyLines (limit 100)

  prepareDescriptionFile : HasIO io =>
                           (templateFilePath : Maybe String)
                        -> (bodyPrefix : String)
                        -> (tmpFileName : String)
                        -> io ()
  prepareDescriptionFile templateFilePath bodyPrefix tmpFileName = do
    templateContents <- maybe (pure "") readTemplate templateFilePath
    let prefilledDescription = "\{bodyPrefix}\n\{templateContents}"
    ignore $ writeFile tmpFileName prefilledDescription

    where
      readTemplate : String -> io String
      readTemplate path =
        case !(exists path) of
             False => pure ""
             True => case !(readFilePage 0 (limit 5000) path) of
                          Left err => pure ""
                          Right (_, lines) => pure $ join "" lines

  ||| Get a description from the user via their preferred EDITOR
  export
  editorDescription : HasIO io => 
                      (editor : String)
                   -> (templateFilePath : Maybe String)
                   -> (bodyPrefix : String)
                   -> io (Either FileError String)
  editorDescription editor templateFilePath bodyPrefix = do
    let tmpFileName = "editor_description.tmp.md"
    prepareDescriptionFile templateFilePath bodyPrefix tmpFileName
    0 <- system "\{editor} \{tmpFileName}"
      | e => pure (Left $ GenericFileError e)
    description <- assert_total $ readFile tmpFileName 
    --              ^ ignore the possibility that an infinte file was
    --                produced.
    when !(exists tmpFileName) $
      ignore $ removeFile tmpFileName
    pure description

||| Get an absolute path for the given directory or file assuming the
||| given path is relative to the root of the Git repository.
export
relativeToRoot : String -> Promise' String
relativeToRoot path = rootDir <&> (++ "/\{path}")
