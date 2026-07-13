module Util.Prompting

import Data.Config
import Data.Fuel
import Data.String

import System
import System.File

import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Util
import Text.PrettyPrint.Prettyprinter.Render.Terminal

import Util

%default total

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
public export
orIfEmpty : (fallback : Maybe String) -> (input : String) -> String
orIfEmpty Nothing  x  = x
orIfEmpty (Just y) "" = y
orIfEmpty (Just _) x  = x

||| Wait for the user to hit the ENTER key and then continue.
|||
||| The description you pass in should be what to expect when enter is
||| pressed. For example, "continue".
export
waitForEnter : HasIO io => String -> io ()
waitForEnter description = do
  putStr "Hit ENTER to \{description} (CTRL+c to abort)"
  ignore getLine

export
enterForDefaultStr : String -> String
enterForDefaultStr str = " (ENTER for default: \{str})"

enterForDefaultPrompt : (prompt : String)
                     -> (defaultAnswer : Maybe String)
                     -> String
enterForDefaultPrompt prompt defaultAnswer =
  "\{prompt}\{defaultStr}"
    where
      defaultStr : String
      defaultStr = maybe "" enterForDefaultStr defaultAnswer

namespace TestEnterForDefaultPrompt
  withoutDefault : enterForDefaultPrompt "prompt?" Nothing === "prompt?"
  withoutDefault = Refl

  withDefault : enterForDefaultPrompt "prompt?" (Just "default") === "prompt? (ENTER for default: default)"
  withDefault = Refl

export
getLineEnterForDefault : HasIO io =>
                         (prompt : String)
                      -> (defaultAnswer : Maybe String)
                      -> io String
getLineEnterForDefault prompt defaultAnswer = do
  putStrLn (enterForDefaultPrompt prompt defaultAnswer)
  orIfEmpty defaultAnswer . trim <$> getLine
export
offerRetry : HasIO io =>
             (fallbackDescription : String)
          -> (failureDescription : String)
          -> (fallback : Lazy a)
          -> io (Maybe a)
          -> io a
offerRetry fallbackDescription failureDescription fallback p = do
  Nothing <- p
    | Just first => pure first
  putStrLn fallbackDescription
  Nothing <- p
    | Just second => pure second
  putStrLn failureDescription
  pure fallback

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

export
printWarning : HasIO io => 
               String 
            -> io ()
printWarning warning =
  if !(isTTY stderr)
    then do
      ignore $ fPutStrLn stderr . renderString . layoutPretty defaultLayoutOptions . annotate (color Yellow) . pretty $ trim warning
      ignore $ fPutStrLn stderr ""
    else do
      ignore $ fPutStrLn stderr warning
      ignore $ fPutStrLn stderr ""

export
printImportant : HasIO io => 
                 Config =>
                 String 
              -> io ()
printImportant message = do
  renderIO $
    hsep [ annotate (color Yellow) "IMPORTANT:"
         , reflow message
         ]
