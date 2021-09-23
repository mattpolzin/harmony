module User

import Data.Config
import Data.Date
import Data.List
import Data.Promise
import Data.PullRequest
import Data.String
import Data.User
import FFI.GitHub
import PullRequest
import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.Terminal
import Util

%default total

replicate' : Color -> Nat -> Char -> Doc AnsiStyle
replicate' c n char =
  annotate (color c) (pretty $ String.replicate n char)

namespace Reflect
  leftTitle : String
  leftTitle = "requested"

  rightTitle : String
  rightTitle = "authored"

  header : (leftPadding : Nat)
        -> Doc AnsiStyle
  header leftPadding =
    indent (cast leftPadding) $
      hsep [ital leftTitle, pretty " ", ital rightTitle]
    where
      ital : String -> Doc AnsiStyle
      ital = annotate italic . pretty

  intro : String
  intro = "Your current pull request summary (out of the past 70 PRs):"

  parameters (pageWidth : Nat, openReq : Nat, closedReq : Nat, closedAuth : Nat, openAuth : Nat)
    chart : (leftPadding : Nat)
         -> Doc AnsiStyle
    chart leftPadding =
      indent (cast leftPadding) $
             replicate' Green  closedReq  '·'
         <+> replicate' Yellow openReq    '<'
        <++> pretty "|"
        <++> replicate' Yellow openAuth   '>'
         <+> replicate' Green  closedAuth '·'

    graph : Doc AnsiStyle
    graph =
      let req      = openReq  + closedReq
          auth     = openAuth + closedAuth
          left     = (foldr max (length leftTitle ) [req, auth])
          right    = (foldr max (length rightTitle) [req, auth])
          full     = left + right + 3 -- center divider is 3 characters
          centerOffset : Double = ((cast pageWidth) / 2) - ((cast full) / 2)
          padTitle = (cast centerOffset) + (left `minus` (length leftTitle))
          padChart = (cast centerOffset) + (left `minus` req)
      in  vsep [header padTitle, chart padChart]

    parameters (earliestOpenAuth : Maybe Date, earliestOpenReq : Maybe Date)
      details :  Doc AnsiStyle
      details =
        vsep [
          pretty intro
        , pretty "requested: "
        , indent 2 . vsep $ catMaybes [
            Just $ hsep [pretty "open:", pretty openReq]
          , earliestOpenReq <&> \date => indent 2 $ hsep [pretty "earliest:", pretty $ show date]
          , Just $ hsep [pretty "closed:", pretty closedReq]
          ]
        , pretty "authored: "
        , indent 2 . vsep $ catMaybes [
            Just $ hsep [pretty "open:", pretty openAuth]
          , earliestOpenAuth <&> \date => indent 2 $ hsep [pretty "earliest:", pretty $ show date]
          , Just $ hsep [pretty "closed:", pretty closedAuth]
          ]
        ]

      print : Doc AnsiStyle
      print = vsep [
                emptyDoc
              , graph
              , emptyDoc
              , details
              ]

  export
  reflectOnSelf : Config => Octokit =>
                  Promise ()
  reflectOnSelf =
    do history <- tuple <$> listPartitionedPRs 70
       myLogin <- login <$> getSelf
       let (openAuthored, closedAuthored) = 
         mapHom (filter ((== myLogin) . author)) history
       let (openRequested, closedRequested) =
         mapHom (filter (any (== myLogin) . reviewers)) history
       let (earliestOpenAuth, earliestOpenReq) =
         mapHom (head' . sort . map createdAt) (openAuthored, openRequested)
       -- TODO: get Terminal width from somewhere to set the page width
       --       to the min of the Terminal width or the intro length.
       putStrLn . renderString $
         print (length intro)
               (length openRequested)
               (length closedRequested)
               (length closedAuthored)
               (length openAuthored)
               earliestOpenAuth
               earliestOpenReq

