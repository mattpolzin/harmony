module Commands.User

import Commands.PullRequest

import Data.Config
import Data.Date
import Data.List
import Data.Promise
import Data.PullRequest
import Data.Review
import Data.String
import Data.User

import FFI.Git
import FFI.GitHub
import Theme
import Util

import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.Terminal

%default total

replicate' : Config => {d,l : _} -> SemanticColor d l -> Nat -> Char -> Doc AnsiStyle
replicate' c n char =
  theme c . pretty $ String.replicate n char

namespace Reflect
  prCount : Fin 101
  prCount = 100

  reviewDetailsCount : Fin 101
  reviewDetailsCount = 25

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
  intro = "Your current pull request summary (out of the past \{show prCount} PRs):"

  parameters (pageWidth, reviews, openReq, closedReq, closedAuth, openAuth : Nat)
    chart : Config =>
            (leftPadding : Nat)
         -> Doc AnsiStyle
    chart leftPadding =
      indent (cast leftPadding) $
             replicate' Completed' reviews    '·'
         <+> replicate' Missed     closedReq  '◦'
         <+> replicate' Pending    openReq    '<'
        <++> pretty "|"
        <++> replicate' Pending    openAuth   '>'
         <+> replicate' Completed' closedAuth '·'

    graph : Config => Doc AnsiStyle
    graph =
      let req      = openReq  + closedReq
          auth     = openAuth + closedAuth
          left     = (foldr max (length leftTitle ) [req + reviews, auth])
          right    = (foldr max (length rightTitle) [req, auth])
          full     = left + right + 3 -- center divider is 3 characters
          centerOffset : Double = ((cast pageWidth) / 2) - ((cast full) / 2)
          padTitle = (cast centerOffset) + (left `minus` (length leftTitle))
          padChart = (cast centerOffset) + (left `minus` (req + reviews))
      in  vsep [header padTitle, chart padChart]

    parameters (mostRecentReview, earliestOpenAuth, earliestOpenReq : Maybe Date)
      details : Doc AnsiStyle
      details =
        vsep [
          pretty intro
        , emptyDoc
        , annotate underline $ pretty "Requested Reviews:"
        , indent 2 . vsep $ catMaybes [
            Just $ pretty "unreviewed:"
          , Just $ indent 2 . vsep $ catMaybes [
              Just $ hsep [pretty "open:", annotate (color Yellow) $ pretty openReq]
            , earliestOpenReq <&> \date => indent 2 $ hsep [pretty "earliest:", pretty $ show date]
            , Just $ hsep [pretty "closed:", annotate (color Red) $ pretty closedReq]
            ]
          , Just $ hsep [pretty "reviews*:", annotate (color Green) $ pretty reviews]
          , mostRecentReview <&> \date => indent 2 $ hsep [pretty "most recent:", pretty $ show date]
          ]
        , emptyDoc
        , annotate underline $ pretty "Authored Pulls:"
        , indent 2 . vsep $ catMaybes [
            Just $ hsep [pretty "open:", annotate (color Yellow) $ pretty openAuth]
          , earliestOpenAuth <&> \date => indent 2 $ hsep [pretty "earliest:", pretty $ show date]
          , Just $ hsep [pretty "closed:", annotate (color Green) $ pretty closedAuth]
          ]
        , emptyDoc
        , annotate italic $ pretty "*review count (not PR count) of most recent \{show reviewDetailsCount} PRs."
        ]

      print : Config => Doc AnsiStyle
      print = vsep [
                emptyDoc
              , graph
              , emptyDoc
              , details
              , emptyDoc
              ]

  ||| Print information about the currently authenticated user's recent Pull Request
  ||| history. What review requests they have addressed or not, how many PRs they 
  ||| have waiting for review, how many have been closed recently, etc.
  export
  reflectOnSelf : Config => Octokit =>
                  Promise' ()
  reflectOnSelf = do
    prs     <- listPartitionedPRs prCount {pageBreaks=4}
    myLogin <- login <$> getSelf
    reviews <- reviewsByUser myLogin (take (cast reviewDetailsCount) . reverse . sortBy (compare `on` createdAt) $ combined prs)
    let mostRecentReview = map submittedAt . head' $ sortBy (compare `on` submittedAt) reviews
    let history = tuple prs
    let (openAuthored, closedAuthored) = 
      mapHom (filter ((== myLogin) . author)) history
    let (openRequested, closedRequested) =
      mapHom (filter (any (== myLogin) . reviewers)) history
    let (earliestOpenAuth, earliestOpenReq) =
      mapHom (head' . sort . map createdAt) (openAuthored, openRequested)
    renderIO $
      print (length intro)
            (length reviews)
            (length openRequested)
            (length closedRequested)
            (length closedAuthored)
            (length openAuthored)
            mostRecentReview
            earliestOpenAuth
            earliestOpenReq

namespace Me
  print : Config
       -> (gitEmail : Maybe String)
       -> (githubUser : User)
       -> (githubTeams : List String)
       -> Doc AnsiStyle
  print config gitEmail githubUser githubTeams =
    vsep [
        emptyDoc
      , email
      , emptyDoc
      , fullName
      , login
      , emptyDoc
      , teams
      , emptyDoc
      ]
    where
      ul : String -> Doc AnsiStyle
      ul = annotate underline . pretty

      it : String -> Doc AnsiStyle
      it = annotate italic . pretty

      dataVal : String -> Doc AnsiStyle
      dataVal = theme Data . pretty

      email : Doc AnsiStyle
      email = "Git Email:" <++> case gitEmail of
                                     Just e => dataVal e
                                     Nothing => it "Not set"

      fullName : Doc AnsiStyle
      fullName = "GitHub Name:" <++> dataVal githubUser.name

      login : Doc AnsiStyle
      login = "GitHub Login:" <++> dataVal githubUser.login

      teams : Doc AnsiStyle
      teams = vsep $
                ul "GitHub Teams:" :: (map it githubTeams)

  ||| Print information about the currently authenticated and configured user.
  ||| This includes information that can be retrieved from Git as well as GitHub.
  export
  printInfoOnSelf : Config => Octokit => Git =>
                    Promise' ()
  printInfoOnSelf @{config} = do
    gitEmail <- handleUnsetEmail <$> userEmail
    githubUser <- getSelf
    githubTeams <- sort <$> listMyTeams 
    renderIO $ print config gitEmail githubUser githubTeams
      where
        handleUnsetEmail : String -> Maybe String
        handleUnsetEmail "" = Nothing
        handleUnsetEmail e = Just e

