module Commands.Graph

import Data.Config
import Data.Date
import Data.Fuel
import Data.List
import Data.Nat
import Data.String
import Data.PullRequest
import Data.ReviewScore
import Data.SortedMap

import Theme

import public Text.PrettyPrint.Prettyprinter
import public Text.PrettyPrint.Prettyprinter.Render.Terminal
import public Text.PrettyPrint.Prettyprinter.Symbols
import public Text.PrettyPrint.Prettyprinter.Util
import public Text.PrettyPrint.Prettyprinter.Doc

%default total

interface HasLength s where
  length : s -> Nat

export
HasLength String where
  length = String.length

interface Graphable g where
  totalWidth : g -> Nat
  label : g -> (maxLabelLength : Nat) -> Doc AnsiStyle
  labelLength : g -> Nat
  ||| The score represents the total positive impact.
  score : g -> Nat
  ||| The detractor is a quantity that gets overlaid on top of the score to indicate
  ||| a negative impact.
  detractor : g -> Nat
  ||| The bonus indicates some measure that is not always represented but when it is,
  ||| it has additive impact beyond the score.
  bonus : g -> Nat

data AugmentedReviewScore : login -> Type where
  Augmented : ReviewScore login -> (bonus : Nat) -> AugmentedReviewScore login

countPadRight : HasLength s => (rightJustifyWithin : Nat) -> s -> Nat
countPadRight rightJustifyWithin x =
  let l = length x
  in  rightJustifyWithin `minus` l

HasLength login => Pretty login => Graphable (AugmentedReviewScore login) where
  totalWidth  (Augmented x sbonus)     = x.partialScore
  label       (Augmented x _     ) max = (annotate italic $ pretty x.user) <+> (pretty $ String.replicate (countPadRight max x.user) ' ')
  labelLength (Augmented x _     )     = length x.user
  score       (Augmented x _     )     = x.combinedScore
  detractor   (Augmented x _     )     = x.partialScore `minus` x.combinedScore
  bonus       (Augmented _ sbonus)     = sbonus

record PRsOnDate dateTy where
  constructor MkPRsOnDate
  date : dateTy
  prCount : Nat

Pretty Date where
  pretty = pretty . showYearAndMonth

export
HasLength Date where
  length = String.length . showYearAndMonth

(.countStr) : PRsOnDate dateTy -> String
g.countStr = "(\{show g.prCount})"

parameters (config : Config)
  -- Make the PR count on each date graphable for the
  -- health command's graph.
  HasLength dateTy => Pretty dateTy => Graphable (PRsOnDate dateTy) where
    totalWidth g = g.prCount
    labelLength g = length g.date + 1 + String.length g.countStr
    label g max = coloredLabel <++> countInParens
      where
        coloredLabel : Doc AnsiStyle
        coloredLabel = if g.prCount == 0
                          then (theme Good $ pretty g.date)
                          else if g.prCount < 2
                                  then pretty g.date
                                  else if g.prCount < 6
                                      then (theme NotGreat $ pretty g.date)
                                      else (theme Bad $ pretty g.date)

        countInParens : Doc AnsiStyle
        countInParens = if g.prCount > 4
                           then (annotate italic $ pretty g.countStr)
                           else pretty ""
    score g = g.prCount
    detractor _ = 0
    bonus _ = 0

  ||| Graph a single line (bar) of dots.
  ||| @ indentation a number of leading spaces to product off to the left (uses Doc's @indent@)
  ||| @ score the net score to graph out in yellow.
  ||| @ detractor the amount detracting from the score, graphed in red.
  ||| @ bonus a bonus indicator graphed on the far right in green.
  bar : (indentation : Nat) -> (score : Nat) -> (detractor : Nat) -> (bonus : Nat) -> Doc AnsiStyle
  bar idt score detractor bonus = indent (cast idt) . hcat $
                                [ theme Missed . hcat $ replicate detractor (pretty '◦')
                                , theme Pending' . hcat $ replicate score (pretty '·')
                                , theme Completed . hcat $ replicate bonus (pretty '▪')
                                ]

  graphOne : Graphable g => (highScore : Nat) -> (maxBonus : Nat) -> (maxLabelLength : Nat) -> g -> Doc AnsiStyle
  graphOne highScore maxBonus maxLabelLength graph =
        -- we create a bar with the combinedScore and then fill in any
        -- remaining space with an indication of the detractor. We cap
        -- the detractor representation at the high score to make everything
        -- line up nicely. The detractor is just there to give some indication
        -- of review requests that did not count positively toward the score.
    let idt = highScore `minus` (totalWidth graph)
        remainingSpace = highScore `minus` (score graph)
    in bar idt (score graph) (min remainingSpace (detractor graph)) 0 <++> (label graph maxLabelLength) <++> bar 0 0 0 (bonus graph)

  graph : Graphable g => (highScore : Nat) -> (maxBonus : Nat) -> (maxLabelLength : Nat) -> List g -> Doc AnsiStyle
  graph highScore maxBonus maxLabelLength = vsep . map (graphOne highScore maxBonus maxLabelLength)

||| Produce a graph of open pull requests per month (by the month the PR was created).
export
healthGraph : Config =>
              (openPullRequests : List PullRequest)
           -> (org : String)
           -> (repo : String)
           -> Doc AnsiStyle
healthGraph @{config} openPullRequests org repo =
  let groups = groupBy ((==) `on` .month `on` .createdAt) $ sortBy (compare `on` .createdAt) openPullRequests
      max    = foldr (\xs,m => max (length xs) m) 1 groups
  in vsep [ header
          , graph config max 0 0 (unfoldGraph (limit 48) groups Nothing)
          , emptyDoc
          , pretty link
          , emptyDoc
          ]
  where
    link : String
    link = "https://github.com/\{org}/\{repo}/pulls?q=is%3Apr+is%3Aopen+sort%3Acreated-asc"

    graphable : (List1 PullRequest) -> PRsOnDate Date
    graphable (pr ::: tail) = MkPRsOnDate pr.createdAt (S $ length tail)

    unfoldGraph : Fuel -> List (List1 PullRequest) -> (acc : Maybe (Date, List1 (PRsOnDate Date))) -> List (PRsOnDate Date)
    unfoldGraph Dry _ Nothing = []
    unfoldGraph Dry _ (Just (x, ys)) = forget ys
    unfoldGraph _ [] Nothing = []
    unfoldGraph _ [] (Just (x, ys)) = forget ys
    unfoldGraph (More fuel) (next@(nextOne ::: tail) :: xs) Nothing =
      unfoldGraph fuel xs (Just (nextOne.createdAt, (graphable next) ::: []))
    unfoldGraph (More fuel) (next@(nextOne ::: tail) :: xs) (Just (prevDate, acc)) =
      if (nextMonth prevDate.month) == nextOne.createdAt.month
         then insertNext
         else insertPlaceholder
      where
        insertNext : List (PRsOnDate Date)
        insertNext =
          unfoldGraph fuel xs (Just (nextOne.createdAt, graphable next ::: forget acc))

        insertPlaceholder : List (PRsOnDate Date)
        insertPlaceholder =
          let placeholderDate : Date = { month $= nextMonth } prevDate
              placeholder = MkPRsOnDate placeholderDate 0
          in unfoldGraph fuel (next :: xs) (Just (placeholderDate, placeholder ::: forget acc))

    header : Doc AnsiStyle
    header = vsep $
               catMaybes [ Just $ emptyDoc
                         , Just $ pretty "Open Pull Requests grouped by month created."
                         , Just $ emptyDoc
                         , Just $ emptyDoc
                         ]

||| Produce a graph of relative review workload for developers.
||| @ closedReviews    The logins of each reviewer of each closed PR (duplicates intact).
||| @ openReviews      The logins of each reviewer of each open PR (duplicates intact).
||| @ candidates       The logins of all potential reviewers that should be considered.
||| @ completedReviews Optionally pass a map from login to count of completed reviews to
|||                    graph as well.
export
reviewsGraph : Config => HasLength login => Ord login => Pretty login =>
               (closedReviews : List login)
            -> (openReviews : List login)
            -> (candidates : List login)
            -> (completedReviews : Maybe (SortedMap login Nat))
            -> Doc AnsiStyle
reviewsGraph @{config} closedReviews openReviews candidates completedReviews =
  let scoredOptions = reverse $ scoredReviewers closedReviews openReviews (sort $ nub candidates)
      augmentedOptions : List (AugmentedReviewScore login) = 
        case completedReviews of
             Nothing        => (flip Augmented 0) <$> scoredOptions
             Just completed => augment completed <$> scoredOptions
      maxBonus = maybe 0 id (maxValue . SortedMap.toList <$> completedReviews)
      maxLoginLength = foldr (maximum . labelLength) 0 augmentedOptions
  in  case scoredOptions of
           [] => emptyDoc
           ((MkScore _ s c) :: _) =>
             let highScore = c + (s `minus` c)
             in  vsep [ header 
                      , graph config (if highScore > 0 then highScore else 1) maxBonus maxLoginLength augmentedOptions
                      , footer
                      ]
  where
    pendingDot : Doc AnsiStyle
    pendingDot = theme Pending' $ pretty "·"

    missedDot : Doc AnsiStyle
    missedDot = theme Missed $ pretty "◦"

    completedBox : Doc AnsiStyle
    completedBox = theme Completed $ pretty "▪"

    header : Doc AnsiStyle
    header = vsep $
               catMaybes [ Just $ emptyDoc
                         , Just $ pretty "Weighted review workload."
                         , Just $ pretty "4x the number of open review requests" <++> parens pendingDot
                         , Just $ pretty "1x the number of closed PRs with unanswered review requests" <++> parens missedDot
                         , if (null completedReviews) then Nothing else Just $ pretty "1x the number of completed reviews" <++> parens completedBox
                         , Just $ parens $ missedDot <++> pretty "overlayed on" <++> pendingDot
                         , Just $ emptyDoc
                         ]

    note : Doc AnsiStyle -> Doc AnsiStyle
    note doc = (annotate bold "Note:") <++> doc

    footer : Doc AnsiStyle
    footer = vsep 
      [ emptyDoc
      , note $ reflow """
        It is a strongly held opinion of Harmony that the above graph is not a measure of each developers' productivity.
        It is a proxy for each developers' existing PR review workload that may help to understand how Harmony is
        choosing reviewers or similarly help one developer decide which other developers have capacity to help review
        their work.
        """
      , emptyDoc
      ]

    augment : (completedReviews : SortedMap login Nat) -> ReviewScore login -> AugmentedReviewScore login
    augment completed score =
      Augmented score (maybe 0 id $ lookup score.user completed)

    maxValue : List (a, Nat) -> Nat
    maxValue [] = 0
    maxValue ((x, y) :: xs) = max y (maxValue xs)

