module Util.Github

||| If possible, extract a Github issue number from the given string.
|||
||| For example, in "feature/1234/do-a-thing" `parseGithubIssueNumber` will give
||| you "1234"
export
parseGithubIssueNumber : String -> Maybe String
parseGithubIssueNumber = map (pack . reverse) . guardSuccess . foldl go (Start, []) . unpack
  where
    data Part = Start | StartOver | PreSlash | Number | PostSlash | End

    startOver : (Part, List Char)
    startOver = (StartOver, [])

    guardSuccess : (Part, List Char) -> Maybe (List Char)
    guardSuccess (PostSlash, y) = Just y
    guardSuccess (End, y) = Just y
    guardSuccess _ = Nothing

    go : (Part, List Char) -> Char -> (Part, List Char)
      -- start off looking for integer characters or a forward slash.
    go (Start, cs) '/' = (PreSlash, cs)
    go (Start, cs) c   = if isDigit c
                            then (Number, c :: cs) 
                            else startOver

      -- if you've found a forward slash, check for integers next.
    go (PreSlash, cs) c = if isDigit c
                             then (Number, c :: cs) 
                             else startOver

      -- if you've started over, look for a slash.
    go (StartOver, cs) '/' = (PreSlash, cs) 
    go (StartOver, cs) _ = startOver

      -- if you've found integer characters, keep an eye out for a slash.
    go (Number , cs) '/' = (PostSlash, cs)
      -- and if you find anything but an integer character or a slash, start over.
    go (Number , cs) c = if isDigit c 
                            then (Number, c :: cs)
                            else startOver

      -- once we are done, we just ignore the remaining characters.
    go (End      , cs) c   = (End, cs)
    go (PostSlash, cs) c   = (PostSlash, cs)

