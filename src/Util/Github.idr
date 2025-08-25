module Util.Github

||| If possible, extract a Github issue number from the given string.
|||
||| For example, in "feature/1234/do-a-thing" `parseGithubIssueNumber` will give
||| you "1234"
export
parseGithubIssueNumber : String -> Maybe String
parseGithubIssueNumber = map (pack . reverse) . guardSuccess . foldl go (Start, []) . unpack
  where
    data GHPrefix = G | H | Dash
    data Part = Start | StartOver | PreSlash | Prefix GHPrefix | Number | PostSlash

    startOver : (Part, List Char)
    startOver = (StartOver, [])

    guardSuccess : (Part, List Char) -> Maybe (List Char)
    guardSuccess (PostSlash, y) = Just y
    guardSuccess (Number, y) = Just y
    guardSuccess _ = Nothing

    goPrefix : (GHPrefix, List Char) -> Char -> (Part, List Char)
    goPrefix (G, cs) 'H'  = (Prefix H, cs)
    goPrefix (H, cs) '-'  = (Prefix Dash, cs)
    goPrefix (Dash, cs) c = if isDigit c
                               then (Number, c :: cs) 
                               else startOver
    goPrefix _ _ = startOver

    go : (Part, List Char) -> Char -> (Part, List Char)
      -- start off looking for integer characters or a forward slash or the GH- prefix.
    go (Start, cs) '/' = (PreSlash, cs)
    go (Start, cs) 'G' = (Prefix G, cs)
    go (Start, cs) c   = if isDigit c
                            then (Number, c :: cs) 
                            else startOver

      -- if a prefix is found, try to finish it off
    go (Prefix p, cs) c = goPrefix (p, cs) c

      -- if you've found a forward slash, check for integers next.
    go (PreSlash, cs) 'G' = (Prefix G, cs)
    go (PreSlash, cs) c   = if isDigit c
                               then (Number, c :: cs) 
                               else startOver

      -- if you've started over, look for a slash or prefix.
    go (StartOver, cs) '/' = (PreSlash, cs) 
    go (StartOver, cs) _   = startOver

      -- if you've found integer characters, keep an eye out for a slash.
    go (Number , cs) '/' = (PostSlash, cs)
      -- and if you find anything but an integer character or a slash, start over.
    go (Number , cs) c = if isDigit c 
                            then (Number, c :: cs)
                            else startOver

      -- once we are done, we just ignore the remaining characters.
    go (PostSlash, cs) c   = (PostSlash, cs)

