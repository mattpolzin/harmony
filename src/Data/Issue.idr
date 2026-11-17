module Data.Issue

%default total

public export
interface IssueRef a where
  ||| Some identifier that is suitable for use in URLs and branch names. Does
  ||| not need to be the thing called "id".
  total
  identifier : a -> String
  ||| A title, headline, summary, or similar.
  total
  title : a -> String

public export
(.identifier) : IssueRef a => a -> String
x.identifier = identifier x

public export
(.title) : IssueRef a => a -> String
x.title = title x

export
IssueRef a => Show a where
  show issue = "[\{issue.identifier}] \{issue.title}"

public export
data AnyIssue : Type where
  MkI : IssueRef ty => ty -> AnyIssue
