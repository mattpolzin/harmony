module Data.Project

import Data.Date
import Data.Either
import Data.List
import Data.String
import Data.Vect
import JSON.Parser
import Language.JSON.Accessors

%default total

public export
record ProjectRef where
  constructor MkProjectRef
  ||| The project's opaque GraphQL identifier
  graphQlId        : String
  ||| The project's "number" (as seen in URIs referring to the project)
  number           : Integer
  ||| The project's title
  title            : String

export
Show ProjectRef where
  show (MkProjectRef graphQlId number title) = 
    "[\{show number}] \{title}"

export
Eq ProjectRef where
  (MkProjectRef id1 n1 t1) == (MkProjectRef id2 n2 t2) = id1 == id2 && n1 == n2 && t1 == t2

export
Ord ProjectRef where
  (MkProjectRef _ n1 t1) > (MkProjectRef _ n2 t2) = n1 > n2 || (n1 == n2 && t1 > t2)

  (MkProjectRef _ n1 t1) < (MkProjectRef _ n2 t2) = n1 < n2 || (n1 == n2 && t1 < t2)

export
parseProjectRef : JSON -> Either String ProjectRef
parseProjectRef json =
 do projectRef <- object json
    [id, projectNumber, projectTitle] <- lookupAll ["graphQlId", "number", "title"] projectRef
    graphQlId   <- string id
    number      <- integer projectNumber
    title       <- string projectTitle
    pure $ MkProjectRef {
        graphQlId
      , number
      , title
      }

||| Parse a single project reference from a JSON String
export
parseProjectRefString : String -> Either String ProjectRef
parseProjectRefString =
  (mapFst (const "Failed to parse JSON of Project Reference") . parseJSON Virtual) >=> parseProjectRef

||| Parse a list of project references from a JSON String
export
parseProjectRefsString : String -> Either String (List ProjectRef)
parseProjectRefsString =
  (mapFst (const "Failed to parse JSON of Project References") . parseJSON Virtual) >=> array parseProjectRef

export
json : ProjectRef -> JSON
json (MkProjectRef graphQlId number title) = JObject [("graphQlId", JString graphQlId), ("number", JInteger number), ("title", JString title)]

public export
record Project where
  constructor MkProject
  ||| An opaque GraphQL Id for the project.
  graphQlId        : String
  ||| The project's "number" (as seen in URIs referring to the project).
  number           : Integer
  ||| The project's title
  title            : String
  ||| The project's short description
  shortDescription : Maybe String
  ||| When the project was created.
  createdAt        : Date
  ||| The `login` of the creator of the project.
  creator          : String
  ||| Whether the project has been closed.
  closed           : Bool

%name Project prj, prj1, prj2

export
Show Project where
  show (MkProject _ number title _ _ _ _) = 
    "[\{show number}] \{title}"

export
(.reference) : Project -> ProjectRef
p.reference = MkProjectRef p.graphQlId p.number p.title

export
reference : Project -> ProjectRef
reference = (.reference)

export
isCreator : String -> Project -> Bool
isCreator login = (== login) . creator

export
parseProject : JSON -> Either String Project
parseProject json =
 do project <- object json
    [graphQlId, projectNumber, projectTitle, shortDescription, createdAtStr,
    creatorLogin, closed] <- lookupAll ["graphql_id", "number", "title",
                                       "short_description", "created_at",
                                       "creator", "closed"] project
    graphQlId   <- string graphQlId
    number      <- integer projectNumber
    title       <- string projectTitle
    shortDescription <- optional string shortDescription
    createdAt   <- parseDateTime =<< string createdAtStr
    creator     <- string creatorLogin
    closed      <- bool closed
    pure $ MkProject {
        graphQlId
      , number
      , title
      , shortDescription
      , createdAt
      , creator
      , closed
      }

||| Parse a single project from a JSON String
export
parseProjectString : String -> Either String Project
parseProjectString =
  (mapFst (const "Failed to parse JSON") . parseJSON Virtual) >=> parseProject

||| Parse a list of projects from a JSON String
export
parseProjectsString : String -> Either String (List Project)
parseProjectsString =
  (mapFst (const "Failed to parse JSON") . parseJSON Virtual) >=> array parseProject
