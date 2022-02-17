module Model.Repo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Json.Decode as De


type alias Repo =
    { name : String
    , description : Maybe String
    , url : String
    , pushedAt : String
    , stars : Int
    }


view : Repo -> Html msg
view repo =
    div [class "repo"] 
    [ h1 [class "repo-name"] [text repo.name]
    , h1 [class "repo-description"] [text <| Maybe.withDefault "" repo.description]
    , h1 [class "repo-url"] [ a [ href repo.url ] [ text repo.url ] ]
    , h1 [class "repo-stars"] [text <| String.fromInt repo.stars]
    ]
    --Debug.todo "Implement Model.Repo.view"


sortByStars : List Repo -> List Repo
sortByStars repos =
    List.sortBy .stars repos
    --Debug.todo "Implement Model.Repo.sortByStars"


{-| Deserializes a JSON object to a `Repo`.
Field mapping (JSON -> Elm):

  - name -> name
  - description -> description
  - html\_url -> url
  - pushed\_at -> pushedAt
  - stargazers\_count -> stars

-}
decodeRepo : De.Decoder Repo
decodeRepo =
    De.map5 Repo
        (De.field "name" De.string)
        (De.field  "description" (De.nullable De.string))
        (De.field "url" De.string)
        (De.field "pushedAt" De.string)
        (De.field "stars" De.int)
    --De.fail "Not implemented"
    --Debug.todo "Implement Model.Repo.decodeRepo"
