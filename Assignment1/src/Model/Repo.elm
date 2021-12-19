module Model.Repo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Json.Decode as De
import List exposing (..)
import Json.Decode exposing (maybe)
import Html.Attributes exposing (style)


type alias Repo =
    { name : String
    , description : Maybe String
    , url : String
    , pushedAt : String
    , stars : Int
    }

view : Repo -> Html msg
view repo =
    let
        border = style "border" "solid 4px"
        margin = style "margin" "2px"
        color = style "background-color" "#669999"
    in
        div [border, margin, color,class "repo"]
         [
             h3 [class "repo-name"] [ text <| repo.name ]
            ,em [class "repo-description"] [ repo.description |> Maybe.withDefault "No description" |> text ]
            ,h4 [class "repo-url"] [a [ href <| repo.url] [ repo.url |> text]]
            ,h6 [class "repo-stars"] [ text <| String.fromInt repo.stars ]
        ]

sortByStars : List Repo -> List Repo
sortByStars repos = (sortBy .stars >> reverse )repos


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
        (De.maybe (De.field "description" De.string))
        (De.field "html_url" De.string)
        (De.field "pushed_at" De.string)
        (De.field "stargazers_count" De.int)

