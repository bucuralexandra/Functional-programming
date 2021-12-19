module Model.PersonalDetails exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, id)
import Html.Attributes exposing (href)
import Html.Attributes exposing (style)


type alias DetailWithName =
    { name : String
    , detail : String
    }


type alias PersonalDetails =
    { name : String
    , contacts : List DetailWithName
    , intro : String
    , socials : List DetailWithName
    }

textWrite: DetailWithName -> String
textWrite = \x -> x.name ++ ": " ++ x.detail

view : PersonalDetails -> Html msg
view details =
        let 
             getContacts contacts = List.map (\i -> li [class "contact-detail"] [ (textWrite >> text) i ]) contacts
             getLinks links = List.map(\i -> li [class "social-link"] [a [ href <| i.detail] [ text i.name ] ]) links
             textColor = style "color" "white"
        in
            div []
            [ h1 [ textColor, id "name" ] [text details.name]
            , em [ id "intro"] [text details.intro]
            , h2 [textColor] [  i [] [ text "Contacts" ]]
            , ul [] (getContacts details.contacts)
            , h2 [textColor] [ i [] [ text "Links" ]]
            , ul [] (getLinks details.socials)
            ]
        