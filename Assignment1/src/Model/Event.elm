module Model.Event exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, href)
import Model.Event.Category exposing (EventCategory(..))
import Model.Interval exposing (..)
import Model.Date exposing ( dateToString, Date)
import List exposing (sortWith)
import Html.Attributes exposing (style)



type alias Event =
    { title : String
    , interval : Interval
    , description : Html Never
    , category : EventCategory
    , url : Maybe String
    , tags : List String
    , important : Bool
    }


categoryView : EventCategory -> Html Never
categoryView category =
    case category of
        Academic ->
            text "Academic"

        Work ->
            text "Work"

        Project ->
            text "Project"

        Award ->
            text "Award"

sortByInterval : List Event -> List Event
sortByInterval events = 
     let
        compareEvents = \eA eB -> Model.Interval.compare eA.interval eB.interval
     in  
     events 
     |>  sortWith compareEvents


urlView : Maybe String -> Html Never
urlView url = 
    case url of
       Just word -> a [ href word ] [text word]
       Nothing -> text "" 
intervalView: Interval -> String
intervalView (Interval {start, end}) = 
        case (end) of
        Just b ->  dateToString (Just start) ++ "-" ++ dateToString (Just b)
        Nothing -> dateToString (Just start) ++ "-" ++ "Present"
prettyViewBoolean:Bool -> String
prettyViewBoolean b = if b then "Important event" else "Not an important event"

isImportant : Bool -> List (Attribute msg)
isImportant c =
    let 
        border = style "border" "solid 4px"
        margin = style "margin" "2px"
        color = style "background-color" "#999966"
        allign = style "text-align" "left"
    in
     if c then 
        [border, margin, color, allign, class "event" , class "event-important"] 
    else 
        [ border,margin,color,allign, class "event"]

view :Event -> Html Never
view event = 
    let
        getTags tags = List.map (\i -> li [] [ text i ]) tags
    in
        div (isImportant event.important)
        [ 
          h2[style "color" "white" , class "event-title"] [ text event.title ]
        , h5 [class "event-interval"] [ text <| intervalView event.interval] 
        , h4 [class "event-category"] [ categoryView event.category ]
        , em[class "event-description"] [ event.description ]
        , h4 [class "event-url"] [ urlView event.url ]
        , ul [] (getTags event.tags) 
        , h6[] [ (prettyViewBoolean >> text) event.important]
        ]
