module Model.Event.Category exposing (EventCategory(..), SelectedEventCategories, allSelected, eventCategories, isEventCategorySelected, set, view)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (checked, class, style, type_)
import Html.Events exposing (onCheck)
import List exposing (member)
import List exposing (filter)


type EventCategory
    = Academic
    | Work
    | Project
    | Award


eventCategories =
    [ Academic, Work, Project, Award ]


{-| Type used to represent the state of the selected event categories
-}
type SelectedEventCategories
    = SelectedEventCategories { list : List EventCategory }

list : SelectedEventCategories -> List EventCategory
list (SelectedEventCategories e) =
    e.list

{-| Returns an instance of `SelectedEventCategories` with all categories selected

    isEventCategorySelected Academic allSelected --> True

-}
allSelected : SelectedEventCategories
allSelected = SelectedEventCategories { list = [ Academic, Work, Project, Award ]}


{-| Given a the current state and a `category` it returns whether the `category` is selected.

    isEventCategorySelected Academic allSelected --> True

-}
isEventCategorySelected : EventCategory -> SelectedEventCategories -> Bool
isEventCategorySelected category current =  member category <| list current

{-| Given an `category`, a boolean `value` and the current state, it sets the given `category` in `current` to `value`.

    allSelected |> set Academic False |> isEventCategorySelected Academic --> False

    allSelected |> set Academic False |> isEventCategorySelected Work --> True

-}

updateSelectedEventCategories: SelectedEventCategories -> EventCategory -> String -> SelectedEventCategories
updateSelectedEventCategories (SelectedEventCategories current) elem mode = 
    case mode of
       "+" -> SelectedEventCategories { list = elem::current.list}
       _ -> SelectedEventCategories { list = filter (\x -> x /= elem) current.list}

set : EventCategory -> Bool -> SelectedEventCategories -> SelectedEventCategories
set category value current =
     if value then 
                if isEventCategorySelected category current then current 
                else updateSelectedEventCategories current category "+"
    else
                if not (isEventCategorySelected category current) then current 
                else updateSelectedEventCategories current category "-"



checkbox : String -> Bool -> EventCategory -> Html ( EventCategory, Bool )
checkbox name state category =
    div [ style "display" "inline", class "category-checkbox" ]
        [ input [ type_ "checkbox", onCheck (\c -> ( category, c )), checked state ] []
        , text name
        ]


view : SelectedEventCategories -> Html ( EventCategory, Bool )
view model =
     div [] 
     [
          checkbox "Academic" (isEventCategorySelected Academic model) Academic
        , checkbox "Work" (isEventCategorySelected Work model) Work
        , checkbox "Project" (isEventCategorySelected Project model) Project
        , checkbox "Award" (isEventCategorySelected Award model) Award
     ]