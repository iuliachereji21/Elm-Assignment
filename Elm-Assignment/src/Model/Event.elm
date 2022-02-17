module Model.Event exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, style)
import Model.Event.Category exposing (EventCategory(..))
import Model.Interval as Interval exposing (Interval, compare, view)


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
    --List.sortBy .interval events
    List.sortWith (\x y -> Interval.compare x.interval y.interval) events
    --Debug.todo "Implement Event.sortByInterval"


view : Event -> Html Never
view event =
    let 
        isImportant =
            case event.important of
                True -> [class "event", class "event-important"]
                _ -> [class "event"]
    in
    div isImportant
    [ h1 [ class "event-category" ] [categoryView event.category] 
    , h1 [ class "event-title", style "color" "red" ] [text event.title]
    , h1 [ class "event-description" ] [event.description]
    , h1 [class "event-url"] [text <| Maybe.withDefault "" event.url]
    , h1 [class "event-interval"] [Interval.view event.interval]
    ]
    --Debug.todo "Implement the Model.Event.view function"
