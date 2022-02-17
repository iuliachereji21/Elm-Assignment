module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Http
import Json.Decode as De
import Model exposing (..)
import Model.Event as Event
import Model.Event.Category as EventCategory exposing(EventCategory(..), SelectedEventCategories(..), eventCategories )
import Model.PersonalDetails as PersonalDetails
import Model.Repo as Repo


type Msg
    = GetRepos
    | GotRepos (Result Http.Error (List Repo.Repo))
    | SelectEventCategory EventCategory.EventCategory
    | DeselectEventCategory EventCategory.EventCategory


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


getRepos : Cmd Msg
getRepos = Http.get 
    { url =  "http://localhost:8000"
    , expect = Http.expectJson GotRepos (De.list Repo.decodeRepo)
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , getRepos
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let 
        ac: SelectedEventCategories ->Bool
        ac (SelectedEventCategories ev) =
            ev.academic
        wo: SelectedEventCategories ->Bool
        wo (SelectedEventCategories ev) =
            ev.work
        pr: SelectedEventCategories ->Bool
        pr (SelectedEventCategories ev) =
            ev.project
        aw: SelectedEventCategories ->Bool
        aw (SelectedEventCategories ev) =
            ev.award
        acad = ac model.selectedEventCategories
        wor = wo model.selectedEventCategories
        proj = pr model.selectedEventCategories
        awar = aw model.selectedEventCategories

    in
    case msg of
        GetRepos ->
            ( model, Cmd.none )

        GotRepos res ->
            ( {model | repos = Result.withDefault [] res}, Cmd.none )

        SelectEventCategory category ->
            case category of
                EventCategory.Academic -> ({model | selectedEventCategories = SelectedEventCategories {academic = True, work = wor, project = proj, award = awar}},Cmd.none)
                EventCategory.Work -> ({model | selectedEventCategories = SelectedEventCategories {academic = acad, work = True, project = proj, award = awar}},Cmd.none)
                EventCategory.Project -> ({model | selectedEventCategories = SelectedEventCategories {academic = acad, work = wor, project = True, award = awar}},Cmd.none)
                EventCategory.Award -> ({model | selectedEventCategories = SelectedEventCategories {academic = acad, work = wor, project = proj, award = True}},Cmd.none)
                
        DeselectEventCategory category ->
            case category of
                EventCategory.Academic -> ({model | selectedEventCategories = SelectedEventCategories {academic = False, work = wor, project = proj, award = awar}},Cmd.none)
                EventCategory.Work -> ({model | selectedEventCategories = SelectedEventCategories {academic = acad, work = False, project = proj, award = awar}},Cmd.none)
                EventCategory.Project -> ({model | selectedEventCategories = SelectedEventCategories {academic = acad, work = wor, project = False, award = awar}},Cmd.none)
                EventCategory.Award -> ({model | selectedEventCategories = SelectedEventCategories {academic = acad, work = wor, project = proj, award = False}},Cmd.none)
                


eventCategoryToMsg : ( EventCategory.EventCategory, Bool ) -> Msg
eventCategoryToMsg ( event, selected ) =
    if selected then
        SelectEventCategory event

    else
        DeselectEventCategory event


view : Model -> Html Msg
view model =
    let
        eventCategoriesView =
            EventCategory.view model.selectedEventCategories |> Html.map eventCategoryToMsg

        eventsView =
            model.events
                |> List.filter (.category >> (\cat -> EventCategory.isEventCategorySelected cat model.selectedEventCategories))
                |> List.map Event.view
                |> div []
                |> Html.map never

        reposView =
            model.repos
                |> Repo.sortByStars
                |> List.take 5
                |> List.map Repo.view
                |> div []
    in
    div []
        [ PersonalDetails.view model.personalDetails
        , h2 [] [ text "Experience" ]
        , eventCategoriesView
        , eventsView
        , h2 [] [ text "My top repos" ]
        , reposView
        ]    
