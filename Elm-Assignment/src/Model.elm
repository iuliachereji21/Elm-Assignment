module Model exposing (..)

import Html exposing (b, div, p, text)
import Model.Date as Date
import Model.Event as Event exposing (Event)
import Model.Event.Category exposing (EventCategory(..), SelectedEventCategories, allSelected)
import Model.Interval as Interval
import Model.PersonalDetails exposing (DetailWithName, PersonalDetails)
import Model.Repo exposing (Repo)


type alias Model =
    { personalDetails : PersonalDetails
    , events : List Event
    , selectedEventCategories : SelectedEventCategories
    , repos : List Repo
    }


academicEvents : List Event
academicEvents =
    [ { title = "I graduated Nicolae Balcescu high school"
      , interval = Interval.full (Date.full 2015 Date.Sep) (Date.full 2019 Date.Jun) |> Maybe.withDefault (Interval.withDurationYears (Date.onlyYear 2015) 4)
      , description = p [] [ text "I obtained ", b [] [ text "very" ], text " good grades." ]
      , category = Academic
      , url = Nothing
      , tags = []
      , important = False
      }
    , { title = "I obtained my baccalaureate degree"
      , interval = Interval.withDurationMonths 2019 Date.Jun 1
      , description = div [] [text "I got the grades: Mathematics 10, Informatics 10, Romanian language 9 with the final mark 9.66."]
      , category = Academic
      , url = Nothing
      , tags = []
      , important = True
      }
    , { title = "I am an engineering student at the Technical University of Cluj-Napoca"
      , interval = Interval.open <| Date.full 2019 Date.Oct
      , description = div [] [text "I study computer science in the 3rd year out of 4 and I expect to graduate in 2023 and get a bachelor degree."]
      , category = Academic
      , url = Nothing
      , tags = []
      , important = True
      }  
    ]


workEvents : List Event
workEvents =
    [ { title = "I did not work before"
      , interval = Interval.open <| Date.full 2019 Date.Oct
      , description = text ""
      , category = Work
      , url = Nothing
      , tags = []
      , important = False
      }
    ]


projectEvens : List Event
projectEvens =
    [ { title = "Java desktop application"
      , interval = Interval.withDurationMonths 2020 Date.Oct 3
      , description = text "Order food from a restaurant. And also have the possibility to create an account. It also had a database. I did it working in a team with a colleague."
      , category = Project
      , url = Nothing
      , tags = []
      , important = True
      }
    , { title = "VHDL ATM"
      , interval = Interval.withDurationMonths 2020 Date.Mar 3
      , description = text "I did a finite state machine simulating an ATM with a RAM of data. Clients could withdraw money, check thair ballance, etc. I did it working in a team with a colleague."
      , category = Project
      , url = Nothing
      , tags = []
      , important = False
      }
    ]


personalDetails : PersonalDetails
personalDetails =
    { name = "Iulia Chereji"
    , intro = "I like programming and to learn new things!"
    , contacts = [ DetailWithName "email" "iuliachereji@gmail.com", DetailWithName "phone" "+40755228943" ]
    , socials = [ DetailWithName "facebook" "https://www.facebook.com/iulia.chereji.1/" ]
    }


initModel : Model
initModel =
    { personalDetails = personalDetails
    , events = Event.sortByInterval <| academicEvents ++ workEvents ++ projectEvens
    , selectedEventCategories = allSelected
    , repos = []
    }
