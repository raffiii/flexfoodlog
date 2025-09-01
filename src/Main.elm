module Main exposing (main)

import Browser
import Events
import Html exposing (..)
import Html.Attributes exposing (class, type_)
import Json.Decode as D
import Random
import SearchableDropdown


main : Program D.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : D.Value -> ( Model, Cmd Msg )
init flags =
    let
        seed =
            D.decodeValue
                (D.field "seed" D.int)
                flags
                |> Result.withDefault 42

        foodForm =
            FoodForm "" (SearchableDropdown.init [ "Apple", "Banana", "Carrot", "Date", "Eggplant" ])

        eventState =
            Events.State (Random.initialSeed seed)

        model =
            { foodForm = foodForm
            , eventState = eventState
            }

        sampleEvent =
            Events.buildEnvelope
                "test:0"
                (Events.TitleChanged "App started")
                "SampleStartup:0"
                "SampleStartup:0"
                eventState
    in
    ( model, sampleEvent |> Cmd.map EventMsg )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SearchableDropdownMsg subMsg ->
            let
                foodForm =
                    model.foodForm

                updatedDropdown =
                    SearchableDropdown.update subMsg foodForm.dropdown
            in
            ( { model | foodForm = { foodForm | dropdown = updatedDropdown } }, Cmd.none )

        EventMsg subMsg ->
            let
                ( updatedEventState, cmd ) =
                    Events.update subMsg model.eventState
            in
            ( { model | eventState = updatedEventState }, Cmd.map EventMsg cmd )


type Msg
    = NoOp
    | SearchableDropdownMsg SearchableDropdown.Msg
    | EventMsg Events.Msg


type alias FoodForm =
    { datetime : String
    , dropdown : SearchableDropdown.Model
    }


type alias Model =
    { foodForm : FoodForm
    , eventState : Events.State
    }


greet =
    h1 [] [ text "Hello, Elm!" ]


view : Model -> Html Msg
view model =
    main_ [ class "container-fluid" ]
        [ greet
        , newMeal model.foodForm
        ]


newMeal : FoodForm -> Html Msg
newMeal model =
    article []
        [ header [] [ h2 [] [ text "Record Meal" ] ]
        , form []
            [ Html.map SearchableDropdownMsg <| SearchableDropdown.view model.dropdown
            , input [ type_ "datetime", Html.Attributes.placeholder "Date" ] []
            , textarea [ Html.Attributes.placeholder "Notes" ] []
            ]
        ]
