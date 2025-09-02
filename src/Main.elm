module Main exposing (main)

import Browser
import Events
import Html exposing (..)
import Html.Attributes exposing (class, type_)
import Html.Events
import Json.Decode as D
import Meal
import Random
import SearchableDropdown



-- TYPES


type Msg
    = NoOp
    | SearchableDropdownMsg SearchableDropdown.Msg
    | EventMsg (Events.Msg )
    | SaveMeal
    | MealMsg Meal.Msg
    --| DataMsg DbMsg

-- type DbMsg
--     = MealTransform Meal.DbMsg


type alias FoodForm =
    { datetime : String
    , dropdown : SearchableDropdown.Model
    }


type alias Model =
    { foodForm : FoodForm
    , eventState : Events.State
    , mealModal : Meal.Modal
    }

type Event
    = MealEvent Meal.Event

-- MAIN


main : Program D.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- INIT, UPDATE, VIEW


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
            , mealModal = Meal.initModal
            }

    in
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SearchableDropdownMsg subMsg ->
            let
                foodForm =
                    model.foodForm

                ( updatedDropdown, cmd ) =
                    SearchableDropdown.update (\_ _ -> Cmd.none) subMsg foodForm.dropdown
            in
            ( { model | foodForm = { foodForm | dropdown = updatedDropdown } }, cmd )

        EventMsg subMsg ->
            let
                ( updatedEventState, cmd ) =
                    Events.update (Debug.log "Event update" subMsg) model.eventState
            in
            ( { model | eventState = updatedEventState }, Cmd.map EventMsg cmd )

        SaveMeal ->
            ( model, Cmd.none )

        MealMsg subMsg ->
            let
                ( updatedMealModal, cmd ) =
                    Meal.updateMeal
                        (\se ->
                            Cmd.map EventMsg <| Events.buildEnvelope se model.eventState
                        )
                        (Cmd.map MealMsg)
                        subMsg
                        model.mealModal
            in
            ( { model | mealModal = updatedMealModal }, cmd )


view : Model -> Html Msg
view model =
    main_ [ class "container-fluid" ]
        [ title
        , newMeal model.foodForm
        , Meal.viewMealModal model.mealModal |> Html.map MealMsg
        ]



-- HELPERS




title : Html msg
title =
    h1 [] [ text "FlexFoodLog" ]


newMeal : FoodForm -> Html Msg
newMeal model =
    article []
        [ header [] [ h2 [] [ text "Record Meal" ] ]
        , form []
            [ Html.map SearchableDropdownMsg <| SearchableDropdown.view model.dropdown
            , input [ type_ "datetime-local", Html.Attributes.placeholder "Date" ] []
            , textarea [ Html.Attributes.placeholder "Notes" ] []
            , button
                [ type_ "submit"
                , Html.Events.preventDefaultOn "click" (D.succeed ( NoOp, True ))
                ]
                [ text "Save" ]
            ]
        ]
