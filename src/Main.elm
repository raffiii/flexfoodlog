module Main exposing (main)

import Browser
import Event
import Html exposing (..)
import Html.Attributes exposing (class, type_)
import Html.Events
import Json.Decode as D
import Meal
import Random
import SearchableDropdown
import Ports



-- TYPES


type Msg
    = NoOp
    | EventMsg Event.Msg
    | SaveMeal
    | MealMsg Meal.Msg
    | QueryAll



--| DataMsg DbMsg
-- type DbMsg
--     = MealTransform Meal.DbMsg


type State
    = RecordMeal Meal.Modal

type alias DataState 
    = { meals : List Meal.Meal }


type alias Model =
    { eventState : Event.Model
    , mealModal : Meal.Modal
    }


type Event
    = MealEvent Meal.Event String -- StreamId

type TypeEvent
    = MealTypeEvent Meal.HydrationEvent


-- MAIN


main : Program D.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT, UPDATE, VIEW

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [
        Sub.map MealMsg Meal.addedIngredientsSub
    ]

init : D.Value -> ( Model, Cmd Msg )
init flags =
    let
        seed =
            D.decodeValue
                (D.field "seed" D.int)
                flags
                |> Result.withDefault 42

        eventState =
            Event.initialModel (Random.initialSeed seed)

        ( mealModel, cmd_ ) =
            Meal.initModal

        model =
            { eventState = eventState
            , mealModal = mealModel
            }
    in
    ( model, Cmd.map MealMsg cmd_ )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        EventMsg subMsg ->
            let
                ( updatedEventState, cmd ) =
                    Event.update (\_ -> NoOp) (Debug.log "Event update" subMsg) model.eventState
            in
            ( { model | eventState = updatedEventState }, cmd )

        SaveMeal ->
            ( model, Cmd.none )

        MealMsg subMsg ->
            let
                ( updatedMealModal, cmd ) =
                    Meal.updateMeal
                        (Cmd.map MealMsg)
                        subMsg
                        model.mealModal
            in
            ( { model | mealModal = updatedMealModal }, cmd )
        
        QueryAll ->
            ( model, Cmd.batch [Ports.queryAllEvents (), Ports.queryStreamEvents ""] )


view : Model -> Html Msg
view model =
    main_ [ class "container-fluid" ]
        [ title
        , Meal.viewMealModal model.mealModal |> Html.map MealMsg
        ]



-- HELPERS

title : Html msg
title =
    h1 [] [ text "FlexFoodLog" ]

initialState : DataState
initialState =
    { meals = [] }

applyTypeEvent : TypeEvent -> DataState -> DataState
applyTypeEvent typeEvent state =
    case typeEvent of
        MealTypeEvent mealEvent ->
            Meal.applyMealEventList mealEvent state.meals |> (\meals -> { state | meals = meals })
