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
import Ports



-- TYPES


type Msg
    = NoOp
    | EventMsg Events.Msg
    | SaveMeal
    | MealMsg Meal.Msg
    | QueryAll



--| DataMsg DbMsg
-- type DbMsg
--     = MealTransform Meal.DbMsg


type State
    = RecordMeal Meal.Modal


type alias Model =
    { eventState : Events.State
    , mealModal : Meal.Modal
    }


type Event
    = MealEvent Meal.Event String -- StreamId



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
            Events.State (Random.initialSeed seed)

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
        
        QueryAll ->
            ( model, Cmd.batch [Ports.queryAllEvents (), Ports.queryStreamEvents ""] )


view : Model -> Html Msg
view model =
    main_ [ class "container-fluid" ]
        [ title
        , Meal.viewMealModal model.mealModal |> Html.map MealMsg
        ]



-- HELPERS


parseEvent : D.Decoder Event
parseEvent =
    D.field "type" D.string
        |> D.andThen
            (\t ->
                D.oneOf
                    [ D.map2 MealEvent
                        (D.field "payload" (Meal.parseMealEvent t))
                        (D.field "streamId" D.string)
                    ]
            )


title : Html msg
title =
    h1 [] [ text "FlexFoodLog" ]
