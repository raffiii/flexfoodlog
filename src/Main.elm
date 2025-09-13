module Main exposing (main)

import Browser
import Event as Ev
import Html exposing (..)
import Html.Attributes exposing (class)
import Json.Decode as D
import Ports
import Random
import ViewMeal as Meal



-- TYPES


type Msg
    = NoOp
    | EventMsg Ev.Msg
    | SaveMeal
    | MealMsg Meal.Msg
    | QueryAll
    | HydrationEvents (List TypeEvent)


type alias Model =
    { eventState : Ev.Model
    , meals : Meal.Model
    }


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
subscriptions _ =
    Sub.batch
        [ Sub.map HydrationEvents (Ev.recieveEvents decodeEventList)
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
            Ev.initialModel (Random.initialSeed seed)

        ( mealModel, cmd_ ) =
            Meal.init

        model =
            { eventState = eventState
            , meals = mealModel
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
                    Ev.update (\_ -> NoOp) (Debug.log "Event update" subMsg) model.eventState
            in
            ( { model | eventState = updatedEventState }, cmd )

        SaveMeal ->
            ( model, Cmd.none )

        MealMsg subMsg ->
            let
                ( updatedMealModal, cmd, evCmd ) =
                    Meal.update
                        (Debug.log "Sending ViewMealMsg" subMsg)
                        model.meals
            in
            ( { model | meals = updatedMealModal }
            , Cmd.batch
                [ Cmd.map EventMsg evCmd
                , Cmd.map MealMsg cmd
                ]
            )

        QueryAll ->
            ( model, Cmd.batch [ Ports.queryAllEvents (), Ports.queryStreamEvents "" ] )

        HydrationEvents typeEvents ->
            ( List.foldl applyTypeEvent model typeEvents, Cmd.none )


view : Model -> Html Msg
view model =
    main_ [ class "container-fluid" ]
        [ title
        , Meal.view model.meals |> Html.map MealMsg
        ]



-- HELPERS


title : Html msg
title =
    h1 [] [ text "FlexFoodLog" ]


decodeEvent : Ev.RecievedEnvelope -> List TypeEvent
decodeEvent env =
    let
        parserConstrucors =
            [ ( Meal.parseMealEvent, MealTypeEvent )
            ]
    in
    parserConstrucors
        |> List.map (\( parser, constructor ) -> parser >> Maybe.map constructor)
        |> List.filterMap (\f -> f env)


decodeEventList : Result D.Error (List Ev.RecievedEnvelope) -> List TypeEvent
decodeEventList result =
    result
        |> Result.withDefault []
        |> List.concatMap
            decodeEvent


applyTypeEvent : TypeEvent -> Model -> Model
applyTypeEvent typeEvent model =
    case typeEvent of
        MealTypeEvent mealEvent ->
            Meal.applyMealEventList mealEvent model.meals |> (\meals -> { model | meals = meals })
