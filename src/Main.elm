module Main exposing (main)

import Browser
import Event as Ev
import Html exposing (..)
import Html.Attributes exposing (class)
import Json.Decode as D
import Random
import ViewMeal as Meal
import ViewSymptom as Symptom



-- TYPES


type Msg
    = NoOp
    | EventMsg Ev.Msg
    | SaveMeal
    | MealMsg Meal.Msg
    | SymptomMsg Symptom.Msg
    | QueryAll
    | HydrationEvents (List TypeEvent)
    | PersistanceResult (List TypeEvent)


type alias Model =
    { eventState : Ev.Model
    , meals : Meal.Model
    , symptoms : Symptom.Model
    }


type TypeEvent
    = None
    | MealTypeEvent Meal.HydrationEvent
    | SymptomTypeEvent Symptom.HydrationEvent



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
        , Sub.map PersistanceResult (Ev.recievePersistenceResults (\result -> result |> Result.map decodeEvent |> Result.withDefault []))
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

        ( mealModel, mealInitCmd ) =
            Meal.init

        ( symptomModel, symptomCmd ) =
            Symptom.init

        model =
            { eventState = eventState
            , meals = mealModel
            , symptoms = symptomModel
            }

        initCmd =
            Cmd.batch
                [ Cmd.map MealMsg mealInitCmd
                , Cmd.map HydrationEvents Ev.hydrateAllStreams
                , Cmd.map SymptomMsg symptomCmd
                ]
    in
    ( model, initCmd )


mapPersistanceResult : Result Ev.PersistenceError Ev.Envelope -> Msg
mapPersistanceResult result =
    case result of
        Ok envelope ->
            PersistanceResult (decodeEvent envelope)

        Err _ ->
            NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        EventMsg subMsg ->
            let
                ( updatedEventState, cmd ) =
                    Ev.update mapPersistanceResult subMsg model.eventState
            in
            ( { model | eventState = updatedEventState }, cmd )

        SaveMeal ->
            ( model, Cmd.none )

        MealMsg subMsg ->
            let
                ( updatedMealModal, cmd, evCmd ) =
                    Meal.update
                        subMsg
                        model.meals
            in
            ( { model | meals = updatedMealModal }
            , Cmd.batch
                [ Cmd.map EventMsg evCmd
                , Cmd.map MealMsg cmd
                ]
            )

        SymptomMsg subMsg ->
            let
                ( updatedSymptomModel, cmd, evCmd ) =
                    Symptom.update
                        subMsg
                        model.symptoms
            in
            ( { model | symptoms = updatedSymptomModel }
            , Cmd.batch
                [ Cmd.map EventMsg evCmd
                , Cmd.map SymptomMsg cmd
                ]
            )

        QueryAll ->
            ( model, Cmd.none )

        PersistanceResult typeEvents ->
            ( List.foldl applyPersistanceResult model typeEvents, Cmd.none )

        HydrationEvents typeEvents ->
            ( List.foldl applyTypeEvent model typeEvents, Cmd.none )


view : Model -> Html Msg
view model =
    main_ [ class "container-fluid" ]
        [ title
        , Html.article [] [ Meal.view model.meals |> Html.map MealMsg ]
        , Html.article [] [ Symptom.view model.symptoms |> Html.map SymptomMsg ]
        ]



-- HELPERS


title : Html msg
title =
    h1 [] [ text "FlexFoodLog" ]


decodeEvent : Ev.Envelope -> List TypeEvent
decodeEvent env =
    let
        makeMsg ( parser, constructor ) =
            parser >> Maybe.map constructor

        parserConstrucors =
            [ makeMsg ( Meal.parseMealEvent, MealTypeEvent )
            , makeMsg ( Symptom.parseSymptomEvent, SymptomTypeEvent )
            ]
    in
    parserConstrucors
        |> List.filterMap (\f -> f env)


decodeEventList : Result D.Error (List Ev.Envelope) -> List TypeEvent
decodeEventList result =
    result
        |> Result.withDefault []
        |> List.concatMap
            decodeEvent


applyTypeEvent : TypeEvent -> Model -> Model
applyTypeEvent typeEvent model =
    case typeEvent of
        MealTypeEvent mealEvent ->
            model.meals
                |> Meal.applyMealEventList mealEvent
                |> (\meals -> { model | meals = meals })

        SymptomTypeEvent symptomEvent ->
            model.symptoms
                |> Symptom.applySymptomEventList symptomEvent
                |> (\symptoms -> { model | symptoms = symptoms })

        None ->
            model


applyPersistanceResult : TypeEvent -> Model -> Model
applyPersistanceResult typeEvent model =
    case typeEvent of
        MealTypeEvent mealEvent ->
            Meal.applyPersistanceResult mealEvent model.meals |> (\meals -> { model | meals = meals })

        SymptomTypeEvent symptomEvent ->
            Symptom.applyPersistanceResult symptomEvent model.symptoms |> (\symptoms -> { model | symptoms = symptoms })

        None ->
            model
