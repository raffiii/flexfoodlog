module Meal exposing (Meal, Msg, Modal, Event, InteractionEvent, HydrationEvent, initModal, viewMealModal, updateMeal, addedIngredientsSub, applyMealEventList)

import Event as Ev
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import Ports
import SearchableDropdown as SD
import Task
import Time exposing (Month(..))


type alias Meal =
    { datetime : String
    , items : List String
    , notes : Maybe String
    , streamId : String
    , streamPosition : Int
    }


type Msg
    = NoOp
    | MakeEvent Event ( String, String ) -- CorrelationId, Causation Id
    | NoteChanged String
    | DateTimeChanged String
    | SDMsg SD.Msg
    | RetrievedIngredients (List String)


type alias Modal =
    { meal : Meal
    , dropdown : SD.Model
    }


type Event
    = IngredientAdded String
    | IngredientRemoved String
    | NotesUpdated (Maybe String)
    | DateTimeUpdated String
    | DeleteMeal


type InteractionEvent
    = InteractionMealEvent Event String -- StreamId
    | InteractionCreateMeal -- CorrelationId


type HydrationEvent
    = HydrateMealEvent Event String -- StreamId
    | HydrateCreateMeal String -- StreamId

-- CONSTANTS
correlationId : String
correlationId =
    causationId
causationId : String
causationId =
    "meal-modal-1"


-- INIT


initMeal : Meal
initMeal =
    { datetime = ""
    , items = []
    , notes = Nothing
    , streamId = "Meal:*"
    , streamPosition = 0
    }


initModal : ( Modal, Cmd Msg )
initModal =
    ( { meal = initMeal
      , dropdown = SD.init []
      }
    , Ports.queryEventType "IngredientAdded"
    )


addedIngredientsSub : Sub Msg
addedIngredientsSub =
    Ports.onEvents
        (\value ->
            -- Decode [ { type: "IngredientAdded", name: "Apple" }, ... ]
            case
                D.decodeValue
                    (D.list
                        (D.field "type" D.string
                            |> D.andThen
                                (\type_ ->
                                    case Debug.log "" type_ of
                                        "IngredientAdded" ->
                                            D.field "payload" (D.field "ingredient" D.string)

                                        _ ->
                                            D.fail "Not an IngredientAdded event"
                                )
                        )
                    )
                    value
            of
                Ok items ->
                    RetrievedIngredients <| Debug.log "Retrieved" items

                Err _ ->
                    Debug.log "Error on retrieving" NoOp
        )



-- VIEWs


dialog : List (Attribute msg) -> List (Html msg) -> Html msg
dialog attrs nodes =
    Html.node "dialog" attrs nodes


viewMealModal : Modal -> Html Msg
viewMealModal modal =
    dialog [ Attr.attribute "open" "" ]
        [ article []
            [ header []
                [ button [ Attr.attribute "aria-label" "Close", Attr.rel "prev" ] []
                , p [] [ strong [] [ text "Edit Meal" ] ]
                ]
            , viewForm modal
            ]
        ]


viewForm : Modal -> Html Msg
viewForm modal =
    p []
        [ Html.map SDMsg <| SD.view modal.dropdown
        , input
            [ Attr.type_ "datetime-local"
            , Attr.placeholder "Date"
            , Attr.value modal.meal.datetime
            , Html.Events.onInput DateTimeChanged
            , Html.Events.onBlur (MakeEvent (DateTimeUpdated modal.meal.datetime) ( "MealModal", "MealModal" ))
            ]
            []
        , textarea
            [ Attr.placeholder "Notes"
            , Attr.value (Maybe.withDefault "" modal.meal.notes)
            , Html.Events.onInput NoteChanged
            , Html.Events.onBlur (MakeEvent (NotesUpdated modal.meal.notes) ( "MealModal", "MealModal" ))
            ]
            []

        -- , button
        --     [ Attr.type_ "submit"
        --     , Html.Events.preventDefaultOn "click" (D.succeed ( NoOp, True ))
        --     ]
        --     [ text "Save" ]
        ]



-- UPDATE


updateMeal : (Cmd Msg -> Cmd m) -> Msg -> Modal -> ( Modal, Cmd m )
updateMeal mapMsg msg modal =
    let
        oldMeal =
            modal.meal

        oldDropdown =
            modal.dropdown
    in
    case msg of
        SDMsg subMsg ->
            let
                ( updatedDropdown, cmd ) =
                    SD.update onIngredientUpdate subMsg modal.dropdown
            in
            ( { modal | dropdown = updatedDropdown }, mapMsg cmd )

        MakeEvent event _ ->
                let
                    cmd =
                        persistMealEvent (InteractionMealEvent event oldMeal.streamId) oldMeal
                in
                ( modal, mapMsg cmd )

        NoteChanged notes ->
            ( { modal | meal = { oldMeal | notes = Just notes } }, Cmd.none )

        DateTimeChanged datetime ->
            ( { modal | meal = { oldMeal | datetime = datetime } }, Cmd.none )

        RetrievedIngredients items ->
            ( { modal | dropdown = SD.setItems items oldDropdown }, Cmd.none )

        NoOp ->
            ( modal, Cmd.none )


encodeMealEvent : Event -> ( String, E.Value )
encodeMealEvent ev =
    case ev of
        IngredientAdded ingredient ->
            ( "IngredientAdded"
            , E.object [ ( "ingredient", E.string ingredient ) ]
            )

        IngredientRemoved ingredient ->
            ( "IngredientRemoved"
            , E.object [ ( "ingredient", E.string ingredient ) ]
            )

        NotesUpdated notes ->
            ( "NotesUpdated"
            , E.object [ ( "notes", notes |> Maybe.map E.string |> Maybe.withDefault E.null ) ]
            )

        DateTimeUpdated datetime ->
            ( "DateTimeUpdated"
            , E.object [ ( "datetime", E.string datetime ) ]
            )

        DeleteMeal ->
            ( "MealDeleted", E.null )


persistMealEvent : InteractionEvent -> Meal -> Cmd Msg
persistMealEvent mealEvent meal =
    case mealEvent of
        InteractionMealEvent ev streamId ->
            let
                ( eventType, payload ) =
                    encodeMealEvent ev

                eventData =
                    { streamId = streamId
                    , expectedStreamPosition = meal.streamPosition + 1
                    , type_ = eventType
                    , schemaVersion = 1
                    , payload = payload
                    , correlationId = correlationId
                    , causationId = causationId
                    }
            in
            Ev.persist eventData |> Cmd.map (\_ -> NoOp)

        InteractionCreateMeal ->
            let
                ( eventType, payload ) =
                    encodeMealEvent (DateTimeUpdated meal.datetime)

                eventData =
                    { streamType = "meal"
                    , type_ = eventType
                    , schemaVersion = 1
                    , payload = payload
                    , correlationId = correlationId
                    , causationId = causationId
                    }
            in
            Ev.persistNew eventData |> Cmd.map (\_ -> NoOp)


parseMealEvent : String -> D.Decoder HydrationEvent
parseMealEvent type_ =
    let
        mapMealEvent decoder =
            D.map2 HydrateMealEvent decoder (D.field "streamId" D.string)
    in
    case type_ of
        "IngredientAdded" ->
            mapMealEvent
                (D.map IngredientAdded (D.field "name" D.string))

        "IngredientRemoved" ->
            mapMealEvent
                (D.map IngredientRemoved (D.field "name" D.string))

        "MealCreated" ->
            D.map HydrateCreateMeal (D.field "streamId" D.string)

        "NotesUpdated" ->
            mapMealEvent <|
                D.map NotesUpdated (D.field "notes" (D.nullable D.string))

        "DateTimeUpdated" ->
            mapMealEvent <|
                D.map DateTimeUpdated (D.field "datetime" D.string)

        "MealDeleted" ->
            mapMealEvent <|
                D.succeed DeleteMeal

        _ ->
            D.fail ("Unknown event type: " ++ type_)


applyMealEventList : HydrationEvent -> List Meal -> List Meal
applyMealEventList mealEvent meals =
    case mealEvent of
        HydrateCreateMeal streamId ->
            let
                newMeal =
                    { initMeal
                        | streamId = streamId
                    }
            in
            newMeal :: meals

        HydrateMealEvent ev streamId ->
            let
                mealWithId =
                    List.filter (\m -> m.streamId == streamId) meals |> List.head

                updatedMeal =
                    applyEvent ev mealWithId
            in
            case ( mealWithId, updatedMeal ) of
                ( Nothing, Just newMeal ) ->
                    newMeal :: meals

                ( Just _, Just updated ) ->
                    updated :: List.filter (\m -> m.streamId /= streamId) meals

                ( Just _, Nothing ) ->
                    List.filter (\m -> m.streamId /= streamId) meals

                ( Nothing, Nothing ) ->
                    meals


applyEvent : Event -> Maybe Meal -> Maybe Meal
applyEvent ev maybeMeal =
    case ( ev, maybeMeal ) of
        ( IngredientAdded ingredient, Just meal ) ->
            Just { meal | items = ingredient :: meal.items }

        ( IngredientRemoved ingredient, Just meal ) ->
            Just { meal | items = List.filter (\i -> i /= ingredient) meal.items }

        ( NotesUpdated notes, Just meal ) ->
            Just { meal | notes = notes }

        ( DateTimeUpdated datetime, Just meal ) ->
            Just { meal | datetime = datetime }

        ( DeleteMeal, Just _ ) ->
            Nothing

        ( _, Nothing ) ->
            Nothing


onIngredientUpdate : List String -> List String -> Cmd Msg
onIngredientUpdate oldItems newItems =
    let
        addedItems =
            List.filter (\item -> not (List.member item oldItems)) newItems

        removedItems =
            List.filter (\item -> not (List.member item newItems)) oldItems

        addCmds =
            List.map (\item -> IngredientAdded item) addedItems

        removeCmds =
            List.map (\item -> IngredientRemoved item) removedItems

        tasks =
            List.map Task.succeed (addCmds ++ removeCmds)
    in
    Cmd.batch (List.map (Task.perform (\e -> MakeEvent e ( "MealModal:0", "SD:0" ))) tasks)
