module Meal exposing (..)

import Events
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import Ports
import SearchableDropdown as SD
import Task


type alias Meal =
    { datetime : String
    , items : List String
    , notes : Maybe String
    , streamId : String
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
    = MealCreated
    | IngredientAdded String
    | IngredientRemoved String
    | NotesUpdated (Maybe String)
    | DateTimeUpdated String
    | MealDeleted



-- INIT


initMeal : Meal
initMeal =
    { datetime = ""
    , items = []
    , notes = Nothing
    , streamId = "Meal:*"
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
            case D.decodeValue (D.list ( (D.field "type" D.string) |> D.andThen (\type_ ->
                case Debug.log "" type_ of
                    "IngredientAdded" ->
                        D.field "payload" (D.field "ingredient" D.string)

                    _ ->
                        D.fail "Not an IngredientAdded event"
                ))) value of
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


updateMeal : (Events.SmallEnvelope -> Cmd m) -> (Cmd Msg -> Cmd m) -> Msg -> Modal -> ( Modal, Cmd m )
updateMeal mapEnvelope mapMsg msg modal =
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

        MakeEvent event cause ->
            case toEnvelope event cause modal of
                Just envelope ->
                    ( modal, mapEnvelope envelope )

                Nothing ->
                    ( modal, Cmd.none )

        NoteChanged notes ->
            ( { modal | meal = { oldMeal | notes = Just notes } }, Cmd.none )

        DateTimeChanged datetime ->
            ( { modal | meal = { oldMeal | datetime = datetime } }, Cmd.none )

        RetrievedIngredients items ->
            ( { modal | dropdown = SD.setItems items oldDropdown }, Cmd.none )

        NoOp ->
            ( modal, Cmd.none )


toEnvelope : Event -> ( String, String ) -> Modal -> Maybe Events.SmallEnvelope
toEnvelope ev ( correId, causeId ) modal =
    let
        mealId =
            modal.meal.streamId

        ( type_, value ) =
            case ev of
                IngredientAdded ingredient ->
                    ( "IngredientAdded", E.string ingredient )

                IngredientRemoved ingredient ->
                    ( "IngredientRemoved", E.string ingredient )

                MealCreated ->
                    ( "MealCreated", E.object [] )

                NotesUpdated notes ->
                    ( "NotesUpdated"
                    , case notes of
                        Just n ->
                            E.object [ ( "notes", E.string n ) ]

                        Nothing ->
                            E.object [ ( "notes", E.null ) ]
                    )

                DateTimeUpdated datetime ->
                    ( "DateTimeUpdated", E.string datetime )

                MealDeleted ->
                    ( "MealDeleted", E.object [] )
    in
    Just <|
        Events.SmallEnvelope
            mealId
            type_
            value
            correId
            causeId


parseMealEvent : String -> D.Decoder Event
parseMealEvent type_ =
    case type_ of
        "IngredientAdded" ->
            D.map IngredientAdded (D.field "name" D.string)

        "IngredientRemoved" ->
            D.map IngredientRemoved (D.field "name" D.string)

        "MealCreated" ->
            D.succeed MealCreated

        "NotesUpdated" ->
            D.map NotesUpdated (D.field "notes" (D.nullable D.string))

        "DateTimeUpdated" ->
            D.map DateTimeUpdated (D.field "datetime" D.string)

        "MealDeleted" ->
            D.succeed MealDeleted

        _ ->
            D.fail ("Unknown event type: " ++ type_)


applyMealEvent : Event -> Maybe Meal -> Maybe Meal
applyMealEvent ev maybeMeal =
    case ( ev, maybeMeal ) of
        ( MealCreated, Nothing ) ->
            Just initMeal

        ( IngredientAdded ingredient, Just meal ) ->
            Just { meal | items = ingredient :: meal.items }

        ( IngredientRemoved ingredient, Just meal ) ->
            Just { meal | items = List.filter (\i -> i /= ingredient) meal.items }

        ( NotesUpdated notes, Just meal ) ->
            Just { meal | notes = notes }

        ( DateTimeUpdated datetime, Just meal ) ->
            Just { meal | datetime = datetime }

        ( MealDeleted, Just _ ) ->
            Nothing

        _ ->
            maybeMeal


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
