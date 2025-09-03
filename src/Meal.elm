module Meal exposing (..)

import Events
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
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
    | MakeEvent Event
    | NoteChanged String
    | DateTimeChanged String
    | SDMsg SD.Msg


type alias Modal =
    { meal : Meal
    , dropdown : SD.Model
    }


type Event
    = MealCreated String String Meal -- CorrelationId, Causation Id, Meal
    | IngredientAdded String String String -- CorrelationId, Causation Id, Ingredient
    | IngredientRemoved String String String -- CorrelationId, Causation Id, Ingredient
    | NotesUpdated String String (Maybe String) -- CorrelationId, Causation Id, Notes
    | DateTimeUpdated String String String -- CorrelationId, Causation Id, DateTime
    | MealDeleted String String -- CorrelationId, Causation Id



-- INIT


initMeal : Meal
initMeal =
    { datetime = ""
    , items = []
    , notes = Nothing
    , streamId = "Meal:*"
    }


initModal : Modal
initModal =
    { meal = initMeal
    , dropdown = SD.init []
    }



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
            , Html.Events.onBlur (MakeEvent <| DateTimeUpdated "MealModal" "MealModal" modal.meal.datetime)
            ]
            []
        , textarea
            [ Attr.placeholder "Notes"
            , Attr.value (Maybe.withDefault "" modal.meal.notes)
            , Html.Events.onInput NoteChanged
            , Html.Events.onBlur (MakeEvent <| NotesUpdated "MealModal" "MealModal" modal.meal.notes)
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
    case msg of
        SDMsg subMsg ->
            let
                ( updatedDropdown, cmd ) =
                    SD.update onIngredientUpdate subMsg modal.dropdown
            in
            ( { modal | dropdown = updatedDropdown }, mapMsg cmd )

        MakeEvent event ->
            case toEnvelope event modal of
                Just envelope ->
                    ( modal, mapEnvelope envelope )

                Nothing ->
                    ( modal, Cmd.none )

        NoteChanged notes ->
            let
                oldMeal =
                    modal.meal
            in
            ( { modal | meal = { oldMeal | notes = Just notes } }, Cmd.none )

        DateTimeChanged datetime ->
            let
                oldMeal =
                    modal.meal
            in
            ( { modal | meal = { oldMeal | datetime = datetime } }, Cmd.none )

        NoOp ->
            ( modal, Cmd.none )


toEnvelope : Event -> Modal -> Maybe Events.SmallEnvelope
toEnvelope ev modal =
    case ev of
        IngredientAdded corrId causeId ingredient ->
            Just <|
                Events.SmallEnvelope
                    modal.meal.streamId
                    "IngredientAdded"
                    (E.object
                        [ ( "ingredient", E.string ingredient )
                        ]
                    )
                    corrId
                    causeId

        IngredientRemoved corrId causeId ingredient ->
            Just <|
                Events.SmallEnvelope
                    modal.meal.streamId
                    "IngredientRemoved"
                    (E.object
                        [ ( "ingredient", E.string ingredient )
                        ]
                    )
                    corrId
                    causeId

        MealCreated corrId causeId _ ->
            Just <|
                Events.SmallEnvelope
                    modal.meal.streamId
                    "MealCreated"
                    (E.object [])
                    corrId
                    causeId

        NotesUpdated corrId causeId notes ->
            Just <|
                Events.SmallEnvelope
                    modal.meal.streamId
                    "NotesUpdated"
                    (case notes of
                        Just n ->
                            E.object [ ( "notes", E.string n ) ]

                        Nothing ->
                            E.object [ ( "notes", E.null ) ]
                    )
                    corrId
                    causeId

        DateTimeUpdated corrId causeId datetime ->
            Just <|
                Events.SmallEnvelope
                    modal.meal.streamId
                    "DateTimeUpdated"
                    (E.object [ ( "datetime", E.string datetime ) ])
                    corrId
                    causeId

        MealDeleted corrId causeId ->
            Just <|
                Events.SmallEnvelope
                    modal.meal.streamId
                    "MealDeleted"
                    (E.object [])
                    corrId
                    causeId


onIngredientUpdate : List String -> List String -> Cmd Msg
onIngredientUpdate oldItems newItems =
    let
        addedItems =
            List.filter (\item -> not (List.member item oldItems)) newItems

        removedItems =
            List.filter (\item -> not (List.member item newItems)) oldItems

        addCmds =
            List.map (\item -> IngredientAdded "MealModal:0" "SD:0" item) addedItems

        removeCmds =
            List.map (\item -> IngredientRemoved "MealModal:0" "SD:0" item) removedItems

        tasks =
            List.map Task.succeed (addCmds ++ removeCmds)
    in
    Cmd.batch (List.map (Task.perform MakeEvent) tasks)
