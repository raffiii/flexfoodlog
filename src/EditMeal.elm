module EditMeal exposing (Model, Msg, empty, init, initWithMeal, update, view)

import Event as Ev
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Json.Encode as E
import SearchableDropdown as SD



-- Model


type alias Model =
    { streamId : String
    , streamPosition : Int
    , ingredients : SD.Model
    , datetime : String
    , notes : String
    }



-- MESSAGES


type Msg
    = NoOp
    | DropdownMsg SD.Msg
    | NoteChanged String
    | DateTimeChanged String
    | MakeEvent Event


type Event
    = IngredientAdded String
    | IngredientRemoved String
    | NotesUpdated String
    | DateTimeUpdated String
    | DeleteMeal


type InteractionEvent
    = InteractionMealEvent Event String -- StreamId
    | AssignedMealId String -- StreamId



-- CONSTANTS


correlationId : String
correlationId =
    causationId


causationId : String
causationId =
    "meal-modal-1"



-- INIT


empty : List String -> Model
empty ingredientsList =
    { streamId = ""
    , streamPosition = 0
    , ingredients = SD.init ingredientsList
    , datetime = ""
    , notes = ""
    }


init : List String -> ( Model, Cmd Msg )
init ingredientsList =
    let
        eventData =
            { streamType = "meal"
            , type_ = "CreateMeal"
            , schemaVersion = 1
            , payload = E.object []
            , correlationId = correlationId
            , causationId = causationId
            }
    in
    ( empty ingredientsList
    , Ev.persistNew eventData |> Cmd.map (\_ -> NoOp)
    )


initWithMeal :
    { streamId : String
    , streamPosition : Int
    , ingredients : List String
    , datetime : String
    , notes : String
    }
    -> List String
    -> ( Model, Cmd Msg )
initWithMeal meal ingredientsList =
    ( Model meal.streamId meal.streamPosition (SD.init ingredientsList |> SD.setItems meal.ingredients) meal.datetime meal.notes, Cmd.none )



-- VIEW


view : Model -> (Msg -> msg) -> msg -> Html msg
view modal mapMsg onClose =
    dialog [ Attr.attribute "open" "" ]
        [ article []
            [ header []
                [ button [ Attr.attribute "aria-label" "Close", Attr.rel "prev", onClick onClose ] []
                , p [] [ strong [] [ text "Edit Meal" ] ]
                ]
            , Html.map mapMsg <| viewForm modal
            ]
        ]


viewForm : Model -> Html Msg
viewForm modal =
    p []
        [ SD.view modal.ingredients DropdownMsg (MakeEvent << IngredientAdded) (MakeEvent << IngredientRemoved)
        , input
            [ Attr.type_ "datetime-local"
            , Attr.placeholder "Date"
            , Attr.value modal.datetime
            , Html.Events.onInput DateTimeChanged
            , Html.Events.onBlur (MakeEvent (DateTimeUpdated modal.datetime))
            ]
            []
        , textarea
            [ Attr.placeholder "Notes"
            , Attr.value modal.notes
            , Html.Events.onInput NoteChanged
            , Html.Events.onBlur (MakeEvent (NotesUpdated modal.notes))
            ]
            []
        ]



-- -- VIEW HELPERS


dialog : List (Attribute msg) -> List (Html msg) -> Html msg
dialog attrs nodes =
    Html.node "dialog" attrs nodes



-- UPDATE


update :  Msg -> Model -> ( Model, Cmd Msg, Cmd Ev.Msg )
update msg model =
    case msg of
        DropdownMsg subMsg ->
            let
                ( updatedDropdown, cmd ) =
                    SD.update subMsg model.ingredients
            in
            ( { model | ingredients = updatedDropdown }, cmd, Cmd.none )

        MakeEvent event ->
            persistMealEvent (InteractionMealEvent event model.streamId) model

        NoteChanged notes ->
            ( { model | notes = notes }, Cmd.none, Cmd.none )

        DateTimeChanged datetime ->
            ( { model | datetime = datetime }, Cmd.none, Cmd.none )

        NoOp ->
            ( model, Cmd.none, Cmd.none )



-- ENCODE / PERSIST EVENTS


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
            , E.object [ ( "notes", E.string notes ) ]
            )

        DateTimeUpdated datetime ->
            ( "DateTimeUpdated"
            , E.object [ ( "datetime", E.string datetime ) ]
            )

        DeleteMeal ->
            ( "MealDeleted", E.null )


persistMealEvent : InteractionEvent -> Model -> ( Model, Cmd Msg, Cmd Ev.Msg )
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
            ( applyPersistingEvent ev meal, Cmd.none, Ev.persist eventData )

        AssignedMealId newStreamId ->
            ( { meal | streamId = newStreamId }, Cmd.none, Cmd.none )

applyPersistingEvent : Event -> Model -> Model
applyPersistingEvent ev model =
    case ev of
        IngredientAdded ingredient ->
            { model | ingredients = SD.addItem ingredient model.ingredients }

        IngredientRemoved ingredient ->
            { model | ingredients = SD.removeItem ingredient model.ingredients }

        NotesUpdated notes ->
            { model | notes = notes }

        DateTimeUpdated datetime ->
            { model | datetime = datetime }

        DeleteMeal ->
            model
