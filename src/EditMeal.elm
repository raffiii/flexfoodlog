module EditMeal exposing (Model, Msg, InteractionEvent(..), init, initWithMeal, update, view,applyPersistanceResult)

import Event as Ev
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Json.Encode as E
import SearchableDropdown as SD



-- Model


type alias Meal =
    { streamId : String
    , streamPosition : Int
    , ingredients : SD.Model
    , datetime : String
    , notes : String
    }


type Model
    = Creating (List String)
    | Existing Meal



-- MESSAGES


type Msg
    = DropdownMsg SD.Msg
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


init : List String -> ( Model, Cmd Msg, Cmd Ev.Msg )
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
    ( Creating ingredientsList
    , Cmd.none
    , Ev.persistNew eventData
    )

applyPersistanceResult : InteractionEvent -> Model -> Model
applyPersistanceResult event model =
    case ( event, model ) of
        ( AssignedMealId streamId, Creating ingredientsList ) ->
            Existing
                { streamId = streamId
                , streamPosition = 1
                , ingredients = SD.init ingredientsList
                , datetime = ""
                , notes = ""
                }

        _ ->
            model 


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
    ( Existing <| Meal meal.streamId meal.streamPosition (SD.init ingredientsList |> SD.setSelectedItems meal.ingredients) meal.datetime meal.notes, Cmd.none )



-- VIEW


view : Model -> (Msg -> msg) -> msg -> Html msg
view modal mapMsg onClose =
    let
        content =
            case modal of
                Creating _ ->
                    text "Creating new meal..."

                Existing meal ->
                    viewForm meal
    in
    
    dialog [ Attr.attribute "open" "" ]
        [ article []
            [ header []
                [ button [ Attr.attribute "aria-label" "Close", Attr.rel "prev", onClick onClose ] []
                , p [] [ strong [] [ text "Edit Meal" ] ]
                ]
            , Html.map mapMsg content
            ]
        ]


viewForm : Meal -> Html Msg
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


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Ev.Msg )
update msg model =
    case ( msg, model ) of
        ( DropdownMsg subMsg, Existing meal ) ->
            let
                ( updatedDropdown, cmd ) =
                    SD.update subMsg meal.ingredients
            in
            ( Existing { meal | ingredients = updatedDropdown }, cmd, Cmd.none )

        ( MakeEvent event, Existing meal ) ->
            let
                ( newMeal, cmd, evCmd ) =
                    persistMealEvent (InteractionMealEvent event meal.streamId) meal
            in
            ( Existing {newMeal| streamPosition = meal.streamPosition + 1 }, cmd, evCmd )

        ( NoteChanged notes, Existing meal ) ->
            ( Existing { meal | notes = notes }, Cmd.none, Cmd.none )

        ( DateTimeChanged datetime, Existing meal ) ->
            ( Existing { meal | datetime = datetime }, Cmd.none, Cmd.none )


        _ ->
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


persistMealEvent : InteractionEvent -> Meal -> ( Meal, Cmd Msg, Cmd Ev.Msg )
persistMealEvent mealEvent meal =
    case mealEvent of
        InteractionMealEvent ev streamId ->
            let
                ( eventType, payload ) =
                    encodeMealEvent ev

                eventData =
                    { streamId = streamId
                    , expectedStreamPosition =  meal.streamPosition + 1
                    , type_ = eventType
                    , schemaVersion = 1
                    , payload = payload
                    , correlationId = correlationId
                    , causationId = causationId
                    }
            in
            ( applyPersistingEvent ev meal, Cmd.none, Ev.persist eventData )

        AssignedMealId newStreamId ->
            ( { meal | streamId = newStreamId}, Cmd.none, Cmd.none )


applyPersistingEvent : Event -> Meal -> Meal
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
