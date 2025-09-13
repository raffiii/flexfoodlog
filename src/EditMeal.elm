module EditMeal exposing (Meal, Model, Msg, init, initWithMeal, update, view, emptyMeal)

import Event as Ev
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events
import Json.Encode as E
import SearchableDropdown as SD



-- Model


type alias Meal =
    { streamId : String
    , streamPosition : Int
    , ingredients : List String
    , datetime : String
    , notes : String
    }


type alias Model =
    { meal : Meal
    , dropdown : SD.Model
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


emptyMeal : Meal
emptyMeal =
    { streamId = ""
    , streamPosition = 0
    , ingredients = []
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
    ( Model emptyMeal (SD.init ingredientsList)
    , Ev.persistNew eventData |> Cmd.map (\_ -> NoOp)
    )


initWithMeal : Meal -> List String -> ( Model, Cmd Msg )
initWithMeal meal ingredientsList =
    ( Model meal (SD.init ingredientsList |> SD.setItems meal.ingredients), Cmd.none )



-- VIEW


view : Model -> Html Msg
view modal =
    dialog [ Attr.attribute "open" "" ]
        [ article []
            [ header []
                [ button [ Attr.attribute "aria-label" "Close", Attr.rel "prev" ] []
                , p [] [ strong [] [ text "Edit Meal" ] ]
                ]
            , viewForm modal
            ]
        ]


viewForm : Model -> Html Msg
viewForm modal =
    p []
        [ SD.view modal.dropdown DropdownMsg (MakeEvent << IngredientAdded) (MakeEvent << IngredientRemoved)
        , input
            [ Attr.type_ "datetime-local"
            , Attr.placeholder "Date"
            , Attr.value modal.meal.datetime
            , Html.Events.onInput DateTimeChanged
            , Html.Events.onBlur (MakeEvent (DateTimeUpdated modal.meal.datetime))
            ]
            []
        , textarea
            [ Attr.placeholder "Notes"
            , Attr.value modal.meal.notes
            , Html.Events.onInput NoteChanged
            , Html.Events.onBlur (MakeEvent (NotesUpdated modal.meal.notes))
            ]
            []
        ]



-- -- VIEW HELPERS


dialog : List (Attribute msg) -> List (Html msg) -> Html msg
dialog attrs nodes =
    Html.node "dialog" attrs nodes



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        oldMeal =
            model.meal
    in
    case msg of
        DropdownMsg subMsg ->
            let
                ( updatedDropdown, cmd ) =
                    SD.update subMsg model.dropdown
            in
            ( { model | dropdown = updatedDropdown }, cmd )

        MakeEvent event ->
            let
                ( cmd, meal ) =
                    persistMealEvent (InteractionMealEvent event oldMeal.streamId) oldMeal
            in
            ( { model | meal = meal }, cmd )

        NoteChanged notes ->
            ( { model | meal = { oldMeal | notes = notes } }, Cmd.none )

        DateTimeChanged datetime ->
            ( { model | meal = { oldMeal | datetime = datetime } }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



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


persistMealEvent : InteractionEvent -> Meal -> ( Cmd Msg, Meal )
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
            ( Ev.persist eventData |> Cmd.map (\_ -> NoOp), meal )

        AssignedMealId newStreamId ->
            ( Cmd.none, { meal | streamId = newStreamId } )
