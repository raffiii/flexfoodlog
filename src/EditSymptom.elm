module EditSymptom exposing (InteractionEvent(..), Model, Msg, applyPersistanceResult, init, initWithSymptom, update, view)

import DynamicSelect as DS
import Event as Ev
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Time exposing (Month(..))



-- Model


type alias Symptom =
    { streamId : String
    , streamPosition : Int
    , category : DS.Model
    , datetime : String
    , severity : Maybe Int -- 1 to 10
    , notes : String
    }


type Model
    = Creating (List String)
    | Existing Symptom



-- MESSAGES


type Msg
    = DateTimeChanged String
    | NotesChanged String
    | MakeEvent Event
    | DropdownMsg DS.Msg


type Event
    = CategoryChanged String
    | NotesUpdated String
    | SeverityUpdated (Maybe Int)
    | DateTimeUpdated String
    | DeleteSymptom


type InteractionEvent
    = InteractionSymptomEvent Event String -- StreamId
    | AssignedSymptomId String -- StreamId



-- CONSTANTS


correlationId : String
correlationId =
    causationId


causationId : String
causationId =
    "symptom-modal-1"



-- INIT


init : List String -> ( Model, Cmd Msg, Cmd Ev.Msg )
init categoriesList =
    let
        eventData =
            { streamType = "symptom"
            , type_ = "CreateSymptom"
            , schemaVersion = 1
            , payload = E.object []
            , correlationId = correlationId
            , causationId = causationId
            }
    in
    ( Creating categoriesList
    , Cmd.none
    , Ev.persistNew eventData
    )


applyPersistanceResult : InteractionEvent -> Model -> Model
applyPersistanceResult event model =
    case Debug.log "applying" ( event, model ) of
        ( AssignedSymptomId streamId, Creating categoriesList ) ->
            Existing <|
                Symptom
                    streamId
                    1
                    (DS.init categoriesList)
                    ""
                    Nothing
                    ""

        _ ->
            model


initWithSymptom :
    { streamId : String
    , streamPosition : Int
    , category : String
    , datetime : String
    , severity : Maybe Int -- 1 to 10
    , notes : String
    }
    -> List String
    -> ( Model, Cmd Msg )
initWithSymptom symptom categoriesList =
    ( Existing <|
        Symptom
            symptom.streamId
            symptom.streamPosition
            (DS.init categoriesList |> DS.setSelectedItem (Just symptom.category))
            symptom.datetime
            symptom.severity
            symptom.notes
    , Cmd.none
    )



-- VIEW


view : Model -> (Msg -> msg) -> msg -> Html msg
view modal mapMsg onClose =
    let
        content =
            case modal of
                Creating _ ->
                    text "Creating new symptom..."

                Existing symptom ->
                    viewForm symptom
    in
    dialog [ Attr.attribute "open" "" ]
        [ article []
            [ header []
                [ button [ Attr.attribute "aria-label" "Close", Attr.rel "prev", onClick onClose ] []
                , p [] [ strong [] [ text "Edit Symptom" ] ]
                ]
            , Html.map mapMsg content
            ]
        ]


viewForm : Symptom -> Html Msg
viewForm modal =
    p []
        [ DS.view modal.category DropdownMsg (MakeEvent << CategoryChanged)
        , input
            [ Attr.type_ "datetime-local"
            , Attr.placeholder "Date"
            , Attr.value modal.datetime
            , Html.Events.onInput DateTimeChanged
            , Html.Events.onBlur (MakeEvent (DateTimeUpdated modal.datetime))
            ]
            []
        , viewSeverity modal.severity
        , textarea
            [ Attr.placeholder "Notes"
            , Attr.value modal.notes
            , Html.Events.onInput NotesChanged
            , Html.Events.onBlur (MakeEvent (NotesUpdated modal.notes))
            ]
            []
        ]


viewSeverity : Maybe Int -> Html Msg
viewSeverity severity =
    let
        attrs =
            [ Attr.type_ "range"
            , Attr.min "1"
            , Attr.max "10"
            , Html.Events.on "blur" (D.map (MakeEvent << SeverityUpdated << String.toInt << Debug.log "got int") (D.at [ "target", "value" ] D.string))
            ]

        slider =
            case severity of
                Just s ->
                    input (Attr.value (String.fromInt s) :: attrs) []

                Nothing ->
                    input attrs []
    in
    Html.fieldset [ Attr.attribute "role" "group" ]
        [ Html.label [] [ text "Severity (1-10)" ]
        , slider
        , case severity of
            Just s ->
                text (" " ++ String.fromInt s)

            Nothing ->
                text " Not set"
        ]



-- -- VIEW HELPERS


dialog : List (Attribute msg) -> List (Html msg) -> Html msg
dialog attrs nodes =
    Html.node "dialog" attrs nodes



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Ev.Msg )
update msg model =
    case ( msg, model ) of
        ( MakeEvent event, Existing symptom ) ->
            let
                ( newSymptom, cmd, evCmd ) =
                    persistSymptomEvent (InteractionSymptomEvent event symptom.streamId) symptom
            in
            ( Existing { newSymptom | streamPosition = symptom.streamPosition + 1 }, cmd, evCmd )

        ( DateTimeChanged datetime, Existing symptom ) ->
            ( Existing { symptom | datetime = datetime }, Cmd.none, Cmd.none )

        ( NotesChanged notes, Existing symptom ) ->
            ( Existing { symptom | notes = notes }, Cmd.none, Cmd.none )

        ( DropdownMsg subMsg, Existing symptom ) ->
            let
                ( updatedDropdown, cmd ) =
                    DS.update subMsg symptom.category
            in
            ( Existing { symptom | category = updatedDropdown }, cmd, Cmd.none )

        ( _, Creating _ ) ->
            ( model, Cmd.none, Cmd.none )



-- ENCODE / PERSIST EVENTS


encodeSymptomEvent : Event -> ( String, E.Value )
encodeSymptomEvent ev =
    case ev of
        CategoryChanged category ->
            ( "CategoryUpdated"
            , E.object [ ( "category", E.string category ) ]
            )

        NotesUpdated notes ->
            ( "NotesUpdated"
            , E.object [ ( "notes", E.string notes ) ]
            )

        SeverityUpdated severity ->
            ( "SeverityUpdated"
            , E.object
                [ ( "severity"
                  , severity
                        |> Maybe.map E.int
                        |> Maybe.withDefault E.null
                  )
                ]
            )

        DateTimeUpdated datetime ->
            ( "DateTimeUpdated"
            , E.object [ ( "datetime", E.string datetime ) ]
            )

        DeleteSymptom ->
            ( "SymptomDeleted", E.null )


persistSymptomEvent : InteractionEvent -> Symptom -> ( Symptom, Cmd Msg, Cmd Ev.Msg )
persistSymptomEvent symptomEvent symptom =
    case symptomEvent of
        InteractionSymptomEvent ev streamId ->
            let
                ( eventType, payload ) =
                    encodeSymptomEvent ev

                eventData =
                    { streamId = streamId
                    , expectedStreamPosition = symptom.streamPosition + 1
                    , type_ = eventType
                    , schemaVersion = 1
                    , payload = payload
                    , correlationId = correlationId
                    , causationId = causationId
                    }
            in
            ( applyPersistingEvent ev symptom, Cmd.none, Ev.persist eventData )

        AssignedSymptomId newStreamId ->
            ( { symptom | streamId = newStreamId }, Cmd.none, Cmd.none )


applyPersistingEvent : Event -> Symptom -> Symptom
applyPersistingEvent ev model =
    case ev of
        DateTimeUpdated datetime ->
            { model | datetime = datetime }

        DeleteSymptom ->
            model

        NotesUpdated notes ->
            { model | notes = notes }

        SeverityUpdated severity ->
            { model | severity = severity }

        CategoryChanged category ->
            { model | category = DS.setSelectedItem (Just category) model.category }



