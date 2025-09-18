module ViewSymptom exposing
    ( HydrationEvent
    , Model
    , Msg
    , Symptom
    , applyPersistanceResult
    , applySymptomEventList
    , init
    , parseSymptomEvent
    , update
    , view
    )

import EditSymptom as ES
import Event as Ev
import Html exposing (Html, button, div, fieldset, h1, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, class, style)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Set
import Symbols


type alias Model =
    { symptoms : List Symptom
    , dialog : Dialog
    }


type alias Symptom =
    { streamId : String
    , streamPosition : Int
    , category : String
    , datetime : String
    , severity : Maybe Int -- 1 to 10
    , notes : String
    }


type Dialog
    = Closed
    | Open ES.Model


type Msg
    = NoOp
    | OpenDialog Symptom
    | OpenNewSymptomDialog
    | CloseDialog
    | DeleteSymptom Symptom
    | EditSymptomMsg ES.Msg


type Event
    = SymptomCreated
    | CategoryUpdated String
    | DateTimeUpdated String
    | NotesUpdated String
    | SeverityUpdated Int
    | SymptomDeleted


type HydrationEvent
    = HydrationEvent Event String Int -- StreamId, StreamPosition


correlationId : String
correlationId =
    causationId


causationId : String
causationId =
    "view-symptom"


init : ( Model, Cmd Msg )
init =
    ( { symptoms = []
      , dialog = Closed
      }
    , Cmd.none
    )


emptySymptom : Symptom
emptySymptom =
    { streamId = ""
    , streamPosition = 1
    , category = ""
    , datetime = ""
    , severity = Nothing
    , notes = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Ev.Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, Cmd.none )

        OpenDialog symptom ->
            let
                categories =
                    model.symptoms
                        |> List.map (\s -> s.category)
                        |> Set.fromList
                        |> Set.toList

                ( editModel, cmd ) =
                    ES.initWithSymptom symptom categories
            in
            ( { model | dialog = Open editModel }
            , Cmd.map EditSymptomMsg cmd
            , Cmd.none
            )

        OpenNewSymptomDialog ->
            let
                categories =
                    model.symptoms
                        |> List.map (\s -> s.category)
                        |> Set.fromList
                        |> Set.toList

                ( editModel, cmd, evCmd ) =
                    ES.init categories
            in
            ( { model | dialog = Open editModel }
            , Cmd.map EditSymptomMsg cmd
            , evCmd
            )

        CloseDialog ->
            ( { model | dialog = Closed }
            , Cmd.none
            , Cmd.none
            )

        DeleteSymptom symptom ->
            let
                eventData =
                    { streamId = symptom.streamId
                    , expectedStreamPosition = symptom.streamPosition + 1
                    , type_ = "SymptomDeleted"
                    , schemaVersion = 1
                    , payload = E.object []
                    , correlationId = correlationId
                    , causationId = causationId
                    }
            in
            ( model
            , Cmd.none
            , Ev.persist eventData
            )

        EditSymptomMsg esmsg ->
            case model.dialog of
                Closed ->
                    ( model, Cmd.none, Cmd.none )

                Open emodel ->
                    let
                        ( updatedEmodel, cmd, evCmd ) =
                            ES.update esmsg emodel
                    in
                    ( { model | dialog = Open updatedEmodel }
                    , Cmd.map EditSymptomMsg cmd
                    , evCmd
                    )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Symptoms" ]
        , button [ onClick OpenNewSymptomDialog ] [ text "Add Symptom" ]
        , viewSymptomList model.symptoms
        , case model.dialog of
            Closed ->
                text ""

            Open emodel ->
                ES.view emodel EditSymptomMsg CloseDialog
        ]


viewSymptomList : List Symptom -> Html Msg
viewSymptomList symptoms =
    table [ class "striped" ]
        [ thead []
            [ tr []
                [ th [] [ text "Ingredients" ]
                , th [] [ text "DateTime" ]
                , th [] [ text "Severity" ]
                , th [] [ text "Notes" ]
                , th [] []
                ]
            ]
        , tbody [] (List.map viewSymptom symptoms)
        ]


viewSymptom : Symptom -> Html Msg
viewSymptom symptom =
    tr []
        [ td [] [ text symptom.category ]
        , td [] [ text symptom.datetime ]
        , td [] [ text (Maybe.withDefault "" (symptom.severity |> Maybe.map String.fromInt)) ]
        , td [] [ text symptom.notes ]
        , td []
            [ fieldset [ attribute "role" "group", style "margin" "0" ]
                [ button [ onClick (OpenDialog symptom), style "padding" "10 15" ] [ Symbols.editOutline ]
                , button [ class "secondary", onClick (DeleteSymptom symptom), style "padding" "15" ] [ Symbols.deleteOutline ]
                ]
            ]
        ]


parseSymptomEvent : Ev.Envelope -> Maybe HydrationEvent
parseSymptomEvent envelope =
    let
        decoder =
            D.map3 HydrationEvent
                (decodeSymptomEvent envelope.type_)
                (D.succeed envelope.streamId)
                (D.succeed envelope.streamPosition)
    in
    if envelope.streamId |> String.startsWith "symptom" |> not then
        Nothing

    else
        envelope.payload
            |> D.decodeValue decoder
            |> Debug.log "Decoded Symptom Event"
            |> Result.toMaybe


decodeSymptomEvent : String -> D.Decoder Event
decodeSymptomEvent type_ =
    case Debug.log "Decoding type" type_ of
        "CategoryUpdated" ->
            D.map CategoryUpdated (D.field "category" D.string)

        "SymptomCreated" ->
            D.succeed SymptomCreated

        "DateTimeUpdated" ->
            D.map DateTimeUpdated (D.field "datetime" D.string)

        "SymptomDeleted" ->
            D.succeed SymptomDeleted

        "CreateSymptom" ->
            D.succeed SymptomCreated

        "SeverityUpdated" ->
            D.map SeverityUpdated (D.field "severity" D.int)

        "NotesUpdated" ->
            D.map NotesUpdated (D.field "notes" (D.nullable D.string) |> D.map (Maybe.withDefault ""))

        _ ->
            D.fail ("Unknown event type: " ++ type_)


applySymptomEventList : HydrationEvent -> Model -> Model
applySymptomEventList (HydrationEvent ev streamId streamPosition) model =
    let
        symptoms =
            model.symptoms

        symptomWithId =
            List.filter (\s -> s.streamId == streamId) symptoms |> List.head

        updatedSymptom =
            if streamPosition == (symptomWithId |> Maybe.map .streamPosition |> Maybe.withDefault 0) + 1 then
                applyEvent ev symptomWithId streamId |> Maybe.map (\s -> { s | streamPosition = streamPosition })

            else
                symptomWithId

        newSymptoms =
            case ( symptomWithId, updatedSymptom ) of
                ( Nothing, Just newSymptom ) ->
                    newSymptom :: symptoms

                ( Just _, Just updated ) ->
                    updated :: List.filter (\s -> s.streamId /= streamId) symptoms

                ( Just _, Nothing ) ->
                    List.filter (\s -> s.streamId /= streamId) symptoms

                ( Nothing, Nothing ) ->
                    symptoms

        sortedNewSymptoms =
            newSymptoms
                |> List.sortBy .datetime
                |> List.reverse
    in
    { model | symptoms = sortedNewSymptoms }


applyEvent : Event -> Maybe Symptom -> String -> Maybe Symptom
applyEvent ev maybeSymptom streamId =
    case ( ev, maybeSymptom ) of
        ( SymptomCreated, Nothing ) ->
            Just { emptySymptom | streamId = streamId }

        ( SymptomCreated, Just _ ) ->
            maybeSymptom

        ( CategoryUpdated category, Just symptom ) ->
            Just { symptom | category = category }

        ( DateTimeUpdated datetime, Just symptom ) ->
            Just { symptom | datetime = datetime }

        ( SymptomDeleted, Just _ ) ->
            Nothing

        ( SeverityUpdated severity, Just symptom ) ->
            Just { symptom | severity = Just severity }

        ( NotesUpdated notes, Just symptom ) ->
            Just { symptom | notes = notes }

        ( _, Nothing ) ->
            Nothing


applyPersistanceResult : HydrationEvent -> Model -> Model
applyPersistanceResult events model =
    let
        newModel =
            applySymptomEventList events model
    in
    case ( events, model.dialog ) of
        ( HydrationEvent SymptomCreated streamId _, Open dialog ) ->
            { newModel | dialog = Open <| ES.applyPersistanceResult (ES.AssignedSymptomId streamId) dialog }

        _ ->
            newModel
