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
import Html exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as D
import Set


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
        , ul [] (List.map viewSymptom <| model.symptoms)
        , case model.dialog of
            Closed ->
                text ""

            Open emodel ->
                ES.view emodel EditSymptomMsg CloseDialog
        ]


viewSymptom : Symptom -> Html Msg
viewSymptom symptom =
    li []
        [ div []
            [ text ("Symptom at " ++ symptom.datetime)
            , button [ onClick (OpenDialog symptom) ] [ text "Edit" ]
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
    envelope.payload
        |> D.decodeValue decoder
        |> Result.toMaybe


decodeSymptomEvent : String -> D.Decoder Event
decodeSymptomEvent type_ =
    case type_ of
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
    in
    { model | symptoms = newSymptoms }


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
