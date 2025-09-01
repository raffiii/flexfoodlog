module Events exposing (Envelope, Event(..), buildEnvelope, update, State, Msg)

import Json.Encode as E
import Ports
import Random exposing (Seed, step)
import Task
import Time exposing (Posix)
import Ulid

schemaVersion : Int
schemaVersion =
    1

type alias State =
    { seed : Seed
    }



-- A domain event (define more as needed)


type Event
    = TitleChanged String
    | ItemAdded String


type Msg
    = NoOp
    | BuiltEnvelope ( State, Envelope )


update : Msg -> State -> ( State, Cmd msg )
update msg state =
    case msg of
        NoOp ->
            ( state, Cmd.none )

        BuiltEnvelope ( newState, envelope ) ->
            ( newState, Ports.saveEvent <| encodeEnvelope envelope )



-- Envelope: metadata + event


type alias Envelope =
    { eventId : String -- ULID/UUID generated in Elm
    , streamId : String -- e.g. "todo:42"
    , timestamp : Posix
    , schemaVersion : Int
    , event : Event
    , correlationId : String
    , causationId : String
    }


encodeEvent : Event -> ( String, E.Value )
encodeEvent ev =
    case ev of
        TitleChanged title ->
            ( "TitleChanged"
            , E.object [ ( "title", E.string title ) ]
            )

        ItemAdded name ->
            ( "ItemAdded"
            , E.object [ ( "name", E.string name ) ]
            )


encodeEnvelope : Envelope -> E.Value
encodeEnvelope env =
    let
        ( eventType, payload ) =
            encodeEvent env.event
    in
    E.object
        [ ( "eventId", E.string env.eventId )
        , ( "streamId", E.string env.streamId )
        , ( "timestamp", E.string (String.fromInt (Time.posixToMillis env.timestamp)) )
        , ( "schemaVersion", E.int env.schemaVersion )
        , ( "type", E.string eventType )
        , ( "payload", payload )
        , ( "meta"
          , E.object
                [ ( "correlationId", E.string env.correlationId )
                , ( "causationId", E.string env.causationId )
                ]
          )
        ]


buildEnvelope : String -> Event -> String -> String -> State -> Cmd Msg
buildEnvelope streamId event correlationId causationId state =
    Time.now
        |> Task.perform
            (\now ->
                let
                    ( ulid, newSeed ) =
                        step (Ulid.ulidGenerator now) state.seed

                    eventId =
                        Ulid.toString ulid
                in
                ( { seed = newSeed }
                , { eventId = eventId
                  , streamId = streamId
                  , timestamp = now
                  , schemaVersion = schemaVersion
                  , event = event
                  , correlationId = correlationId
                  , causationId = causationId
                  }
                )
                    |> BuiltEnvelope
            )
