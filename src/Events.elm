module Events exposing (Envelope, Event(..), Msg, State, buildEnvelope, update)

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


type Event a
    = TitleChanged String
    | ItemAdded String
    | Modification (a -> ( String, E.Value )) a
    | Undo
    | Redo


type Msg a
    = NoOp
    | BuiltEnvelope ( State, Envelope a )


update : Msg a -> State -> ( State, Cmd msg )
update msg state =
    case msg of
        NoOp ->
            ( state, Cmd.none )

        BuiltEnvelope ( newState, envelope ) ->
            ( newState, Ports.saveEvent <| encodeEnvelope envelope )



-- Envelope: metadata + event


type alias Envelope a =
    { eventId : String -- ULID/UUID generated in Elm
    , streamId : String -- e.g. "todo:42"
    , timestamp : Posix
    , schemaVersion : Int
    , event : Event a
    , correlationId : String
    , causationId : String
    }


encodeEvent : Event a -> ( String, E.Value )
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

        Modification f a ->
            f a

        Undo ->
            ( "Undo", E.null )

        Redo ->
            ( "Redo", E.null )


encodeEnvelope : Envelope  a -> E.Value
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


buildEnvelope : String -> Event a -> String -> String -> State -> Cmd (Msg a)
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
