module Events exposing (Envelope, Msg, SmallEnvelope, State, buildEnvelope, update)

import Html.Attributes exposing (type_)
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



-- ev domain event (define more as needed)


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
    , type_ : String
    , payload : E.Value
    , correlationId : String
    , causationId : String
    }


type alias SmallEnvelope =
    { streamId : String
    , type_ : String
    , payload : E.Value
    , correlationId : String
    , causationId : String
    }



-- encodeEvent : Event ev -> ( String, E.Value )
-- encodeEvent ev =
--     case ev of
--         TitleChanged title ->
--             ( "TitleChanged"
--             , E.object [ ( "title", E.string title ) ]
--             )
--         ItemAdded name ->
--             ( "ItemAdded"
--             , E.object [ ( "name", E.string name ) ]
--             )
--         Modification f ev ->
--             f a
--         Undo ->
--             ( "Undo", E.null )
--         Redo ->
--             ( "Redo", E.null )


encodeEnvelope : Envelope -> E.Value
encodeEnvelope env =
    E.object
        [ ( "eventId", E.string env.eventId )
        , ( "streamId", E.string env.streamId )
        , ( "timestamp", E.string (String.fromInt (Time.posixToMillis env.timestamp)) )
        , ( "schemaVersion", E.int env.schemaVersion )
        , ( "type", E.string env.type_ )
        , ( "payload", env.payload )
        , ( "meta"
          , E.object
                [ ( "correlationId", E.string env.correlationId )
                , ( "causationId", E.string env.causationId )
                ]
          )
        ]


buildEnvelope : SmallEnvelope -> State -> Cmd Msg
buildEnvelope env state =
    Time.now
        |> Task.perform
            (\now ->
                let
                    ( ulid, newSeed ) =
                        step (Ulid.ulidGenerator now) state.seed

                    eventId =
                        Ulid.toString ulid
                in
                ( { state | seed = newSeed }
                , { eventId = eventId
                  , streamId = env.streamId
                  , timestamp = now
                  , schemaVersion = schemaVersion
                  , type_ = env.type_
                  , payload = env.payload
                  , correlationId = env.correlationId
                  , causationId = env.causationId
                  }
                )
                    |> BuiltEnvelope
            )
