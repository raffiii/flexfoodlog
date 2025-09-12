port module Event exposing
    ( Envelope
    , EventData
    , Model
    , Msg
    , NewStreamEventData
    , PersistenceError(..)
    , RecievedEnvelope
    , decodeEnvelope
    , decodePersistenceError
    , decodePersistenceResult
    , decodeRecievedEnvelope
    , fillEnvelope
    , hydrateStream
    , initialModel
    , persist
    , persistNew
    , recieveEvents
    , recievePersistenceResults
    , update
    )

{-| This module provides a generic, stateful persistence layer for an event-sourced
system using IndexedDB.

It handles the generation of system-managed event metadata (ID, timestamp) and
provides a command-based API that abstracts away the complexities of state
management and persistence calls.

This module is ONLY responsible for writing events. The logic for reading,
decoding, and applying events to a domain model is handled by other modules in
the application.

This module requires two ports to be implemented in JavaScript:

port persistEventCmd : Json.Encode.Value -> Cmd msg
port persistNewEventCmd : Json.Encode.Value -> Cmd msg

These ports are responsible for performing the IndexedDB transaction and returning
a `Result PersistenceError Envelope` to a corresponding subscription port.

-}

import Json.Decode as D
import Json.Decode.Pipeline as P
import Json.Encode as E
import Random exposing (Seed)
import Task
import Time
import Ulid



--==============================================================================
-- PORTS
--==============================================================================


{-| Port to send an event for an existing stream to JavaScript for persistence.
The JS side should use the `expectedStreamPosition` for an optimistic concurrency check.
-}
port persistEventCmd : E.Value -> Cmd msg


{-| Port to send an event for a new stream to JavaScript for persistence.
The JS side should atomically generate a new stream ID and set the position to 1.
-}
port persistNewEventCmd : E.Value -> Cmd msg


{-| Port to send a stream ID to JavaScript to request all events for that stream.
-}
port hydrateStreamCmd : String -> Cmd msg


{-| Port to receive a list of hydrated events from JavaScript. This is typically
called on application startup or when a refresh is needed. The value should be
a JSON array of Envelope objects.
-}
port eventsSubscription : (D.Value -> msg) -> Sub msg


{-| Port to receive the result of a persistence operation from JavaScript.
The value should be a JSON object representing a `Result PersistenceError Envelope`.
-}
port persistenceResultSub : (D.Value -> msg) -> Sub msg



--==============================================================================
-- TYPES & TYPE ALIASES
--==============================================================================


{-| The internal state of the Events module, holding the seed for ULID generation.
This should be stored in your application's top-level model.
-}
type alias Model =
    { seed : Seed
    }


{-| Internal messages used by this module's update function to process persistence
requests. The parent application will wrap these messages.
-}
type Msg
    = InternalPersistRequest Time.Posix EventData
    | InternalPersistNewRequest Time.Posix NewStreamEventData


{-| The full, rich data structure for an event as it is stored in the database.
-}
type alias Envelope =
    { eventId : String
    , streamId : String
    , streamPosition : Int
    , timestamp : Time.Posix
    , schemaVersion : Int
    , type_ : String
    , payload : E.Value
    , correlationId : String
    , causationId : String
    }


type alias RecievedEnvelope =
    { eventId : String
    , streamId : String
    , streamPosition : Int
    , timestamp : Int
    , schemaVersion : Int
    , type_ : String
    , payload : D.Value
    , correlationId : String
    , causationId : String
    }


{-| The data a caller must provide to persist an event to an _existing_ stream.
-}
type alias EventData =
    { streamId : String
    , expectedStreamPosition : Int
    , type_ : String
    , schemaVersion : Int
    , payload : E.Value
    , correlationId : String
    , causationId : String
    }


{-| The data a caller must provide to persist the first event for a _new_ stream.
-}
type alias NewStreamEventData =
    { streamType : String
    , type_ : String
    , schemaVersion : Int
    , payload : E.Value
    , correlationId : String
    , causationId : String
    }


{-| Describes potential errors during the persistence process, typically received
from JavaScript via a port after a failed IndexedDB transaction.
-}
type PersistenceError
    = ConnectionError String
    | ConcurrencyError
    | TransactionError String
    | DecodingError



--==============================================================================
-- LIFECYCLE & UPDATE
--==============================================================================


{-| Creates the initial state for the Events module. You must provide an initial
`Ulid.Seed`, which is typically generated once when the application starts.
-}
initialModel : Seed -> Model
initialModel seed =
    Model seed


{-| The update function for the Events module. It processes internal messages,
generates ULIDs, and creates commands to be executed by JavaScript via ports.

The first argument is a function that wraps the final result of a persistence
operation into the parent application's `Msg` type.

-}
update :
    (Result PersistenceError Envelope -> msg)
    -> Msg
    -> Model
    -> ( Model, Cmd msg )
update onResponse msg model =
    case msg of
        InternalPersistRequest now eventData ->
            let
                ( ulid, newSeed ) =
                    Random.step (Ulid.ulidGenerator now) model.seed

                envelope =
                    { eventId = Ulid.toString ulid
                    , streamId = eventData.streamId
                    , streamPosition = eventData.expectedStreamPosition
                    , timestamp = now
                    , schemaVersion = eventData.schemaVersion
                    , type_ = eventData.type_
                    , payload = eventData.payload
                    , correlationId = eventData.correlationId
                    , causationId = eventData.causationId
                    }

                cmd =
                    persistEventCmd (encodeEnvelope envelope)
            in
            ( { model | seed = newSeed }, Cmd.map onResponse cmd )

        InternalPersistNewRequest now newStreamData ->
            let
                ( ulid, newSeed ) =
                    Random.step (Ulid.ulidGenerator now) model.seed

                -- The streamId and streamPosition will be determined by the JS side
                -- within the transaction. We send placeholders.
                envelope =
                    { eventId = Ulid.toString ulid
                    , streamId = newStreamData.streamType ++ ":?" -- Placeholder
                    , streamPosition = 1
                    , timestamp = now
                    , schemaVersion = newStreamData.schemaVersion
                    , type_ = newStreamData.type_
                    , payload = newStreamData.payload
                    , correlationId = newStreamData.correlationId
                    , causationId = newStreamData.causationId
                    }

                -- We also need to send the streamType to JS so it can query the right counter
                jsonToSend =
                    E.object
                        [ ( "streamType", E.string newStreamData.streamType )
                        , ( "envelope", encodeEnvelope envelope )
                        ]

                cmd =
                    persistNewEventCmd jsonToSend
            in
            ( { model | seed = newSeed }, Cmd.map onResponse cmd )



--==============================================================================
-- PUBLIC COMMANDS
--==============================================================================


{-| Creates a command to request all events for a specific stream from the
database. The result will be sent to the `eventsSubscription`.
-}
hydrateStream : String -> Cmd msg
hydrateStream streamId =
    hydrateStreamCmd streamId


{-| Creates a command to persist an event to an _existing_ stream.

You must provide the `expectedStreamPosition` to ensure data integrity.
The command will be processed by this module's `update` function.

-}
persist : EventData -> Cmd Msg
persist eventData =
    Task.perform (\now -> InternalPersistRequest now eventData) Time.now


{-| Creates a command to persist the first event for a _new_ stream.

Use this when creating a new aggregate. The system (via JavaScript) is
responsible for atomically generating the next available numeric ID for the
given `streamType`.

-}
persistNew : NewStreamEventData -> Cmd Msg
persistNew newStreamData =
    Task.perform (\now -> InternalPersistNewRequest now newStreamData) Time.now


fillEnvelope : EventData -> Cmd Msg
fillEnvelope eventData =
    Task.perform (\now -> InternalPersistRequest now eventData) Time.now



--==============================================================================
-- JSON ENCODERS
--==============================================================================


encodeEnvelope : Envelope -> E.Value
encodeEnvelope envelope =
    E.object
        [ ( "eventId", E.string envelope.eventId )
        , ( "streamId", E.string envelope.streamId )
        , ( "streamPosition", E.int envelope.streamPosition )
        , ( "timestamp", E.int (Time.posixToMillis envelope.timestamp) )
        , ( "schemaVersion", E.int envelope.schemaVersion )
        , ( "type_", E.string envelope.type_ )
        , ( "payload", envelope.payload )
        , ( "correlationId", E.string envelope.correlationId )
        , ( "causationId", E.string envelope.causationId )
        ]


decodeRecievedEnvelope : D.Decoder (List RecievedEnvelope)
decodeRecievedEnvelope =
    D.list
        (D.succeed RecievedEnvelope
            |> P.required "eventId" D.string
            |> P.required "streamId" D.string
            |> P.required "streamPosition" D.int
            |> P.required "timestamp" D.int
            |> P.required "schemaVersion" D.int
            |> P.required "type_" D.string
            |> P.required "payload" D.value
            |> P.required "correlationId" D.string
            |> P.required "causationId" D.string
        )


decodeEnvelope : D.Decoder Envelope
decodeEnvelope =
    D.succeed Envelope
        |> P.required "eventId" D.string
        |> P.required "streamId" D.string
        |> P.required "streamPosition" D.int
        |> P.required "timestamp" (D.map Time.millisToPosix D.int)
        |> P.required "schemaVersion" D.int
        |> P.required "type_" D.string
        |> P.required "payload" D.value
        |> P.required "correlationId" D.string
        |> P.required "causationId" D.string


decodePersistenceError : D.Decoder PersistenceError
decodePersistenceError =
    D.field "tag" D.string
        |> D.andThen
            (\tag ->
                let
                    argsDecoder =
                        D.field "args" (D.index 0 D.string)
                in
                case tag of
                    "ConnectionError" ->
                        D.map ConnectionError argsDecoder

                    "ConcurrencyError" ->
                        D.succeed ConcurrencyError

                    "TransactionError" ->
                        D.map TransactionError argsDecoder

                    _ ->
                        D.fail <| "Unknown error tag: " ++ tag
            )


decodePersistenceResult : D.Decoder a -> D.Decoder (Result PersistenceError a)
decodePersistenceResult valueDecoder =
    D.field "ok" D.bool
        |> D.andThen
            (\ok ->
                if ok then
                    D.map Ok (D.field "value" valueDecoder)

                else
                    D.map Err (D.field "value" decodePersistenceError)
            )



--==============================================================================
-- SUBSCRIPTIONS
--==============================================================================


recieveEvents : (Result D.Error (List RecievedEnvelope) -> msg) -> Sub msg
recieveEvents toMsg =
    eventsSubscription (D.decodeValue decodeRecievedEnvelope >> toMsg)


recievePersistenceResults : (Result PersistenceError Envelope -> msg) -> Sub msg
recievePersistenceResults toMsg =
    let
        decoded =
            D.decodeValue (decodePersistenceResult decodeEnvelope)

        toMsgFlat result =
            result
                |> Result.mapError (always DecodingError)
                |> Result.andThen identity
                |> toMsg
    in
    persistenceResultSub (decoded >> toMsgFlat)
