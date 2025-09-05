port module IDB exposing (upgrade, incrementID, writeData, requestReadData, Msg(..), Model, update)

import Dict exposing (Dict)
import Json.Encode as E
import Task
import Html.Attributes exposing (value)



-- DB upgrade


port upgradeDB : E.Value -> Cmd msg


upgrade : List ( String, List String ) -> Cmd msg
upgrade =
    upgradeDB << encodeUpgrade


encodeUpgrade : List ( String, List String ) -> E.Value
encodeUpgrade stores =
    E.list
        (\( name, indices ) ->
            E.object
                [ ( "name", E.string name )
                , ( "indices", E.list E.string indices )
                ]
        )
        stores



-- Example usage:
-- upgradeDB [ ("meals", ["datetime"]), ("ingredients", ["name"]) ]


port increment : E.Value -> Cmd msg
port write : E.Value -> Cmd msg
port requestRead : E.Value -> Cmd msg

incrementID : String -> String -> Cmd msg
incrementID store field =
    increment <| E.object
        [ ( "store", E.string store )
        , ( "field", E.string field )
        ]

writeData : String -> E.Value -> Cmd msg
writeData store value =
    write <| E.object
        [ ( "store", E.string store )
        , ( "value", value )
        ]

requestReadData : String -> String -> String -> Cmd msg
requestReadData store field callbackId =
    requestRead <| E.object
        [ ( "store", E.string store )
        , ( "field", E.string field )
        , ( "callbackId", E.string callbackId )
        ]

type Msg msg
    = GotDbResponse String E.Value
    | RegisterCallback String (E.Value -> msg) Bool


type alias Model msg =
    { callbacks : Dict String (E.Value -> msg, Bool)
    }

-- msg is the type of messages in the parent application, used for callbacks
update : Msg msg -> Model msg ->  ( Model msg, Cmd msg )
update msg model =
    case msg of
        GotDbResponse callbackId value ->
            case Dict.get callbackId model.callbacks of
                Just (callback, True) ->
                    ( { model | callbacks = Dict.remove callbackId model.callbacks }
                    , Task.perform (callback) (Task.succeed value)
                    )
                Just (callback, False) ->
                    ( model, Task.perform (callback) (Task.succeed value) )

                Nothing ->
                    ( model, Cmd.none )
        RegisterCallback callbackId callback once ->
            ( { model | callbacks = Dict.insert callbackId (callback, once) model.callbacks }
            , Cmd.none
            )