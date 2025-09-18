module DynamicSelect exposing (..)

import Html exposing (Html, summary)
import Html.Attributes
import Html.Events
import Json.Decode as D


type alias Model =
    { isOpen : Bool
    , searchTerm : String
    , selectedItem : Maybe String
    , items : List String
    }


init : List String -> Model
init items =
    { isOpen = False
    , searchTerm = ""
    , selectedItem = Nothing
    , items = items
    }


setSelectedItem : Maybe String -> Model -> Model
setSelectedItem item model =
    { model | selectedItem = item, searchTerm = "" }



-- UPDATE


type Msg
    = ToggleDropdown
    | SearchChanged String
    | CloseDropdown
    | NoOp


update : Msg -> Model -> ( Model, Cmd m )
update msg model =
    case msg of
        ToggleDropdown ->
            ( { model | isOpen = not model.isOpen }, Cmd.none )

        SearchChanged term ->
            ( { model | searchTerm = term, isOpen = True }, Cmd.none )

        CloseDropdown ->
            ( { model | isOpen = False }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


makeItem : (String -> msg) -> String -> Bool-> Html msg
makeItem onChange item isSelected =
    Html.li
        [ Html.Attributes.class "dropdown-item"
        , Html.Events.onClick (onChange item)
        , Html.Attributes.style "font-weight" (if isSelected then "bold" else "normal")
        ]
        [ Html.text item ]


view : Model -> (Msg -> msg) -> (String -> msg) -> Html msg
view model mapParent onChange =
    let
        combinedItems =
            case model.selectedItem of
                Just sel ->
                    sel :: (model.items |> List.filter (\item -> item /= sel))

                Nothing ->
                    model.items

        allItems =
            if model.searchTerm == "" then
                combinedItems

            else if List.member model.searchTerm model.items then
                combinedItems

            else
                model.searchTerm :: combinedItems

        detailsOpen =
            if model.isOpen then
                [ Html.Attributes.attribute "open" "", Html.Attributes.class "dropdown" ]

            else
                [ Html.Attributes.class "dropdown" ]

        keyDecoder =
            D.map
                (\kc ->
                    case kc of
                        -- Enter
                        13 ->
                            ( onChange model.searchTerm, True )

                        -- Escape
                        27 ->
                            ( mapParent CloseDropdown, True )

                        -- Other keys
                        _ ->
                            ( mapParent NoOp, False )
                )
                Html.Events.keyCode

        search =
            Html.li []
                [ Html.input
                    [ Html.Attributes.type_ "search"
                    , Html.Attributes.placeholder "Select Symptom..."
                    , Html.Attributes.value model.searchTerm
                    , Html.Events.onInput <| mapParent << SearchChanged
                    , Html.Events.preventDefaultOn "keydown" keyDecoder
                    ]
                    []
                ]

        isContained item =
            String.contains (String.toLower model.searchTerm) (String.toLower item)

        makeCheckedItem item =
            makeItem onChange item
    in
    details detailsOpen
        [ summary [ Html.Events.onClick <| mapParent ToggleDropdown ]
            [ Html.text
                (case model.selectedItem of
                    Nothing ->
                        "Select item"

                    Just item ->
                        item
                )
            ]
        , Html.ul []
            (search
                :: (allItems
                        |> List.filter isContained
                        |> List.map (\i -> makeCheckedItem i (model.selectedItem == Just i))
                   )
            )
        ]


details : List (Html.Attribute msg) -> List (Html msg) -> Html msg
details attrs nodes =
    Html.node "details" attrs nodes
