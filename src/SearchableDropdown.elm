module SearchableDropdown exposing (Model, Msg, init, setItems, update, view)

import Html exposing (Html, details, summary)
import Html.Attributes
import Html.Events
import Json.Decode as D



-- MODEL


type alias Model =
    { isOpen : Bool
    , searchTerm : String
    , selectedItems : List String
    , items : List String
    }


init : List String -> Model
init items =
    { isOpen = False
    , searchTerm = ""
    , selectedItems = []
    , items = items
    }

setItems : List String -> Model -> Model
setItems items model =
    { model | items = items }



-- UPDATE


type Msg
    = ToggleDropdown
    | SearchChanged String
    | ToggleItem String
    | CloseDropdown
    | KeyDown Int String
    | NoOp


update : (List String -> List String -> Cmd m) -> Msg -> Model -> ( Model, Cmd m )
update onUpdate msg model =
    let
        toggleItem item =
            if List.member item model.selectedItems then
                List.filter ((/=) item) model.selectedItems

            else
                item :: model.selectedItems

        toggleUpdate item =
            let
                updatedSelected =
                    toggleItem item
            in
            ( { model | selectedItems = updatedSelected, searchTerm = "" }, onUpdate (Debug.log "previous items" model.selectedItems) (Debug.log "new items" updatedSelected) )
    in
    case msg of
        ToggleDropdown ->
            ( { model | isOpen = not model.isOpen }, Cmd.none )

        SearchChanged term ->
            ( { model | searchTerm = term, isOpen = True }, Cmd.none )

        ToggleItem item ->
            toggleUpdate item

        KeyDown kc term ->
            case kc of
                13 ->
                    -- KeyCode Enter
                    toggleUpdate <| Debug.log "Enter on: " term

                27 ->
                    -- KeyCode Escape
                    ( { model | isOpen = False }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CloseDropdown ->
            ( { model | isOpen = False }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


makeItem : String -> Bool -> Html Msg
makeItem item isSelected =
    Html.li []
        [ Html.label []
            [ Html.input
                [ Html.Attributes.type_ "checkbox"
                , Html.Attributes.checked isSelected
                , Html.Events.onClick (ToggleItem item)
                ]
                []
            , Html.text item
            ]
        ]


view : Model -> Html Msg
view model =
    let
        combinedItems =
            model.selectedItems ++ (model.items |> List.filter (\item -> not (List.member item model.selectedItems)))

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

        search =
            Html.li []
                [ Html.input
                    [ Html.Attributes.type_ "search"
                    , Html.Attributes.placeholder "Meal Item..."
                    , Html.Attributes.value model.searchTerm
                    , Html.Events.onInput SearchChanged
                    , Html.Events.preventDefaultOn "keydown" <|
                        D.map
                            (\kc ->
                                case kc of
                                    -- Enter
                                    13 ->
                                        ( ToggleItem model.searchTerm, True )

                                    -- Escape
                                    27 ->
                                        ( CloseDropdown, True )

                                    -- Other keys
                                    _ ->
                                        ( NoOp, False )
                            )
                            Html.Events.keyCode
                    ]
                    []
                ]
    in
    details detailsOpen
        [ summary [ Html.Events.onClick ToggleDropdown ]
            [ Html.text
                (if List.isEmpty model.selectedItems then
                    "Select items"

                 else
                    String.join ", " model.selectedItems
                )
            ]
        , Html.ul []
            (search
                :: (List.filter (\item -> String.contains (String.toLower model.searchTerm) (String.toLower item)) allItems
                        |> List.map (\item -> makeItem item (List.member item model.selectedItems))
                   )
            )
        ]
