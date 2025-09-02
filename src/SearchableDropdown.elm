module SearchableDropdown exposing (Model, Msg, init, update, view)

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



-- UPDATE


type Msg
    = ToggleDropdown
    | SearchChanged String
    | ToggleItem String
    | CloseDropdown
    | KeyDown Int String
    | NoOp


update : Msg -> Model -> Model
update msg model =
    let
        toggleItem item =
            if List.member item model.selectedItems then
                List.filter ((/=) item) model.selectedItems

            else
                item :: model.selectedItems
    in
    case msg of
        ToggleDropdown ->
            { model | isOpen = not model.isOpen }

        SearchChanged term ->
            { model | searchTerm = term, isOpen = True }

        ToggleItem item ->
            let
                updatedSelected =
                    toggleItem item
            in
            { model | selectedItems = updatedSelected, searchTerm = "" }

        KeyDown kc term ->
            case kc of
                13 ->
                    -- KeyCode Enter
                    let
                        updatedSelected =
                            toggleItem term
                    in
                    { model | selectedItems = updatedSelected, searchTerm = "" }

                _ ->
                    model

        CloseDropdown ->
            { model | isOpen = False }

        NoOp ->
            model



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
