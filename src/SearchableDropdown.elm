module SearchableDropdown exposing (Model, Msg, init, update, view)

import Html exposing (Html, details, summary)
import Html.Attributes
import Html.Events



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


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleDropdown ->
            { model | isOpen = not model.isOpen }

        SearchChanged term ->
            { model | searchTerm = term, isOpen = True }

        ToggleItem item ->
            let
                updatedSelected =
                    if List.member item model.selectedItems then
                        List.filter ((/=) item) model.selectedItems

                    else
                        item :: model.selectedItems
            in
            { model | selectedItems = updatedSelected }

        CloseDropdown ->
            { model | isOpen = False }



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
        allItems =
            if model.searchTerm == "" then
                model.items

            else
                model.searchTerm :: model.items

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
