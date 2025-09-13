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
    | CloseDropdown
    | NoOp


update :  Msg -> Model -> ( Model, Cmd m )
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


makeItem : (String -> Bool -> msg) -> String -> Bool -> Html msg
makeItem toggleItem item isSelected =
    Html.li []
        [ Html.label []
            [ Html.input
                [ Html.Attributes.type_ "checkbox"
                , Html.Attributes.checked isSelected
                , Html.Events.onClick (toggleItem item isSelected)
                ]
                []
            , Html.text item
            ]
        ]


view : Model -> (Msg -> msg) -> (String -> msg) -> (String -> msg) -> Html msg
view model mapParent addItem removeItem =
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

        toggleItem item isSelected =
            if isSelected then
                removeItem item

            else
                addItem item

        keyDecoder =
            D.map
                (\kc ->
                    case kc of
                        -- Enter
                        13 ->
                            ( toggleItem model.searchTerm (List.member model.searchTerm model.selectedItems), True )

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
                    , Html.Attributes.placeholder "Meal Item..."
                    , Html.Attributes.value model.searchTerm
                    , Html.Events.onInput <| mapParent << SearchChanged
                    , Html.Events.preventDefaultOn "keydown" keyDecoder
                    ]
                    []
                ]

        isContained item =
            String.contains (String.toLower model.searchTerm) (String.toLower item)

        makeCheckedItem item =
            makeItem toggleItem item (List.member item model.selectedItems)
    in
    details detailsOpen
        [ summary [ Html.Events.onClick <| mapParent ToggleDropdown ]
            [ Html.text
                (if List.isEmpty model.selectedItems then
                    "Select items"

                 else
                    String.join ", " model.selectedItems
                )
            ]
        , Html.ul []
            (search
                :: (allItems
                        |> List.filter isContained
                        |> List.map makeCheckedItem
                   )
            )
        ]
