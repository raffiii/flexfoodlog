module ViewMeal exposing
    ( HydrationEvent
    , Meal
    , Model
    , Msg
    , applyMealEventList
    , dummyInit
    , init
    , parseMealEvent
    , update
    , view
    )

import EditMeal as EM
import Event as Ev
import Html exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as D


type alias Model =
    { meals : List EM.Meal
    , dialog : Dialog
    }


type alias Meal =
    EM.Meal


type Dialog
    = Closed
    | Open EM.Model


type Msg
    = NoOp
    | OpenDialog EM.Meal
    | OpenNewMealDialog
    | CloseDialog
    | EditMealMsg EM.Msg


type Event
    = MealCreated
    | IngredientAdded String
    | IngredientRemoved String
    | NotesUpdated String
    | DateTimeUpdated String
    | MealDeleted


type HydrationEvent
    = HydrationEvent Event String -- StreamId


init : ( Model, Cmd Msg )
init =
    ( { meals = []
      , dialog = Closed
      }
    , Cmd.none
    )


dummyInit : ( Model, Cmd Msg )
dummyInit =
    let
        meal1 =
            { streamId = "meal-1"
            , streamPosition = 1
            , ingredients = [ "Chicken", "Rice" ]
            , datetime = "2024-10-01 12:00"
            , notes = "Delicious!"
            }

        meal2 =
            { streamId = "meal-2"
            , streamPosition = 1
            , ingredients = [ "Beef", "Potatoes" ]
            , datetime = "2024-10-02 18:30"
            , notes = "Hearty meal."
            }
    in
    ( { meals = [ meal1, meal2 ]
      , dialog = Closed
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OpenDialog meal ->
            let
                ingredients =
                    List.concatMap (\m -> m.ingredients) model.meals

                ( editModel, cmd ) =
                    EM.initWithMeal meal ingredients
            in
            ( { model | dialog = Open editModel }
            , Cmd.map EditMealMsg cmd
            )

        OpenNewMealDialog ->
            let
                ingredients =
                    List.concatMap (\m -> m.ingredients) model.meals

                ( editModel, cmd ) =
                    EM.init ingredients
            in
            ( { model | dialog = Open editModel }
            , Cmd.map EditMealMsg cmd
            )

        CloseDialog ->
            ( { model | dialog = Closed }
            , Cmd.none
            )

        EditMealMsg emsg ->
            case model.dialog of
                Closed ->
                    ( model, Cmd.none )

                Open emodel ->
                    let
                        ( updatedEmodel, cmd ) =
                            EM.update emsg emodel
                    in
                    ( { model | dialog = Open updatedEmodel }
                    , Cmd.map EditMealMsg cmd
                    )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Meals" ]
        , button [ onClick OpenNewMealDialog ] [ text "Add Meal" ]
        , ul [] (List.map viewMeal model.meals)
        , case model.dialog of
            Closed ->
                text ""

            Open emodel ->
                EM.view emodel |> Html.map EditMealMsg
        ]


viewMeal : EM.Meal -> Html Msg
viewMeal meal =
    li []
        [ div []
            [ text ("Meal at " ++ meal.datetime)
            , button [ onClick (OpenDialog meal) ] [ text "Edit" ]
            ]
        ]


parseMealEvent : Ev.RecievedEnvelope -> Maybe HydrationEvent
parseMealEvent envelope =
    envelope.payload
        |> D.decodeValue (decodeMealEvent envelope.type_)
        |> Result.toMaybe


decodeMealEvent : String -> D.Decoder HydrationEvent
decodeMealEvent type_ =
    let
        mapMealEvent decoder =
            D.map2 HydrationEvent decoder (D.field "streamId" D.string)
    in
    case type_ of
        "IngredientAdded" ->
            mapMealEvent
                (D.map IngredientAdded (D.field "name" D.string))

        "IngredientRemoved" ->
            mapMealEvent
                (D.map IngredientRemoved (D.field "name" D.string))

        "MealCreated" ->
            mapMealEvent <| D.succeed MealCreated

        "NotesUpdated" ->
            mapMealEvent <|
                D.map NotesUpdated (D.field "notes" (D.nullable D.string) |> D.map (Maybe.withDefault ""))

        "DateTimeUpdated" ->
            mapMealEvent <|
                D.map DateTimeUpdated (D.field "datetime" D.string)

        "MealDeleted" ->
            mapMealEvent <|
                D.succeed MealDeleted

        _ ->
            D.fail ("Unknown event type: " ++ type_)


applyMealEventList : HydrationEvent -> Model -> Model
applyMealEventList (HydrationEvent ev streamId) model =
    let
        meals =
            model.meals

        mealWithId =
            List.filter (\m -> m.streamId == streamId) meals |> List.head

        updatedMeal =
            applyEvent ev mealWithId

        newMeals =
            case ( mealWithId, updatedMeal ) of
                ( Nothing, Just newMeal ) ->
                    newMeal :: meals

                ( Just _, Just updated ) ->
                    updated :: List.filter (\m -> m.streamId /= streamId) meals

                ( Just _, Nothing ) ->
                    List.filter (\m -> m.streamId /= streamId) meals

                ( Nothing, Nothing ) ->
                    meals
    in
    { model | meals = newMeals }


applyEvent : Event -> Maybe Meal -> Maybe Meal
applyEvent ev maybeMeal =
    case ( ev, maybeMeal ) of
        ( MealCreated, Nothing ) ->
            Just EM.emptyMeal

        ( MealCreated, Just _ ) ->
            maybeMeal

        ( IngredientAdded ingredient, Just meal ) ->
            Just { meal | ingredients = ingredient :: meal.ingredients }

        ( IngredientRemoved ingredient, Just meal ) ->
            Just { meal | ingredients = List.filter (\i -> i /= ingredient) meal.ingredients }

        ( NotesUpdated notes, Just meal ) ->
            Just { meal | notes = notes }

        ( DateTimeUpdated datetime, Just meal ) ->
            Just { meal | datetime = datetime }

        ( MealDeleted, Just _ ) ->
            Nothing

        ( _, Nothing ) ->
            Nothing
