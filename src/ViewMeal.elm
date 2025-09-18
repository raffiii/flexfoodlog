module ViewMeal exposing
    ( HydrationEvent
    , Meal
    , Model
    , Msg
    , applyMealEventList
    , applyPersistanceResult
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
    { meals : List Meal
    , dialog : Dialog
    }


type alias Meal =
    { streamId : String
    , streamPosition : Int
    , ingredients : List String
    , datetime : String
    , notes : String
    }


type Dialog
    = Closed
    | Open EM.Model


type Msg
    = NoOp
    | OpenDialog Meal
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
    = HydrationEvent Event String Int -- StreamId, StreamPosition


init : ( Model, Cmd Msg )
init =
    ( { meals = []
      , dialog = Closed
      }
    , Cmd.none
    )


emptyMeal : Meal
emptyMeal =
    { streamId = ""
    , streamPosition = 1
    , ingredients = []
    , datetime = ""
    , notes = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Ev.Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, Cmd.none )

        OpenDialog meal ->
            let
                ingredients =
                    List.concatMap (\m -> m.ingredients) model.meals

                ( editModel, cmd ) =
                    EM.initWithMeal meal ingredients
            in
            ( { model | dialog = Open editModel }
            , Cmd.map EditMealMsg cmd
            , Cmd.none
            )

        OpenNewMealDialog ->
            let
                ingredients =
                    List.concatMap (\m -> m.ingredients) model.meals

                ( editModel, cmd, evCmd ) =
                    EM.init ingredients
            in
            ( { model | dialog = Open editModel }
            , Cmd.map EditMealMsg cmd
            , evCmd
            )

        CloseDialog ->
            ( { model | dialog = Closed }
            , Cmd.none
            , Cmd.none
            )

        EditMealMsg emsg ->
            case model.dialog of
                Closed ->
                    ( model, Cmd.none, Cmd.none )

                Open emodel ->
                    let
                        ( updatedEmodel, cmd, evCmd ) =
                            EM.update emsg emodel
                    in
                    ( { model | dialog = Open updatedEmodel }
                    , Cmd.map EditMealMsg cmd
                    , evCmd
                    )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Meals" ]
        , button [ onClick OpenNewMealDialog ] [ text "Add Meal" ]
        , ul [] (List.map viewMeal <| model.meals)
        , case model.dialog of
            Closed ->
                text ""

            Open emodel ->
                EM.view emodel EditMealMsg CloseDialog
        ]


viewMeal : Meal -> Html Msg
viewMeal meal =
    li []
        [ div []
            [ text ("Meal at " ++ meal.datetime)
            , button [ onClick (OpenDialog meal) ] [ text "Edit" ]
            ]
        ]


parseMealEvent : Ev.Envelope -> Maybe HydrationEvent
parseMealEvent envelope =
    envelope.payload
        |> D.decodeValue
            (D.map3 HydrationEvent
                (decodeMealEvent envelope.type_)
                (D.succeed envelope.streamId)
                (D.succeed envelope.streamPosition)
            )
        |> Result.toMaybe


decodeMealEvent : String -> D.Decoder Event
decodeMealEvent type_ =
    case type_ of
        "IngredientAdded" ->
            D.map IngredientAdded (D.field "ingredient" D.string)

        "IngredientRemoved" ->
            D.map IngredientRemoved (D.field "ingredient" D.string)

        "MealCreated" ->
            D.succeed MealCreated

        "NotesUpdated" ->
            D.map NotesUpdated (D.field "notes" (D.nullable D.string) |> D.map (Maybe.withDefault ""))

        "DateTimeUpdated" ->
            D.map DateTimeUpdated (D.field "datetime" D.string)

        "MealDeleted" ->
            D.succeed MealDeleted

        "CreateMeal" ->
            D.succeed MealCreated

        _ ->
            D.fail ("Unknown event type: " ++ type_)


applyMealEventList : HydrationEvent -> Model -> Model
applyMealEventList (HydrationEvent ev streamId streamPosition) model =
    let
        meals =
            model.meals

        mealWithId =
            List.filter (\m -> m.streamId == streamId) meals |> List.head

        updatedMeal =
            if streamPosition == (mealWithId |> Maybe.map .streamPosition |> Maybe.withDefault 0) + 1 then
                applyEvent ev mealWithId streamId |> Maybe.map (\m -> { m | streamPosition = streamPosition })

            else
                mealWithId

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


applyEvent : Event -> Maybe Meal -> String -> Maybe Meal
applyEvent ev maybeMeal streamId =
    case ( ev, maybeMeal ) of
        ( MealCreated, Nothing ) ->
            Just { emptyMeal | streamId = streamId }

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


applyPersistanceResult : HydrationEvent -> Model -> Model
applyPersistanceResult events model =
    let
        newModel =
            applyMealEventList events model
    in
    case ( events, model.dialog ) of
        ( HydrationEvent MealCreated streamId _, Open dialog ) ->
            -- When a meal is created, we need to set its streamId and streamPosition
            { newModel | dialog = Open <| EM.applyPersistanceResult (EM.AssignedMealId streamId) dialog }

        _ ->
            newModel
