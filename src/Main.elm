module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Todo =
    { id : Int
    , title : String
    , finished : Bool
    }


type alias Model =
    { todos : List Todo
    , inputValue : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] ""
    , Cmd.none
    )



-- UPDATE


type Msg
    = Add Todo
    | ToggleFinish Int Bool
    | Remove Int
    | ChangeInput String


toggleFinish : Int -> Bool -> Todo -> Todo
toggleFinish id value todo =
    if todo.id == id then
        { todo | finished = value }

    else
        todo


removeTodo : Int -> Todo -> Bool
removeTodo id todo =
    todo.id /= id


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add todo ->
            ( { model | todos = model.todos ++ [ todo ] }, Cmd.none )

        ToggleFinish id value ->
            ( { model | todos = List.map (toggleFinish id value) model.todos }, Cmd.none )

        Remove id ->
            ( { model | todos = List.filter (removeTodo id) model.todos }, Cmd.none )

        ChangeInput value ->
            ( { model | inputValue = value }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


todoView : Todo -> Html Msg
todoView todo =
    li []
        [ input [ type_ "checkbox", id (String.fromInt todo.id), checked todo.finished, onCheck (ToggleFinish todo.id) ] []
        , label
            [ for (String.fromInt todo.id)
            , style "text-decoration"
                (if todo.finished then
                    "line-through"

                 else
                    "none"
                )
            ]
            [ text todo.title, button [ onClick (Remove todo.id), style "margin-left" "10px" ] [ text "Remove" ] ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "New task", value model.inputValue, onInput ChangeInput ] []
        , button [ onClick (Add (Todo (List.length model.todos + 1) model.inputValue False)) ] [ text "Add" ]
        , ul [] (List.map todoView model.todos)
        ]
