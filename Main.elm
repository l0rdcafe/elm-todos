module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)

type alias Todo =
    {
        id: Int
        , title : String
        , isDone: Bool
    }

type alias Model =
    {
        todos : List Todo
        , todoInput : String
    }

initialModel : Model
initialModel =
    {
        todos = []
        , todoInput = ""
    }

type alias Msg =
    {
        msgType : String
        , data : String
    }

getID : List Todo -> Int
getID todos =
    let ids = List.map (\todo -> todo.id) todos
    in
        case List.maximum ids of
            Nothing -> 1
            Just max -> max + 1

update : Msg -> Model -> Model
update msg model =
    if msg.msgType == "CreateTodo" then
        { model | todos = model.todos ++ [{ id = getID model.todos, title = msg.data, isDone = False }], todoInput = "" }
    else if msg.msgType == "DeleteTodo" then
        { model | todos = List.filter (\todo -> String.fromInt todo.id /= msg.data ) model.todos }
    else if msg.msgType == "UpdateTodo" then
        { model | todoInput = msg.data }
    else if msg.msgType == "ToggleTodo" then
        { model | todos = List.map (\todo -> if String.fromInt todo.id == msg.data then { todo | isDone = not todo.isDone } else todo ) model.todos }
    else
        model

viewTodo : Todo -> Html Msg
viewTodo todo =
    li [ id (String.fromInt todo.id) ] [
        input [ name "isDone", type_ "checkbox", checked todo.isDone, onClick { msgType = "ToggleTodo", data = String.fromInt todo.id } ] []
        , text todo.title
        , button [ onClick { msgType = "DeleteTodo", data = String.fromInt todo.id } ] [ text "Delete" ]
    ]

view : Model -> Html Msg
view model =
    let 
        todos =
            List.map viewTodo model.todos
    in
        div []
            [ h1 [] [ text "Elm Todos" ] 
            , Html.form [ onSubmit { msgType = "CreateTodo", data = model.todoInput } ] [
                input [ name "todo", value model.todoInput, onInput (\val -> { msgType = "UpdateTodo", data = val }) ] []
                , button [] [ text "Create" ]
            ]
            , ul [] todos
            ]

main =
    Browser.sandbox
    {
        init = initialModel
        , view = view
        , update = update
    }