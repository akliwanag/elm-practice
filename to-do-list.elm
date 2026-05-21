module Main exposing (main)
import Browser
import Html exposing (Html, div, text, p, input, br, button)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (type_, checked, value, placeholder)

type Msg
    = MsgAddTask
    | MsgCheckTask Int
    | MsgUpdateTask Int String
    

type alias Model = { tasks : List String }

init : { tasks : List String }
init = { tasks = [""] }

removeAt : Int -> List a -> List a
removeAt idx lst =
    if 0 <= idx && idx < List.length lst then
      let
        first = List.take (idx) lst
        second = List.drop (idx+1) lst
      in
        List.append first second
    else
      lst

update: Msg -> Model -> Model
update msg model =
    case msg of
        MsgAddTask ->
            { model
            | tasks = model.tasks ++ [""]
            }
        MsgCheckTask n ->
            { model
            | tasks = removeAt n model.tasks
            }
        MsgUpdateTask n val ->
            { model
            | tasks = 
            let
                first = List.append (List.take n model.tasks) (List.singleton val)
                second = List.drop (n+1) model.tasks
            in
            List.append first second
            }
                

viewTaskList : Int -> String -> Html Msg
viewTaskList index task =
    div []
        [ input [ type_ "checkbox", checked False, onClick (MsgCheckTask index) ] []
        , input [ placeholder "Enter task", value task, onInput (\new -> MsgUpdateTask index new) ] []
        ]


view : Model -> Html Msg
view model = 
    div []
        [ p [] [ text "To-do List" ]
        , br [] []
        , div [] (List.indexedMap viewTaskList model.tasks)
        , br [] []
        , button [ onClick MsgAddTask ] [ text "Add" ]
        ]



main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
