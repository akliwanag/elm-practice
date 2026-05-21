module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, input, p, select, option, br)
import Html.Attributes exposing (value, placeholder)
import Html.Events exposing (onClick, onInput)


type alias Model =
    { val : String, display : String, mode : Mode }


initialModel : Model
initialModel =
    { val = "" , display = "Enter a temperature in °C" , mode = ModeCtoF}


type Msg
    = MsgEditInput String
    | MsgChangeMode String

type Mode
    = ModeCtoF 
    | ModeFtoC

getDisplay : Model -> String -> String
getDisplay model str =
    let
        maybeFloat = String.toFloat str
    in
        case maybeFloat of
            Nothing ->
                "Invalid input"
            Just realFloat ->
                if model.mode == ModeCtoF then
                    str ++ " °C = " ++ (String.fromFloat (cToF realFloat)) ++ " °F" 
                else
                    str ++ " °F = " ++ (String.fromFloat (fToC realFloat)) ++ " °C" 

cToF : Float -> Float
cToF n =
    (n * (9/5)) + 32

fToC : Float -> Float
fToC n =
    (n - 32) * (5/9)
    
stringToMode : String -> Mode
stringToMode str =
    if str == "ModeCtoF" then
        ModeCtoF 
    else
        ModeFtoC
    
update : Msg -> Model -> Model
update msg model =
    case msg of
        MsgEditInput newVal ->
            let 
                newModel = 
                    { model
                    | val = newVal
                    }
            in
            { newModel
            | display =
                if newVal == "" then
                    "Enter a temperature in °C"
                else
                    getDisplay newModel newVal
            }
        MsgChangeMode newMode->
            let
                newModel =
                    { model
                    | mode = stringToMode newMode
                    }
            in
            { newModel
            | display =
                if newModel.val == "" then
                    "Enter a temperature in °C"
                else
                    getDisplay newModel newModel.val
            }


view : Model -> Html Msg
view model =
    div []
        [ select [ onInput MsgChangeMode ] [ option [ value "ModeCtoF" ] [ text "Celsius to Fahrenheit" ], option [ value "ModeFtoC" ] [ text "Fahrenheit to Celsius" ] ]
        , br [] []
        , input [ value model.val , onInput MsgEditInput , placeholder "Enter a temperature in °C" ] []
        , p [] [ text model.display ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
