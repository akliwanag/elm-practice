module Main exposing (main)

import Html exposing (Html, div, text, p)

favoriteFoods : List String
favoriteFoods =
    [ "Pizza", "Tacos", "Burgers" ]


rankFoods : List String -> List String
rankFoods foods =
    List.indexedMap (\index food -> (String.fromInt (index+1)) ++ ". " ++ food) foods

view : List String -> Html msg
view rankedList =
    div [] (List.map (\food -> p [] [ text food ]) rankedList)

main =
    view (rankFoods favoriteFoods)
