module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

main : Program () Model Msg
main = 
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }

-- MODEL
type alias Model =
    { currentMessage : String
    , messages : List String
    , clickMessage : List String
    , current : Int
    , result : Int
    , commands : Int -> Int -> Int
    , buttons :
        { clear : String
        , del : String
        , nums : List Int
        , signs : List String }}
init : Model
init =
    { currentMessage = "currentMessage"
    , messages= ["message01", "message02"]
    , clickMessage = ["clickMessage"]
    , current = 0
    , result = 0
    , commands = (+)
    , buttons = 
        { clear = "clear"
        , del = "del"
        , nums =
            List.range 0 9
                |> List.reverse
        , signs =
            [ "+", "-", "*", "/", "="]}}

stringToInt : String -> Int
stringToInt str = Maybe.withDefault 0 <| String.toInt str

-- VIEW

viewButtonText : String -> Html Msg
viewButtonText str =
    li []
    [ button [] [ text str ] ]

viewButtonNum : Int -> Html Msg
viewButtonNum int =
    li []
    [ button [ onClick (Input int) ] [ text <| String.fromInt int ]]

viewButtonSign : Msg -> String -> Html Msg
viewButtonSign msg str =
    li []
    [ button [ onClick msg ] [ text str ]]

view : Model -> Html Msg
view model =
    div []
        [ div []
            [ div [] 
                [ text (String.fromInt model.current)]
            , div [] 
                [ text (String.fromInt model.result)]
            , div []
                [ div [] 
                    [ div [] 
                        [ text model.currentMessage]
                    , div [] []]
                , div [] 
                    [ img [ src "./assets/character.png"] []]]]
        , div [] 
            [ ul []
                (model.buttons.nums 
                    |> List.map viewButtonNum)
            , ul [] 
                <| List.append
                [ viewButtonText model.buttons.clear
                , viewButtonText model.buttons.del
                , viewButtonSign Sum "+" ]
                (model.buttons.signs |> List.map viewButtonText) ] ]
                

-- UPDATE
update : Msg -> Model -> Model
update msg model =
    case msg of
        Input input ->
            { model
                | current = stringToInt <| (String.fromInt model.current) ++ (String.fromInt input) }
        Sum ->
            { model 
                | result = model.commands model.result model.current
                , current = 0
                , commands = (+) }

type Msg
    = Input Int
    | Sum