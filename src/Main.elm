module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)

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
    , log : Int
    , buttons: 
        { clear : String
        , del : String
        , nums : List String
        , signs : List String }}
init : Model
init =
    { currentMessage = "currentMessage"
    , messages= ["message01", "message02"]
    , clickMessage = ["clickMessage"]
    , current = 0
    , result = 0
    , log = 0
    , buttons = 
        { clear = "clear"
        , del = "del"
        , nums =
            List.range 0 9
                |> List.map String.fromInt
                |> List.reverse
        , signs =
            [ "+", "-", "*", "/", "="]}}

sum a b =
    a + b
sub a b =
    a - b
mul a b =
    a * b
divide a b =
    a / b

-- VIEW

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
                <| List.map (\item -> li [] [ text item ]) model.buttons.nums
            , ul [] 
                <| List.append
                [ li [] [text model.buttons.clear]
                , li [] [text model.buttons.del]]
                <| List.map (\item -> li [] [ text item]) model.buttons.signs ] ]

-- UPDATE
update : Msg -> Model -> Model
update msg model = model

type Msg
    = Sum
    | Decrement