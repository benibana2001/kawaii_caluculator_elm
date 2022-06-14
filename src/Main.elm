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
    , buttons: 
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

-- stringFromInt : Int -> String
-- stringFromInt int = Maybe.withDefault "0" (String.fromInt int)

sumString : String -> String -> Int
sumString a b =
    (stringToInt a) + (stringToInt b)

-- VIEW

viewButtonNum : Int -> Html Msg
viewButtonNum int =
    li []
    [ button [ onClick (Input int) ] [ text <| String.fromInt int ]]

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
                <| List.map (\item -> viewButtonNum item) model.buttons.nums
            , ul [] 
                <| List.append
                [ li [] [text model.buttons.clear]
                , li [] [text model.buttons.del]]
                <| List.map (\item -> li [] [ text item ]) model.buttons.signs ] ]

-- UPDATE
update : Msg -> Model -> Model
update msg model =
    case msg of
        Input input ->
            { model | current = input }
        Sum input ->
            { model | result =  (+) model.current input }

type Msg
    = Input Int
    | Sum Int
    -- | Sub Int -> Int