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
    , current : Float
    , result : Float
    , commands : Float -> Float -> Float
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

stringToFloat : String -> Float
stringToFloat str = Maybe.withDefault 0 <| String.toFloat str

-- VIEW

viewButtonText : Msg -> String -> Html Msg
viewButtonText msg str =
    li []
    [ button [ onClick msg ] [ text str ] ]

viewButtonNum : Int -> Html Msg
viewButtonNum int =
    li []
    [ button [ onClick (Input int) ] [ text <| String.fromInt int ]]

viewButtonSign : (Float -> Float -> Float) -> String -> Html Msg
viewButtonSign command str =
    li []
    [ button [ onClick (Command command) ] [ text str ]]

view : Model -> Html Msg
view model =
    div []
        [ div []
            [ div [] 
                [ text (String.fromFloat model.current)]
            , div [] 
                [ text (String.fromFloat model.result)]
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
                [ viewButtonText Clear model.buttons.clear
                , viewButtonText Del model.buttons.del
                , viewButtonSign (+) "+" 
                , viewButtonSign (-) "-" 
                , viewButtonSign (*) "*"
                , viewButtonSign (/) "/" ] ] ]
                

-- UPDATE
update : Msg -> Model -> Model
update msg model =
    case msg of
        Input input ->
            { model
                | current = stringToFloat <| (String.fromFloat model.current) ++ (String.fromInt input) }
        Command command ->
            { model 
                | result = model.commands model.result model.current
                , current = 0
                , commands = command }
        Clear ->
            { model
                | result = 0
                , current = 0
                , commands = (+) }
        Del ->
            { model
                | current = 0 }

type Msg
    = Input Int
    | Command (Float -> Float -> Float)
    | Clear
    | Del