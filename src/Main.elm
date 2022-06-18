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
    , command : Float -> Float -> Float
    , buttons :
        { clear : String
        , del : String
        , nums : List Int
        , signs : List String }}
init : Model
init =
    { currentMessage = "Elm Elm Elm Elm Elm Elm"
    , messages= ["message01", "message02"]
    , clickMessage = ["clickMessage"]
    , current = 0
    , result = 0
    , command = (+)
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

viewButton : Msg -> String -> String -> Html Msg
viewButton msg str className =
    li []
    [ button [ onClick msg, class className] [ text str] ]

viewButtonNum : Int -> Html Msg
viewButtonNum int =
    li []
    [ button [ onClick (Input int) ] [ text <| String.fromInt int ]]

viewButtonCommand : (Float -> Float -> Float) -> String -> String -> Html Msg
viewButtonCommand command str className =
    li []
    [ button [ onClick (Command command), class className ] [ text str ]]

viewButtonEqual = 
    viewButton Equal "=" "mark"

view : Model -> Html Msg
view model =
    div [ id "app" ]
        [ div [class "display"]
            [ div [class "current"] 
                [ text (String.fromFloat model.current)]
            , div [class "result"] 
                [ text (String.fromFloat model.result)]
            , div [class "comment"]
                [ div [class "comment-inner"] 
                    [ div [class "textarea"] 
                        [ text model.currentMessage]]
                , div [class "character"] 
                    [ img [ src "src/assets/character.png"] []]]]
        , div [class "container"] 
            [ ul [class "left"]
                ( []
                    |> List.append
                        [ viewButton Del "" ""
                        , viewButtonNum 0
                        , viewButton Del "" "" ]
                    |> List.append
                        ( model.buttons.nums 
                            |> List.filter (\n -> n > 0)
                            |> List.map viewButtonNum ))
            , ul [class "right"] 
                [ viewButton Clear model.buttons.clear "clear"
                , viewButton Del model.buttons.del "del"
                , viewButtonCommand (+) "+" "mark"
                , viewButtonCommand (-) "-" "mark"
                , viewButtonCommand (*) "*" "mark"
                , viewButtonCommand (/) "/" "mark"
                , viewButtonEqual ] ] ]
                

-- UPDATE
update : Msg -> Model -> Model
update msg model =
    case msg of
        Input input ->
            { model
                | current = stringToFloat <| (String.fromFloat model.current) ++ (String.fromInt input) }
        Command command ->
            { model 
                | result = model.command model.result model.current
                , current = 0
                , command = command }
        Clear ->
            { model
                | result = 0
                , current = 0
                , command = (+) }
        Del ->
            { model
                | current = 0 }
        Equal ->
            { model
                | current = model.command model.result model.current
                , result = 0
                , command = (+) }

type Msg
    = Input Int
    | Command (Float -> Float -> Float)
    | Clear
    | Del
    | Equal