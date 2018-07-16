module OtherPage exposing (Model, Msg, init, update, view)

import Html exposing (..)


type alias Model =
    { text : String
    }


type Msg
    = NoOp


init =
    ( Model "Hello", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [] [ text model.text ]
