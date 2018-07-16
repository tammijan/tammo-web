-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/http.html


module Main exposing (..)

import AnimePage exposing (Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import OtherPage


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Page
    = Anime AnimePage.Model
    | Other OtherPage.Model


type alias Flags =
    { anime : AnimePage.AnimeFlags
    }


type alias Model =
    { flags : Flags
    , page : Page
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( model, cmd ) =
            AnimePage.init flags.anime
    in
    ( Model flags (Anime model)
    , Cmd.map AnimePageMsg cmd
    )



-- UPDATE


type NavItem
    = AnimeItem
    | OtherItem


type Msg
    = AnimePageMsg AnimePage.Msg
    | OtherPageMsg OtherPage.Msg
    | ShowAnime
    | ShowOther


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( AnimePageMsg pageMsg, Anime pageModel ) ->
            toPage model Anime AnimePageMsg AnimePage.update pageMsg pageModel

        ( OtherPageMsg pageMsg, Other pageModel ) ->
            toPage model Other OtherPageMsg OtherPage.update pageMsg pageModel

        ( ShowAnime, _ ) ->
            initPage model Anime AnimePageMsg (AnimePage.init model.flags.anime)

        ( ShowOther, _ ) ->
            initPage model Other OtherPageMsg OtherPage.init

        _ ->
            ( model, Cmd.none )


initPage mainModel toModel toMsg init =
    let
        ( newModel, newCmd ) =
            init
    in
    ( { mainModel | page = toModel newModel }, Cmd.map toMsg newCmd )


toPage mainModel toModel toMsg subUpdate subMsg subModel =
    let
        ( newModel, newCmd ) =
            subUpdate subMsg subModel
    in
    ( { mainModel | page = toModel newModel }, Cmd.map toMsg newCmd )



-- VIEW


view : Model -> Html Msg
view model =
    let
        ( navItem, mainContent ) =
            case model.page of
                Anime pageModel ->
                    ( AnimeItem, AnimePage.view pageModel |> Html.map AnimePageMsg )

                Other pageModel ->
                    ( OtherItem, OtherPage.view pageModel |> Html.map OtherPageMsg )
    in
    div [ class "container-fluid" ]
        [ div [ class "row" ]
            [ div [ class "col-12 col-md-4 col-lg-3 col-xl-2 mb-3" ] [ viewNavigation navItem ]
            , div [ class "col-12 col-md" ] [ mainContent ]
            ]
        ]


viewNavigation : NavItem -> Html Msg
viewNavigation active =
    let
        navClass item =
            if item == active then
                class "nav-link active"
            else
                class "nav-link"
    in
    nav [ class "nav nav-pills flex-column" ]
        [ a [ navClass AnimeItem, onClick ShowAnime, href "javascript:void(0)" ] [ text "Anime" ]
        , a [ navClass OtherItem, onClick ShowOther, href "javascript:void(0)" ] [ text "Other" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
