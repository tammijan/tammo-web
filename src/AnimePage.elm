module AnimePage exposing (AnimeFlags, Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, at, map, string)
import Json.Decode.Pipeline exposing (decode, hardcoded, requiredAt)
import List
import Task


-- MODEL


type EntryId
    = EntryId String


type alias AnimeFlags =
    { spreadsheet1 : String
    , spreadsheet2 : String
    }


type alias RemoteEntry =
    { id : EntryId
    , name : String
    , ep : String
    , genre : String
    , rating : String
    , comment : String
    , image : String
    , collapsed : Bool
    }


type alias Entry =
    { id : EntryId
    , name : String
    , ep : String
    , genre : String
    , rating : String
    , comment : String
    , image : String
    , collapsed : Bool
    , epComments : List Comment
    }


type alias RemoteComment =
    { id : EntryId
    , ep : String
    , comment : String
    }


type alias Comment =
    { ep : String
    , comment : String
    }


type WebContent a
    = Loading
    | Error String
    | Success a


type alias Model =
    { flags : AnimeFlags
    , content : WebContent (List Entry)
    , showImages : Bool
    }


init : AnimeFlags -> ( Model, Cmd Msg )
init flags =
    ( Model flags Loading True
    , getContent flags
    )



-- UPDATE


type Msg
    = RemoteData (Result Http.Error (List Entry))
    | ToggleEntryCollapse EntryId
    | ToggleImages


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleEntryCollapse entryId ->
            case model.content of
                Success entries ->
                    let
                        newEntries =
                            List.map
                                (\e ->
                                    if compareEntryIds entryId e.id then
                                        { e | collapsed = not e.collapsed }
                                    else
                                        e
                                )
                                entries
                    in
                    ( { model | content = Success newEntries }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ToggleImages ->
            ( { model | showImages = not model.showImages }, Cmd.none )

        RemoteData (Ok entries) ->
            ( { model | content = Success entries }, Cmd.none )

        RemoteData (Err _) ->
            ( { model | content = Error "error" }, Cmd.none )


compareEntryIds : EntryId -> EntryId -> Bool
compareEntryIds (EntryId id1) (EntryId id2) =
    id1 == id2



-- VIEW


view : Model -> Html Msg
view model =
    case model.content of
        Loading ->
            h1 [] [ text "Loading..." ]

        Success entries ->
            let
                hideImageClass =
                    if model.showImages then
                        ""
                    else
                        "hide-images"

                buttonText =
                    if model.showImages then
                        "Hide images"
                    else
                        "Show images"
            in
            div [ class hideImageClass ] <|
                [ div [ class "tool-bar" ]
                    [ button [ type_ "button", class "btn btn-primary", onClick ToggleImages ] [ text buttonText ]
                    ]
                ]
                    ++ List.map viewListEntry entries

        Error message ->
            div [] [ text ("Error: " ++ message) ]


viewListEntry : Entry -> Html Msg
viewListEntry { id, name, ep, genre, rating, comment, image, collapsed, epComments } =
    let
        collapsedClass =
            if collapsed then
                "row collapse"
            else
                "row collapse show"
    in
    div [ class "card entry" ]
        [ div [ class "card-body" ]
            [ h5 [ class "card-title", onClick (ToggleEntryCollapse id) ] [ text name ]
            , div [ class "row" ]
                [ div [ class "col-auto col-image" ] [ a [ href image, target "_blank" ] [ img [ class "image", src image ] [] ] ]
                , div [ class "col-2" ] (viewEntryDesc "Episoden" ep)
                , div [ class "col-2" ] (viewEntryDesc "Genre" genre)
                , div [ class "col-2" ] (viewEntryDesc "Bewertung" rating)
                , div [ class "col" ] (viewEntryDesc "Kommentar" comment)
                ]
            , div [ class collapsedClass ]
                [ div [ class "col" ] [ viewEpComments epComments ]
                ]
            ]
        ]


viewEpComments : List Comment -> Html Msg
viewEpComments comments =
    let
        mapComment { ep, comment } =
            tr []
                [ td [] [ text ep ]
                , td [] [ text comment ]
                ]
    in
    table [ class "table" ]
        [ thead []
            [ tr []
                [ th [ scope "col" ] [ text "Episode" ]
                , th [ scope "col" ] [ text "Kommentar" ]
                ]
            ]
        , tbody [] <| List.map mapComment comments
        ]


viewEntryDesc : String -> String -> List (Html Msg)
viewEntryDesc title value =
    [ div [ class "font-weight-bold" ] [ text title ]
    , div [] [ text value ]
    ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getContent : AnimeFlags -> Cmd Msg
getContent flags =
    httpTask2 flags.spreadsheet2
        |> Task.andThen (httpTask1 flags.spreadsheet1)
        |> Task.attempt RemoteData


httpTask2 : String -> Task.Task Http.Error (List RemoteComment)
httpTask2 url =
    Http.get url decodeRemoteCommentList |> Http.toTask


httpTask1 : String -> List RemoteComment -> Task.Task Http.Error (List Entry)
httpTask1 url comments =
    Http.get url decodeRemoteEntryList
        |> Http.toTask
        |> Task.map (List.map (mapEntry comments))


mapEntry : List RemoteComment -> RemoteEntry -> Entry
mapEntry comments remote =
    let
        epComments =
            comments
                |> List.filter (\c -> compareEntryIds c.id remote.id)
                |> List.map (\c -> { ep = c.ep, comment = c.comment })
    in
    { id = remote.id
    , name = remote.name
    , ep = remote.ep
    , genre = remote.genre
    , rating = remote.rating
    , comment = remote.comment
    , image = remote.image
    , collapsed = True
    , epComments = epComments
    }


decodeRemoteEntryList : Decoder (List RemoteEntry)
decodeRemoteEntryList =
    at [ "feed", "entry" ] (Decode.list decodeRemoteEntry)


decodeRemoteEntry : Decoder RemoteEntry
decodeRemoteEntry =
    decode RemoteEntry
        |> requiredAt [ "gsx$id", "$t" ] (Decode.map EntryId string)
        |> requiredAt [ "gsx$name", "$t" ] string
        |> requiredAt [ "gsx$episoden", "$t" ] string
        |> requiredAt [ "gsx$genre", "$t" ] string
        |> requiredAt [ "gsx$bewertung", "$t" ] string
        |> requiredAt [ "gsx$kommentar", "$t" ] string
        |> requiredAt [ "gsx$image", "$t" ] string
        |> hardcoded True


decodeRemoteCommentList : Decoder (List RemoteComment)
decodeRemoteCommentList =
    at [ "feed", "entry" ] (Decode.list decodeRemoteComment)


decodeRemoteComment : Decoder RemoteComment
decodeRemoteComment =
    decode RemoteComment
        |> requiredAt [ "gsx$id", "$t" ] (Decode.map EntryId string)
        |> requiredAt [ "gsx$episode", "$t" ] string
        |> requiredAt [ "gsx$kommentar", "$t" ] string
