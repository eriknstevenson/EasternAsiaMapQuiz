module Main exposing (..)

import Array exposing (Array)
import Html exposing (Html, button, beginnerProgram, div, text, ul, li, img, h1, h2, a)
import Html.Attributes exposing (src, width, height)
import Html.Events exposing (onClick)


main : Program Never Model Msg
main =
    beginnerProgram { model = initialModel, view = view, update = update }


type Msg
    = ToggleAnswer
    | RemoveLocation Location
    | Next
    | Previous
    | Reset


type alias Location =
    { displayName : String
    , centeredOn : String
    , markerAt : String
    , zoomLevel : Int
    }


type alias Model =
    { currentLocation : Int
    , locations : Array Location
    , showAnswer : Bool
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleAnswer ->
            { model | showAnswer = not (model.showAnswer) }

        Next ->
            let
                newLocation =
                    if model.currentLocation + 1 < Array.length model.locations then
                        model.currentLocation + 1
                    else
                        0
            in
                { model | currentLocation = newLocation, showAnswer = False }

        Previous ->
            let
                newLocation =
                    if model.currentLocation - 1 >= 0 then
                        model.currentLocation - 1
                    else
                        Array.length model.locations - 1
            in
                { model | currentLocation = newLocation, showAnswer = False }

        RemoveLocation toRemove ->
            let
                updatedLocations =
                    Array.filter (\location -> location /= toRemove) model.locations
            in
                { model
                    | locations = updatedLocations
                    , currentLocation = min model.currentLocation (Array.length updatedLocations - 1)
                    , showAnswer = False
                }

        Reset ->
            initialModel


mapImgAttrs : List (Html.Attribute Msg)
mapImgAttrs =
    [ Html.Events.onClick ToggleAnswer, width 512, height 384 ]


view : Model -> Html Msg
view model =
    div []
        [ mainContent model
        ]


mainContent : Model -> Html Msg
mainContent model =
    case Array.get model.currentLocation model.locations of
        Just location ->
            div []
                [ h1 [] [ text "HIST 141 Map Quiz" ]
                , h2 [] [ text ("Where is " ++ location.displayName ++ "?") ]
                , div []
                    [ if model.showAnswer then
                        img (src (makeGoogleMapUrl location) :: mapImgAttrs) []
                      else
                        img (src baseMapUrl :: mapImgAttrs) []
                    ]
                , div [] [ text ("Click on the image to toggle the answer. " ++ toString (Array.length model.locations) ++ " locations remaining.") ]
                , div []
                    [ button [ onClick Previous ] [ text "previous" ]
                    , button [ onClick (RemoveLocation location) ] [ text "remove from set" ]
                    , button [ onClick Next ] [ text "next" ]
                    ]
                , Html.hr [] []
                , div []
                    [ button [ onClick Reset ] [ text "reset" ]
                    ]
                ]

        Nothing ->
            h1 [] [ text "You've learned all of the locations." ]


mapToShow : Model -> Html Msg
mapToShow model =
    img [ src baseMapUrl ] []


baseMapUrl : String
baseMapUrl =
    "http://i.imgur.com/B6tHDvp.jpg"


makeGoogleMapUrl : Location -> String
makeGoogleMapUrl location =
    String.concat
        [ "https://maps.googleapis.com/maps/api/staticmap?center="
        , location.centeredOn
        , "&zoom="
        , toString (location.zoomLevel)
        , "&size=512x384&markers=color:red|"
        , location.markerAt
        , "&style=feature:all|element:geometry.stroke|color:0xff0000|visibility:simplified"
        , "&key=AIzaSyBXspm-veAIsUQ7xELv64zX-wKDx1Fbj4I"
        ]


initialModel : Model
initialModel =
    { currentLocation = 0
    , showAnswer = False
    , locations = Array.fromList studyLocations
    }


defaultLocation : String -> Location
defaultLocation location =
    { displayName = location
    , centeredOn = location
    , markerAt = location
    , zoomLevel = 5
    }


withZoom : Int -> Location -> Location
withZoom zoom location =
    { location | zoomLevel = zoom }


centerOn : String -> Location -> Location
centerOn term location =
    { location | centeredOn = term }


markAt : String -> Location -> Location
markAt term location =
    { location | markerAt = term }


withSearchTerm : String -> Location -> Location
withSearchTerm term location =
    { location | centeredOn = term, markerAt = term }


studyLocations : List Location
studyLocations =
    [ defaultLocation "the Bay of Bohai"
    , defaultLocation "Beijing"
    , defaultLocation "Changchun"
    , defaultLocation "Chengdu" |> withZoom 3 |> centerOn "China"
    , defaultLocation "Dunhuang" |> withZoom 4 |> centerOn "China"
    , defaultLocation "East China Sea"
    , defaultLocation "Han River" |> withZoom 9 |> withSearchTerm "Han River South Korea"
    , defaultLocation "Hangzhou"
    , defaultLocation "Hong Kong"
    , defaultLocation "Kaesong"
    , defaultLocation "Kyoto"
    , defaultLocation "Lanzhou" |> withZoom 3 |> centerOn "China"
    , defaultLocation "Lhasa"
    , defaultLocation "Luoyang"
    , defaultLocation "the Mekong River"
    , defaultLocation "Nagasaki"
    , defaultLocation "Nagoya"
    , defaultLocation "Nanjing"
    , defaultLocation "Osaka"
    , defaultLocation "Pusan/Busan" |> withSearchTerm "Busan"
    , defaultLocation "Pyeongyang"
    , defaultLocation "Qufu"
    , defaultLocation "the Ryukyu Islands"
    , defaultLocation "the Sea of Japan"
    , defaultLocation "Seoul"
    , defaultLocation "Shenyang"
    , defaultLocation "the South China Sea"
    , defaultLocation "Tainan"
    , defaultLocation "Taipei"
    , defaultLocation "Taiwan Strait"
    , defaultLocation "Tokyo"
    , defaultLocation "Ulaanbaatar"
    , defaultLocation "Urumchi" |> withZoom 3
    , defaultLocation "Vladivostok"
    , defaultLocation "Xiamen"
    , defaultLocation "Xi’an" |> withZoom 4
    , defaultLocation "the Yalu River"
    , defaultLocation "the Yangzi River"
    , defaultLocation "the Yellow River"
    , defaultLocation "the Yellow Sea"
    , defaultLocation "Zhengzhou"
    ]
