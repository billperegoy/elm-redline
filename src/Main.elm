module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode


main : Program Never Model Msg
main =
    Html.program
        { init = initModel ! []
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { user : Maybe User
    , regions : List Region
    , hikedSegments : List Segment
    , visibleRegion : Maybe Int
    , visibleTrail : Maybe Int
    }


type alias User =
    { name : String
    , email : String
    }


type alias Region =
    { id : Int
    , name : String
    , trails : List Trail
    }


type alias Trail =
    { id : Int
    , regionId : Int
    , name : String
    , segments : List Segment
    }


type alias Segment =
    { id : Int
    , trailId : Int
    , start : TrailLocation
    , end : TrailLocation
    , length : Float
    , hiked : Bool
    }


type alias TrailLocation =
    { id : Int
    , name : String
    , latitude : Maybe Float
    , longitude : Maybe Float
    }


fakeLocation : TrailLocation
fakeLocation =
    { id = 1
    , name = "fake"
    , latitude = Nothing
    , longitude = Nothing
    }


initModel : Model
initModel =
    { user = Nothing
    , regions =
        [ Region 1
            "region 1"
            [ Trail 1
                1
                "trail 1"
                [ Segment 1 1 fakeLocation fakeLocation 1.2 False
                , Segment 2 1 fakeLocation fakeLocation 1.3 False
                , Segment 3 1 fakeLocation fakeLocation 2.1 False
                ]
            , Trail 2
                1
                "trail 2"
                [ Segment 4 2 fakeLocation fakeLocation 4.2 False
                , Segment 5 2 fakeLocation fakeLocation 1.7 False
                , Segment 6 2 fakeLocation fakeLocation 0.1 False
                ]
            ]
        , Region 2
            "region 2"
            [ Trail 3
                2
                "trail 1"
                [ Segment 7 3 fakeLocation fakeLocation 1.2 False
                , Segment 8 3 fakeLocation fakeLocation 1.3 False
                , Segment 9 3 fakeLocation fakeLocation 2.1 False
                ]
            , Trail 4
                2
                "trail 2"
                [ Segment 10 4 fakeLocation fakeLocation 1.2 False
                , Segment 11 4 fakeLocation fakeLocation 1.3 False
                , Segment 12 4 fakeLocation fakeLocation 2.1 False
                ]
            ]
        ]
    , hikedSegments = []
    , visibleRegion = Nothing
    , visibleTrail = Nothing
    }



-- Update


type Msg
    = NoOp
    | ToggleHiked Int Int Int Bool
    | ToggleRegionVisibility Int
    | ToggleTrailVisibility Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        ToggleHiked regionId trailId segmentId value ->
            { model
                | regions =
                    updateRegions regionId trailId segmentId model.regions
            }
                ! []

        ToggleRegionVisibility regionId ->
            if model.visibleRegion == Just regionId then
                { model
                    | visibleRegion = Nothing
                    , visibleTrail = Nothing
                }
                    ! []
            else
                { model
                    | visibleRegion = Just regionId
                    , visibleTrail = Nothing
                }
                    ! []

        ToggleTrailVisibility trailId ->
            if model.visibleTrail == Just trailId then
                { model
                    | visibleTrail = Nothing
                }
                    ! []
            else
                { model
                    | visibleTrail = Just trailId
                }
                    ! []


updateRegions : Int -> Int -> Int -> List Region -> List Region
updateRegions regionId trailId segmentId regions =
    List.map (\region -> updateRegion regionId trailId segmentId region) regions


updateRegion : Int -> Int -> Int -> Region -> Region
updateRegion regionId trailId segmentId region =
    if region.id == regionId then
        { region | trails = updateTrails trailId segmentId region.trails }
    else
        region


updateTrails : Int -> Int -> List Trail -> List Trail
updateTrails trailId segmentId trails =
    List.map (\trail -> updateTrail trailId segmentId trail) trails


updateTrail : Int -> Int -> Trail -> Trail
updateTrail trailId segmentId trail =
    if trail.id == trailId then
        { trail | segments = updateSegments segmentId trail.segments }
    else
        trail


updateSegments : Int -> List Segment -> List Segment
updateSegments segmentId segments =
    List.map (\segment -> updateSegment segmentId segment) segments


updateSegment : Int -> Segment -> Segment
updateSegment segmentId segment =
    if segment.id == segmentId then
        { segment | hiked = not segment.hiked }
    else
        segment



-- View


pageHeader : Html Msg
pageHeader =
    div [ class "jumbotron text-center" ]
        [ h1
            []
            [ text "Redline the Whites" ]
        ]


pageBody : Model -> Html Msg
pageBody model =
    div []
        [ grandTotals model.regions
        , div []
            (List.map (\region -> singleRegion model.visibleRegion model.visibleTrail region) model.regions)
        ]


grandTotals : List Region -> Html Msg
grandTotals regions =
    let
        total =
            regions
                |> List.map (\region -> regionTotals region)
                |> List.map (\tup -> Tuple.second tup)
                |> List.sum

        hikedTotal =
            regions
                |> List.map (\region -> regionTotals region)
                |> List.map (\tup -> Tuple.first tup)
                |> List.sum

        percentHiked =
            (hikedTotal / total) * 100 |> round
    in
        div [ class "alert alert-success" ]
            [ h2 []
                [ text
                    ("Totals -- "
                        ++ toString hikedTotal
                        ++ " / "
                        ++ toString total
                        ++ "\n        ("
                        ++ toString percentHiked
                        ++ "%)"
                    )
                ]
            ]


regionTotals : Region -> ( Float, Float )
regionTotals region =
    let
        total =
            region.trails
                |> List.map (\trail -> trailTotal trail)
                |> List.map (\tup -> Tuple.second tup)
                |> List.sum

        totalHiked =
            region.trails
                |> List.map (\trail -> trailTotal trail)
                |> List.map (\tup -> Tuple.first tup)
                |> List.sum
    in
        ( totalHiked, total )


regionStats : Region -> String
regionStats region =
    let
        totals =
            regionTotals region

        total =
            Tuple.second totals

        totalHiked =
            Tuple.first totals

        percentHiked =
            ((totalHiked / total) * 100) |> round
    in
        " -- "
            ++ toString totalHiked
            ++ " / "
            ++ toString total
            ++ " ("
            ++ toString percentHiked
            ++ "%)"


singleRegionBody : Maybe Int -> Maybe Int -> Region -> Html Msg
singleRegionBody visibleRegion visibleTrail region =
    if Just region.id == visibleRegion then
        div []
            (List.map (\trail -> singleTrail trail visibleTrail region) region.trails)
    else
        div [] []


singleRegion : Maybe Int -> Maybe Int -> Region -> Html Msg
singleRegion visibleRegion visibleTrail region =
    div []
        [ a [ href "#", onClickNoDefault (ToggleRegionVisibility region.id) ]
            [ div [ class "alert alert-info" ]
                [ h2 [] [ text (region.name ++ " " ++ regionStats region) ]
                ]
            ]
        , singleRegionBody visibleRegion visibleTrail region
        ]


singleTrailBody : Trail -> Maybe Int -> Region -> Html Msg
singleTrailBody trail visibleTrail region =
    if Just trail.id == visibleTrail then
        table
            [ class "table table-striped" ]
            [ tbody []
                ([ headerRow ]
                    ++ (List.map (\segment -> singleSegment segment region trail) trail.segments)
                    ++ [ totalRow trail ]
                )
            ]
    else
        div [] []


singleTrail : Trail -> Maybe Int -> Region -> Html Msg
singleTrail trail visibleTrail region =
    div []
        [ div [ class "alert alert-warning" ]
            [ a [ href "#", onClickNoDefault (ToggleTrailVisibility trail.id) ]
                [ h3 [] [ text trail.name ]
                ]
            ]
        , singleTrailBody trail visibleTrail region
        ]


headerRow : Html Msg
headerRow =
    tr []
        [ th [] [ text "Hiked" ]
        , th [] [ text "Start" ]
        , th [] [ text "End" ]
        , th [] [ text "Length" ]
        ]


trailTotal : Trail -> ( Float, Float )
trailTotal trail =
    let
        totalLength =
            List.map .length trail.segments |> List.sum

        hikedLength =
            trail.segments |> List.filter (\segment -> segment.hiked) |> List.map .length |> List.sum
    in
        ( hikedLength, totalLength )


totalRow : Trail -> Html Msg
totalRow trail =
    let
        total =
            trailTotal trail

        totalLength =
            Tuple.second total

        hikedLength =
            Tuple.first total

        percentHiked =
            (hikedLength / totalLength) * 100 |> round
    in
        tr []
            [ td [ colspan 3 ] [ text "total" ]
            , td []
                [ text
                    ((hikedLength |> toString)
                        ++ " / "
                        ++ (totalLength |> toString)
                        ++ " ("
                        ++ (percentHiked |> toString)
                        ++ "%)"
                    )
                ]
            ]


singleSegment : Segment -> Region -> Trail -> Html Msg
singleSegment segment region trail =
    tr []
        [ td []
            [ input
                [ type_ "checkbox"
                , checked segment.hiked
                , onCheck
                    (ToggleHiked region.id trail.id segment.id)
                ]
                []
            ]
        , td [] [ text segment.start.name ]
        , td [] [ text segment.end.name ]
        , td [] [ toString segment.length |> text ]
        ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ pageHeader
        , pageBody model
        ]


onClickNoDefault : msg -> Attribute msg
onClickNoDefault message =
    let
        config =
            { stopPropagation = True
            , preventDefault = True
            }
    in
        onWithOptions "click" config (Json.Decode.succeed message)



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
