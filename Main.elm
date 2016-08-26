module Main exposing (..)

import Html.App exposing (beginnerProgram)
import Html exposing (Html, div)
import Html.Attributes exposing (href, class, style, attribute)
import Html.Events
import Material.Scheme
import Material.Slider as Slider
import Svg exposing (path, node, svg, text)
import Svg.Attributes exposing (fill, d, viewBox)
import Svg.Events exposing (onClick)
import UndoList
import Dict
import Set
import List.Extra as List
import Maybe.Extra as Maybe


main =
    beginnerProgram
        { model = model
        , view = view
        , update = updateHelper update
        }



-- MODEL


model =
    UndoList.fresh
        { current = automaton.initial
        , trace = []
        , pumped = 1
        }



-- UPDATE


type Msg
    = Click Int
    | Slider Float


update msg ({ current, trace } as model) =
    case msg of
        Click id ->
            let
                ( _, ( from, to ) ) =
                    getSafe id automaton.edges
            in
                if from == current then
                    Just { model | trace = trace ++ [ id ], current = to }
                else
                    Nothing

        Slider pumped ->
            Just { model | pumped = round pumped }


getSafe k d =
    case Dict.get k d of
        Just v ->
            v

        Nothing ->
            Debug.crash "IMPOSSIBLE"


updateHelper updater msg undolist =
    case msg of
        UndoList.Reset ->
            UndoList.reset undolist

        UndoList.Redo ->
            UndoList.redo undolist

        UndoList.Undo ->
            UndoList.undo undolist

        UndoList.Forget ->
            UndoList.forget undolist

        UndoList.New msg ->
            case updater msg undolist.present of
                Just model ->
                    UndoList.new model undolist

                Nothing ->
                    undolist



-- VIEW


colored s ats =
    node s (fill "red" :: ats ++ [ attribute "stroke" "red" ])


bold ats =
    ats ++ [ attribute "stroke-width" "3" ]


automaton =
    let
        states =
            Dict.fromList [ ( 0, "S" ), ( 1, "A" ), ( 2, "B" ), ( 3, "C" ), ( 4, "D" ), ( 5, "E" ), ( 6, "F" ) ]

        initial =
            0

        edges =
            Dict.fromList
                [ ( 7, ( "c", ( 3, 3 ) ) )
                , ( 8, ( "d", ( 3, 4 ) ) )
                , ( 9, ( "b", ( 0, 3 ) ) )
                , ( 10, ( "d", ( 5, 4 ) ) )
                , ( 11, ( "c", ( 5, 6 ) ) )
                , ( 12, ( "a", ( 2, 5 ) ) )
                , ( 13, ( "b", ( 5, 2 ) ) )
                , ( 14, ( "b", ( 1, 2 ) ) )
                , ( 15, ( "b", ( 2, 1 ) ) )
                , ( 16, ( "a", ( 0, 1 ) ) )
                , ( 17, ( "a", ( 1, 6 ) ) )
                ]
    in
        { states = states, initial = initial, edges = edges }


graph colored bold =
    svg
        [ viewBox "0 -80 1026 532", attribute "height" "420", attribute "version" "1.1", attribute "width" "810", attribute "xmlns" "http://www.w3.org/2000/svg" ]
        [ colored 0
            "ellipse"
            (bold 0 [ attribute "cx" "137.5", attribute "cy" "123.5", fill "none", attribute "rx" "30", attribute "ry" "30", attribute "stroke" "black" ])
            []
        , node "text"
            [ attribute "font-family" "Times New Roman", attribute "font-size" "20", attribute "x" "131.5", attribute "y" "129.5" ]
            [ text "S" ]
        , colored 3
            "ellipse"
            (bold 3 [ attribute "cx" "297.5", attribute "cy" "89.5", fill "none", attribute "rx" "30", attribute "ry" "30", attribute "stroke" "black" ])
            []
        , node "text"
            [ attribute "font-family" "Times New Roman", attribute "font-size" "20", attribute "x" "290.5", attribute "y" "95.5" ]
            [ text "C" ]
        , colored 4
            "ellipse"
            (bold 4 [ attribute "cx" "448.5", attribute "cy" "123.5", fill "none", attribute "rx" "30", attribute "ry" "30", attribute "stroke" "black" ])
            []
        , node "text"
            [ attribute "font-family" "Times New Roman", attribute "font-size" "20", attribute "x" "441.5", attribute "y" "129.5" ]
            [ text "D" ]
        , colored 5
            "ellipse"
            (bold 5 [ attribute "cx" "432.5", attribute "cy" "274.5", fill "none", attribute "rx" "30", attribute "ry" "30", attribute "stroke" "black" ])
            []
        , node "text"
            [ attribute "font-family" "Times New Roman", attribute "font-size" "20", attribute "x" "426.5", attribute "y" "280.5" ]
            [ text "E" ]
        , colored 6
            "ellipse"
            (bold 6 [ attribute "cx" "297.5", attribute "cy" "357.5", fill "none", attribute "rx" "30", attribute "ry" "30", attribute "stroke" "black" ])
            []
        , node "text"
            [ attribute "font-family" "Times New Roman", attribute "font-size" "20", attribute "x" "291.5", attribute "y" "363.5" ]
            [ text "F" ]
        , colored 6
            "ellipse"
            (bold 6 [ attribute "cx" "297.5", attribute "cy" "357.5", fill "none", attribute "rx" "24", attribute "ry" "24", attribute "stroke" "black" ])
            []
        , colored 2
            "ellipse"
            (bold 2 [ attribute "cx" "297.5", attribute "cy" "211.5", fill "none", attribute "rx" "30", attribute "ry" "30", attribute "stroke" "black" ])
            []
        , node "text"
            [ attribute "font-family" "Times New Roman", attribute "font-size" "20", attribute "x" "290.5", attribute "y" "217.5" ]
            [ text "B" ]
        , colored 1
            "ellipse"
            (bold 1 [ attribute "cx" "150.5", attribute "cy" "274.5", fill "none", attribute "rx" "30", attribute "ry" "30", attribute "stroke" "black" ])
            []
        , node "text"
            [ attribute "font-family" "Times New Roman", attribute "font-size" "20", attribute "x" "143.5", attribute "y" "280.5" ]
            [ text "A" ]
        , node "polygon"
            [ attribute "points" "92.032,79.389 115.968,102.611", attribute "stroke" "red" ]
            []
        , node "polygon"
            [ fill "red", attribute "points" "115.968,102.611 113.708,93.451 106.744,100.629" ]
            []
        , colored 7
            "path"
            (bold 7 [ d "M 284.275,62.703 A 22.5,22.5 0 1 1 310.725,62.703", fill "none", attribute "stroke" "black" ])
            []
        , node "text"
            [ onClick (UndoList.New <| Click 7), attribute "font-family" "Times New Roman", attribute "font-size" "25", attribute "x" "293.5", attribute "y" "13.5" ]
            [ text "c" ]
        , colored 7
            "polygon"
            [ attribute "points" "310.725,62.703 319.473,59.17 311.382,53.292" ]
            []
        , colored 8
            "path"
            (bold 8 [ d "M 327.447,91.137 A 377.218,377.218 0 0 1 420.741,112.144", fill "none", attribute "stroke" "black" ])
            []
        , colored 8
            "polygon"
            [ attribute "points" "420.741,112.144 414.93,104.712 411.514,114.11" ]
            []
        , node "text"
            [ onClick (UndoList.New <| Click 8), attribute "font-family" "Times New Roman", attribute "font-size" "25", attribute "x" "376.5", attribute "y" "88.5" ]
            [ text "d" ]
        , colored 9
            "path"
            (bold 9 [ d "M 163.845,109.191 A 237.666,237.666 0 0 1 267.613,87.14", fill "none", attribute "stroke" "black" ])
            []
        , colored 9
            "polygon"
            [ attribute "points" "267.613,87.14 259.692,82.015 259.535,92.014" ]
            []
        , node "text"
            [ onClick (UndoList.New <| Click 9), attribute "font-family" "Times New Roman", attribute "font-size" "25", attribute "x" "203.5", attribute "y" "82.5" ]
            [ text "b" ]
        , colored 10
            "path"
            (bold 10 [ d "M 454.327,152.902 A 208.209,208.209 0 0 1 444.359,246.972", fill "none", attribute "stroke" "black" ])
            []
        , colored 10
            "polygon"
            [ attribute "points" "454.327,152.902 450.351,161.458 460.275,160.225" ]
            []
        , node "text"
            [ onClick (UndoList.New <| Click 10), attribute "font-family" "Times New Roman", attribute "font-size" "25", attribute "x" "461.5", attribute "y" "207.5" ]
            [ text "d" ]
        , colored 11
            "path"
            (bold 11 [ d "M 414.036,298.105 A 187.768,187.768 0 0 1 326.897,351.679", fill "none", attribute "stroke" "black" ])
            []
        , colored 11
            "polygon"
            [ attribute "points" "326.897,351.679 335.955,354.315 333.236,344.692" ]
            []
        , node "text"
            [ onClick (UndoList.New <| Click 11), attribute "font-family" "Times New Roman", attribute "font-size" "25", attribute "x" "379.5", attribute "y" "351.5" ]
            [ text "c" ]
        , colored 12
            "path"
            (bold 12 [ d "M 327.397,209.988 A 131.719,131.719 0 0 1 414.455,250.615", fill "none", attribute "stroke" "black" ])
            []
        , colored 12
            "polygon"
            [ attribute "points" "414.455,250.615 412.56,241.373 405.318,248.269" ]
            []
        , node "text"
            [ onClick (UndoList.New <| Click 12), attribute "font-family" "Times New Roman", attribute "font-size" "25", attribute "x" "379.5", attribute "y" "213.5" ]
            [ text "a" ]
        , colored 13
            "path"
            (bold 13 [ d "M 402.712,277.361 A 122.273,122.273 0 0 1 314.441,236.168", fill "none", attribute "stroke" "black" ])
            []
        , colored 13
            "polygon"
            [ attribute "points" "314.441,236.168 315.999,245.472 323.487,238.845" ]
            []
        , node "text"
            [ onClick (UndoList.New <| Click 13), attribute "font-family" "Times New Roman", attribute "font-size" "25", attribute "x" "339.5", attribute "y" "287.5" ]
            [ text "b" ]
        , colored 14
            "path"
            (bold 14 [ d "M 168.916,250.891 A 139.377,139.377 0 0 1 267.703,208.554", fill "none", attribute "stroke" "black" ])
            []
        , colored 14
            "polygon"
            [ attribute "points" "267.703,208.554 259.658,203.627 259.749,213.626" ]
            []
        , node "text"
            [ onClick (UndoList.New <| Click 14), attribute "font-family" "Times New Roman", attribute "font-size" "25", attribute "x" "199.5", attribute "y" "210.5" ]
            [ text "b" ]
        , colored 15
            "path"
            (bold 15 [ d "M 277.619,233.905 A 156.609,156.609 0 0 1 180.436,275.555", fill "none", attribute "stroke" "black" ])
            []
        , colored 15
            "polygon"
            [ attribute "points" "180.436,275.555 188.724,280.061 188.118,270.08" ]
            []
        , node "text"
            [ onClick (UndoList.New <| Click 15), attribute "font-family" "Times New Roman", attribute "font-size" "25", attribute "x" "237.5", attribute "y" "284.5" ]
            [ text "b" ]
        , colored 16
            "path"
            (bold 16 [ d "M 137.139,247.684 A 168.944,168.944 0 0 1 128.919,152.206", fill "none", attribute "stroke" "black" ])
            []
        , colored 16
            "polygon"
            [ attribute "points" "137.139,247.684 138.876,238.411 129.565,242.059" ]
            []
        , node "text"
            [ onClick (UndoList.New <| Click 16), attribute "font-family" "Times New Roman", attribute "font-size" "25", attribute "x" "111.5", attribute "y" "207.5" ]
            [ text "a" ]
        , colored 17
            "path"
            (bold 17 [ d "M 267.666,354.73 A 173.576,173.576 0 0 1 168.282,298.615", fill "none", attribute "stroke" "black" ])
            []
        , colored 17
            "polygon"
            [ attribute "points" "267.666,354.73 260.684,348.385 258.903,358.225" ]
            []
        , node "text"
            [ onClick (UndoList.New <| Click 17), attribute "font-family" "Times New Roman", attribute "font-size" "25", attribute "x" "199.5", attribute "y" "356.5" ]
            [ text "a" ]
        ]


view { past, present, future } =
    let
        { trace, current, pumped } =
            present

        muvw =
            splitTrace trace
    in
        Material.Scheme.top <|
            div [] <|
                [ graph
                    (\id ->
                        if id == current || List.member id trace then
                            colored
                        else
                            node
                    )
                    (\id ->
                        if id == current || Maybe.mapDefault False (\( _, v, _ ) -> List.member id v) muvw then
                            bold
                        else
                            identity
                    )
                ]
                    ++ (if List.isEmpty trace then
                            []
                        else
                            [ case muvw of
                                Nothing ->
                                    div [] [ renderTrace trace ( 0, [] ) [] ]

                                Just ( u, v, w ) ->
                                    div [] [ renderTrace u ( pumped, v ) w ]
                            ]
                       )
                    ++ [ div [] <|
                            (if Maybe.isNothing muvw then
                                (if List.isEmpty past then
                                    []
                                 else
                                    (if List.length past > 1 then
                                        [ Html.button
                                            [ Html.Events.onClick UndoList.Reset ]
                                            [ text "Neu" ]
                                        ]
                                     else
                                        []
                                    )
                                        ++ [ Html.button
                                                [ Html.Events.onClick UndoList.Undo ]
                                                [ text "Zurück" ]
                                           ]
                                )
                                    ++ (if List.isEmpty future then
                                            []
                                        else
                                            [ Html.button
                                                [ Html.Events.onClick UndoList.Redo ]
                                                [ text "Vor" ]
                                            ]
                                       )
                             else
                                [ Slider.view
                                    [ Slider.onChange (UndoList.New << Slider)
                                    , Slider.value (toFloat pumped)
                                    , Slider.min 0
                                    , Slider.max 8
                                    , Slider.step 1
                                    ]
                                , Html.button
                                    [ Html.Events.onClick UndoList.Reset ]
                                    [ text "Neu" ]
                                ]
                            )
                       ]


splitTrace trace =
    let
        { initial, edges } =
            automaton

        targets =
            List.map (flip getSafe edges >> snd >> snd) trace

        firstDuplicateState seen rest =
            case rest of
                head :: tail ->
                    if Set.member head seen then
                        Just head
                    else
                        firstDuplicateState (Set.insert head seen) tail

                [] ->
                    Nothing
    in
        firstDuplicateState (Set.singleton initial) targets
            |> Maybe.map
                (\looped ->
                    case List.elemIndices looped (initial :: targets) of
                        i :: j :: _ ->
                            let
                                ( u, rest ) =
                                    List.splitAt i trace

                                ( v, w ) =
                                    List.splitAt (j - i) rest
                            in
                                ( u, v, w )

                        _ ->
                            Debug.crash "IMPOSSIBLE"
                )


renderTrace u ( pumped, v ) w =
    let
        { states, initial, edges } =
            automaton

        x i f =
            toString (f + i * 120)

        drawState b i state =
            [ (if b then
                colored
               else
                node
              )
                "ellipse"
                [ attribute "cx" (x i 0), attribute "cy" "0", fill "none", attribute "rx" "30", attribute "ry" "30", attribute "stroke" "black" ]
                []
            , node "text"
                [ attribute "font-family" "Times New Roman", attribute "font-size" "20", attribute "x" (x i (-6)), attribute "y" "6" ]
                [ text (getSafe state states) ]
            ]

        addEach b k ( i, id ) acc =
            let
                j =
                    k + i

                ( symbol, ( _, to ) ) =
                    getSafe id edges
            in
                [ (if b then
                    colored
                   else
                    node
                  )
                    "polygon"
                    [ attribute "points" (x j 30 ++ ",0 " ++ x (j + 1) (-30) ++ ",0"), attribute "stroke" "black" ]
                    []
                , (if b then
                    colored
                   else
                    node
                  )
                    "polygon"
                    [ attribute "points" (x (j + 1) (-30) ++ " ,0 " ++ x (j + 1) (-38) ++ ",-5 " ++ x (j + 1) (-38) ++ ",5") ]
                    []
                , node "text"
                    ([ attribute "font-family" "Times New Roman", attribute "font-size" "25", attribute "x" (x (j + 0.5) (-7)), attribute "y" "-9" ]
                        ++ if b then
                            [ fill "red" ]
                           else
                            []
                    )
                    [ text symbol ]
                ]
                    ++ drawState (b && j + 1 < endPumped) (j + 1) to
                    ++ acc

        endPumped =
            List.length u + pumped * List.length v
    in
        drawState False 0 initial
            |> flip (List.foldl (addEach False 0))
                (List.indexedMap (,) u)
            |> flip (List.foldl (addEach True (List.length u)))
                (List.indexedMap (,) (List.concat (List.repeat pumped v)))
            |> flip (List.foldl (addEach False endPumped))
                (List.indexedMap (,) w)
            |> svg [ attribute "version" "1.1", viewBox "-50 -60 1350 120", attribute "height" "72", attribute "width" "810", attribute "xmlns" "http://www.w3.org/2000/svg" ]
