module View exposing (..)

import Dict
import Html
    exposing
        ( Html
        , a
        , button
        , div
        , h1
        , main_
        , text
        )
import Html.Attributes as H exposing (class, href, style)
import Html.Events exposing (onClick, onInput)
import Model exposing (..)
import Note exposing (..)
import Scales exposing (..)
import Update exposing (..)



-- VIEW -----------------------------------------------------------------------
-- Use this to toggle the main styling on a note based on wheter it is currently
-- active or note. Basically just changes the background and font colour.


noteCSS : Bool -> String
noteCSS active =
    if active then
        "bg-indigo-500 text-white font-bold py-2 px-4 mr-4 rounded"

    else
        "bg-indigo-100 text-black font-bold py-2 px-4 mr-4 rounded"



-- This takes a Note (as defined above) and converts that to some  Notice
-- how we use the data for both the `voice` function and this `noteView` function.
-- Our audio graph should never become out of sync with our view!


noteView : Note -> Html Msg
noteView note =
    div [ class <| noteCSS note.triggered, class "flex-1 mx-2 text-center" ]
        [ text note.key ]



--


viewSquare : Model -> Float -> Int -> Html Msg
viewSquare model midi beat =
    button
        [ onClick (ToggleNote midi beat)
        , class
            (noteCSS
                (getVal model
                    midi
                    beat
                )
            )
        , style "border" "1px solid black"
        , style "padding" "20px"
        , style "font-size" "small"
        , style "width" "55px"
        ]
        [ text (String.fromInt (beat + 1)) ]


viewTrack : Model -> Float -> String -> Html Msg
viewTrack model midi key =
    div []
        [ div
            [ class "bg-indigo-800 text-white font-bold rounded px-2"
            ]
            [ text key ]
        , div [ style "padding-bottom" "10px", style "padding-top" "10px" ]
            (List.map
                (\x -> viewSquare model midi x)
                (List.range
                    0
                    (model.len - 1)
                )
            )
        ]


viewMeter : Model -> Int -> Html Msg
viewMeter model beat =
    div
        [ style "padding" "20px"
        , class
            (noteCSS
                (if beat == model.len then
                    model.beat == 0

                 else
                    model.beat == beat
                )
            )
        ]
        [ text (String.fromInt beat) ]


viewScales : Model -> Html Msg
viewScales model =
    div
        [ class "mx-2" ]
        [ Html.select
            [ onInput ChangeScale ]
            (List.map
                (\name ->
                    Html.option
                        [ H.value name
                        , H.selected
                            (name
                                == model.scale.name
                            )
                        ]
                        [ text
                            name
                        ]
                )
                (Dict.keys scales)
            )
        ]


viewTypes : Model -> String -> Html Msg
viewTypes model version =
    div
        [ class "mx-2" ]
        [ Html.select
            [ onInput
                (if version == "piano" then
                    ChangePianoType

                 else
                    ChangeSeqType
                )
            ]
            (List.map
                (\name ->
                    Html.option
                        [ H.value name
                        , H.selected
                            (String.toLower name
                                == (if version == "piano" then
                                        model.pianoType

                                    else
                                        model.seqType
                                   )
                            )
                        ]
                        [ text
                            name
                        ]
                )
                [ "Triangle", "Square", "Sine", "Sawtooth" ]
            )
        ]


view : Model -> Html Msg
view model =
    main_ [ class "m-10" ]
        [ h1 [ class "text-3xl my-4" ]
            [ text "Elm Synth Sequencer by Jonah Fleishhacker" ]
        , Html.a
            [ H.href
                "https://drive.google.com/drive/folders/1EHEkSvU7Hy3n05QMOcYDqycGpvSJtxo2?usp=sharing"
            , style "color" "blue"
            , style "font-style" "italic"
            , style "text-decoration" "underline"
            , class "my-4"
            ]
            [ text "Share songs here                     |   " ]
        , Html.a
            [ H.href
                "https://github.com/jonahdf/elm-synth-sequencer"
            , style "color" "blue"
            , style "font-style" "italic"
            , style "text-decoration" "underline"
            , class "my-f"
            ]
            [ text "Source code" ]
        , div [ class "p-1 my-1" ]
            [ button [ onClick InstructionsToggle, class "bg-indigo-500\n\n            text-white font-bold py-1 px-2 mr-1 rounded" ]
                [ if model.instructions then
                    text "Hide instructions"

                  else
                    text "View instructions"
                ]
            ]
        , if model.instructions then
            Html.p [] [ text "Welcome to Elm Synth Sequencer!\n\n\n            You can use your middle keyboard row to play the piano by pressing the keys displayed below. You can also\n            customize the scale and synth type used by the piano with the\n            dropdown selectors below. \n \n            \n            To use the sequencer, use your mouse\n            to select any combination of squares in the grid. Each row\n            corresponds to the labeled note, and each number represents a beat.\n            You may also customize the number of beats, BPM (beats per minute),\n            or transpose the notes up or down with the sliders. You can download\n            or upload your creations, or view other creations with the google\n            drive link at the top under 'share songs here'. Enjoy!" ]

          else
            Html.hr [] []
        , Html.hr [ style "padding-top" "10px" ] []
        , Html.h2 [] [ text "Scale: " ]
        , viewScales model
        , Html.h2 [] [ text "Piano Wave Type: " ]
        , viewTypes model "piano"
        , div []
            [ Html.input
                [ H.type_ "range"
                , class
                    "bg-indigo-500 mr-4"
                , H.min "0"
                , H.max "10"
                , H.step "0.5"
                , H.value
                    (String.fromFloat
                        model.pianoVolume
                    )
                , onInput PianoVolume
                ]
                []
            , text "Piano Volume"
            ]
        , div [ class "flex", style "padding" "10px" ] <|
            List.map noteView model.piano
        , Html.hr [] []
        , Html.h3 [ style "text-align" "center" ] [ Html.u [] [ text "Sequencer\n        Settings" ] ]
        , div [ class "p-2 my-6" ]
            [ button [ onClick PauseToggle, class "bg-indigo-500 text-white\n\n            font-bold py-2 px-4 mr-4 rounded" ]
                [ if model.go then
                    text "Pause"

                  else
                    text "Play"
                ]
            , button [ onClick Clear, class "bg-indigo-500 text-white\n            font-bold\n            py-2 px-4 mr-4 rounded" ]
                [ text "Clear" ]
            , button [ onClick Reset, class "bg-indigo-500 text-white font-bold\n            py-2 px-4 mr-4 rounded" ] [ text "Reset Sliders" ]
            , button [ onClick Download, class "bg-indigo-900 text-white\n\n\n            font-bold rounded mr-4", style "padding" "20px" ] [ text "Download" ]
            , button [ onClick OpenFileClicked, class "bg-indigo-900\n            text-white\n            font-bold rounded", style "padding" "20px" ] [ text "Upload" ]
            ]
        , div []
            [ Html.input
                [ H.type_ "range"
                , class
                    "bg-indigo-500 mr-4"
                , H.min "0"
                , H.max "10"
                , H.step "0.5"
                , H.value
                    (String.fromFloat
                        model.seqVolume
                    )
                , onInput SeqVolume
                ]
                []
            , text "Sequencer Volume"
            ]
        , div []
            [ Html.input
                [ H.type_ "range"
                , class
                    "bg-indigo-500 mr-4"
                , H.min "-15.0"
                , H.max "15.0"
                , H.step "0.5"
                , H.value
                    (String.fromFloat
                        model.transpose
                    )
                , onInput Transpose
                ]
                []
            , text ("Transpose Value: " ++ String.fromFloat model.transpose)
            ]
        , div []
            [ Html.input
                [ H.type_ "range"
                , class "bg-indigo-500 mr-4"

                -- , style "padding" "10px"
                , H.min "50"
                , H.max "1000"
                , H.value
                    (String.fromFloat
                        model.bpm
                    )
                , onInput Bpm
                ]
                []
            , text ("BPM: " ++ String.fromFloat model.bpm)
            ]
        , div [ style "padding-bottom" "10px" ]
            [ Html.input
                [ H.type_ "range"
                , class "bg-indigo-500 mr-4"

                --, style "padding" "10px"
                , H.min "2"
                , H.max "32"
                , H.value
                    (String.fromInt
                        model.len
                    )
                , onInput ChangeBeats
                ]
                []
            , text ("Beats: " ++ String.fromInt model.len)
            ]
        , Html.h2 [] [ text "Sequencer Wave type: " ]
        , viewTypes model "seq"
        , Html.hr [] []
        , Html.h3 [ style "text-align" "center" ] [ Html.u [] [ text "Sequencer\n        Notes" ] ]
        , div [ class "flex", style "padding" "10px" ] <|
            List.map noteView model.notes
        , Html.hr [] []
        , Html.h3 [ style "text-align" "center" ] [ Html.u [] [ text "Beat\n        Indicator" ] ]
        , div
            [ style "padding" "10px"
            , class "flex-container"
            , style "display"
                "flex"
            , style "justify-content" "center"
            ]
          <|
            List.map
                (\x -> viewMeter model x)
                (List.range 1 model.len)
        , Html.hr [ style "padding" "10px" ] []
        , div
            [ style "padding" "10px"
            , class "flex-container"
            , style "flex-direction" "column"
            , style "display"
                "flex"
            , style "justify-content" "left"
            ]
            (List.map
                (\note ->
                    viewTrack model
                        (Tuple.second note)
                        (Tuple.first
                            note
                        )
                )
                model.scale.notes
            )
        ]
