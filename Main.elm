port module Main exposing (..)

--
--
--

import Array as A
import Browser
import Browser.Events
import Html
    exposing
        ( Attribute
        , Html
        , a
        , button
        , code
        , div
        , h1
        , main_
        , p
        , pre
        , text
        )
import Html.Attributes as H exposing (class, href, style)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Json.Encode
import Time
import WebAudio
import WebAudio.Context as Cont
import WebAudio.Program
import WebAudio.Property as Prop



-- Send the JSON encoded audio graph to javascript


port updateAudio : Json.Encode.Value -> Cmd msg



-- MAIN -----------------------------------------------------------------------


main : Program Cont.AudioContext Model Msg
main =
    WebAudio.Program.element
        { init = init
        , update = update
        , audio = audio
        , view = view
        , subscriptions = subscriptions
        , audioPort = updateAudio
        }



-- MODEL ----------------------------------------------------------------------
--


type alias Note =
    { key : String
    , midi : Float
    , triggered : Bool
    }



--
--name: name of track, tone: note key, beats: Array of booleans


type alias Track =
    { key : String
    , midi : Float
    , beats : A.Array Bool
    }


type alias Model =
    { notes : List Note
    , tracks : A.Array Track
    , beat : Int
    , len : Int
    , context : Cont.AudioContext
    , go : Bool
    , transpose : Float
    , bpm : Float
    }



--


initialModel : List Note
initialModel =
    [ { key = "c", midi = 60, triggered = False }
    , { key = "d", midi = 62, triggered = False }
    , { key = "e", midi = 64, triggered = False }
    , { key = "f", midi = 65, triggered = False }
    , { key = "g", midi = 67, triggered = False }
    , { key = "a", midi = 69, triggered = False }
    , { key = "b", midi = 71, triggered = False }
    , { key = "c", midi = 72, triggered = False }
    , { key = "d", midi = 74, triggered = False }
    ]


midis =
    [ 60, 62, 64, 65, 67, 69, 71, 72, 74 ]


initialTrackHelp : A.Array Track
initialTrackHelp =
    A.fromList
        (List.map
            (\note ->
                { key = note.key
                , midi = note.midi
                , beats = A.repeat 4 False
                }
            )
            initialModel
        )


trackToggle : A.Array Track -> Float -> Int -> A.Array Track
trackToggle tracks midi beat =
    A.map
        (\track ->
            if track.midi == midi then
                case A.get beat track.beats of
                    Just bool ->
                        Track track.key
                            track.midi
                            (A.set beat
                                (not bool)
                                track.beats
                            )

                    Nothing ->
                        Track track.key track.midi track.beats

            else
                Track track.key track.midi track.beats
        )
        tracks


trackSetList : A.Array Track -> List ( Float, Int ) -> A.Array Track
trackSetList tracks lst =
    case lst of
        [] ->
            tracks

        ( midi, beat ) :: tl ->
            trackSetList (trackToggle tracks midi beat) tl


getValIndex : Model -> Int -> Int -> Bool
getValIndex model index beat =
    case A.get index model.tracks of
        Just t ->
            case A.get beat t.beats of
                Just b ->
                    b

                Nothing ->
                    False

        Nothing ->
            False



--


getVal : Model -> Float -> Int -> Bool
getVal model midi beat =
    getValIndex model (midiToIndex model midi) beat


midiToIndex : Model -> Float -> Int
midiToIndex model midi =
    let
        check i lst =
            case lst of
                [] ->
                    Debug.todo "not in model"

                h :: tl ->
                    if h.midi == midi then
                        i

                    else
                        check (i + 1) tl
    in
    check 0 (A.toList model.tracks)


initialTrack : A.Array Track
initialTrack =
    trackSetList initialTrackHelp s


init co =
    ( { notes = initialModel
      , tracks = initialTrack
      , context = co
      , beat = 0
      , len = 4
      , go = True
      , transpose = 0
      , bpm = 120
      }
    , Cmd.none
    )



{- s =
   [ ( 60, 2 )
   , ( 62, 2 )
   , ( 64, 4 )
   , ( 65, 3 )
   , ( 67, 5 )
   , ( 69
     , 8
     )
   ]
-}


s =
    []



-- UPDATE ---------------------------------------------------------------------


type Msg
    = NoOp
      --
      --| NoteOn String
      --| NoteOff String
      --
    | TransposeUp
    | TransposeDown
    | NextStep Time.Posix
    | ToggleNote Float Int
    | PauseToggle
    | Clear
    | Bpm String



--
{-
   noteOn : String -> Model -> Model
   noteOn key model =
       { model
           | notes =
               List.map
                   (\note ->
                       if note.key == key then
                           { note | triggered = True }

                       else
                           note
                   )
                   model.notes
       }
-}


updateTrack : Model -> Model
updateTrack model =
    { model
        | beat =
            if model.beat == model.len then
                0

            else
                model.beat + 1
        , notes =
            A.toList
                (A.map
                    (\track ->
                        case A.get model.beat track.beats of
                            Just bool ->
                                Note track.key track.midi bool

                            Nothing ->
                                Note track.key track.midi False
                    )
                    model.tracks
                )
    }



--
{-
   noteOff : String -> Model -> Model
   noteOff key model =
       { model
           | notes =
               List.map
                   (\note ->
                       if note.key == key then
                           { note | triggered = False }

                       else
                           note
                   )
                   model.notes
       }

-}


transposeUp : Model -> Model
transposeUp model =
    { model
        | transpose = model.transpose + 0.5
    }


transposeDown : Model -> Model
transposeDown model =
    { model
        | transpose = model.transpose - 0.5
    }


updateToggle : Model -> Float -> Int -> Model
updateToggle model midi beat =
    { model
        | tracks = trackToggle model.tracks midi beat
    }


pauseToggle : Model -> Model
pauseToggle model =
    { model
        | go =
            if model.go then
                False

            else
                True
        , notes = []
    }


clear : Model -> Model
clear model =
    { model
        | tracks =
            A.map
                (\track -> { track | beats = A.repeat model.len False })
                model.tracks
    }


updateBpm : Model -> String -> Model
updateBpm model string =
    { model
        | bpm =
            case String.toFloat string of
                Just i ->
                    i

                Nothing ->
                    model.bpm
    }



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            Tuple.pair model Cmd.none

        {-
           NoteOn key ->
               ( noteOn key model
               , Cmd.none
               )

           NoteOff key ->
               ( noteOff key model
               , Cmd.none
               )
        -}
        PauseToggle ->
            ( pauseToggle model, Cmd.none )

        TransposeUp ->
            ( transposeUp model
            , Cmd.none
            )

        TransposeDown ->
            ( transposeDown model
            , Cmd.none
            )

        NextStep newTime ->
            if model.go then
                ( updateTrack model
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        ToggleNote midi beat ->
            ( updateToggle model midi beat, Cmd.none )

        Clear ->
            ( clear model, Cmd.none )

        Bpm string ->
            ( updateBpm model string, Cmd.none )



-- AUDIO ----------------------------------------------------------------------
-- Super simple utility function that takes a MIDI note number like 60 and
-- converts it to the corresponding frequency in Hertz. We use Float for the
-- MIDI number to allow for detuning, and we assume A4 is MIDI note number
-- 69.


mtof : Float -> Float
mtof midi =
    440 * 2 ^ ((midi - 69) / 12)



-- This takes a Note (as defined above) and converts that to a synth voice.


voice : Float -> Note -> WebAudio.Node
voice transpose note =
    WebAudio.oscillator [ Prop.frequency <| mtof (note.midi + transpose) ]
        [ WebAudio.gain
            [ Prop.gain <|
                if note.triggered then
                    0.2

                else
                    0
            ]
            [ WebAudio.dac ]
        ]



-- On the js side, the virtual audio graph is expecting an array of virtual
-- nodes. This plays nicely with our list of Notes, we can simply map the
-- Notes to synth voices and encode the new list.
-- Returns a Cmd Msg as we call the port from within this function (rather
-- than returning the encoded JSON).


audio : Model -> WebAudio.Graph
audio model =
    List.map (voice model.transpose) model.notes



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
        , style "padding" "20px"
        ]
        [ text (Debug.toString beat) ]


viewTrack : Model -> Float -> Html Msg
viewTrack model midi =
    div [ class "track" ]
        [ p [] [ text (Debug.toString midi) ]
        , div []
            (List.map
                (\x -> viewSquare model midi x)
                (List.range
                    0
                    (model.len - 1)
                )
            )
        ]


view : Model -> Html Msg
view model =
    main_ [ class "m-10" ]
        [ h1 [ class "text-3xl my-10" ]
            [ text "Elm Synth Sequencer by Jonah Fleishhacker" ]
        , div [ class "p-2 my-6" ]
            [ button [ onClick TransposeUp, class "bg-indigo-500 text-white font-bold py-2 px-4 mr-4 rounded" ]
                [ text "Transpose up" ]
            , button [ onClick TransposeDown, class "bg-indigo-500 text-white\n            font-bold py-2 px-4 mr-4 rounded" ]
                [ text "Transpose down" ]
            , button [ onClick PauseToggle, class "bg-indigo-500 text-white\n\n            font-bold py-2 px-4 mr-4 rounded" ]
                [ if model.go then
                    text "Pause"

                  else
                    text "Play"
                ]
            , button [ onClick Clear, class "bg-indigo-500 text-white\n            font-bold\n            py-2 px-4 mr-4 rounded" ]
                [ text "Clear" ]
            ]
        , Html.input
            [ H.type_ "range"
            , class "bg-indigo-500 mr-4"
            , style "padding" "10px"
            , H.min "50"
            , H.max "500"
            , H.value
                (Debug.toString
                    model.bpm
                )
            , onInput Bpm
            ]
            []
        , div [ class "flex", style "padding-bottom" "30px" ] [ text (" BPM: " ++ Debug.toString model.bpm) ]
        , div [ class "flex" ] <|
            List.map noteView model.notes
        , div []
            (List.map
                (\midi -> viewTrack model midi)
                midis
            )
        ]



-- SUBSCRIPTIONS --------------------------------------------------------------
--
{-

   noteOnDecoder : List Note -> Json.Decode.Decoder Msg
   noteOnDecoder notes =
       Json.Decode.field "key" Json.Decode.string
           |> Json.Decode.andThen
               (\key ->
                   case List.any (\note -> note.key == key) notes of
                       True ->
                           Json.Decode.succeed (NoteOn key)

                       False ->
                           Json.Decode.fail ""
               )



   --


   noteOffDecoder : List Note -> Json.Decode.Decoder Msg
   noteOffDecoder notes =
       Json.Decode.field "key" Json.Decode.string
           |> Json.Decode.andThen
               (\key ->
                   case List.any (\note -> note.key == key) notes of
                       True ->
                           Json.Decode.succeed (NoteOff key)

                       False ->
                           Json.Decode.fail ""
               )

-}
--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ --Browser.Events.onKeyDown <| noteOnDecoder model.notes
          --, Browser.Events.onKeyUp <| noteOffDecoder model.notes
          Time.every (60000 / model.bpm) NextStep

        --, Cont.every 3 model.time NoOp NextStep model.context
        ]
