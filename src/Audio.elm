module Audio exposing (..)

import Model exposing (..)
import Note exposing (..)
import WebAudio
import WebAudio.Property as Prop



-- AUDIO ----------------------------------------------------------------------


mtof : Float -> Float
mtof midi =
    440 * 2 ^ ((midi - 69) / 12)



-- This takes a Note (as defined above) and converts that to a synth voice.


voice : Float -> Float -> String -> Note -> WebAudio.Node
voice transpose volume waveType note =
    WebAudio.oscillator
        [ Prop.frequency <| mtof (note.midi + transpose)
        , Prop.type_ waveType
        ]
        [ WebAudio.gain
            [ Prop.gain <|
                if note.triggered then
                    volume / 50

                else
                    0
            ]
            [ WebAudio.dac ]
        ]


voicePiano : Float -> Float -> String -> Note -> WebAudio.Node
voicePiano transpose volume waveType note =
    WebAudio.oscillator
        [ Prop.frequency <| mtof (note.midi + transpose)
        , Prop.type_ waveType
        ]
        [ WebAudio.gain
            [ Prop.gain <|
                if note.triggered then
                    volume / 50

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
    List.map (voice model.transpose model.seqVolume model.seqType) model.notes
        ++ List.map (voicePiano model.transpose model.pianoVolume model.pianoType) model.piano
