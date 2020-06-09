module Scales exposing (..)

import Dict exposing (Dict)
import Note exposing (..)


scales : Dict String Scale
scales =
    Dict.fromList
        [ ( "Diatonic"
          , Scale "Diatonic"
                [ ( "C4", 60 )
                , ( "D4", 62 )
                , ( "E4", 64 )
                , ( "F4", 65 )
                , ( "G4", 67 )
                , ( "A4", 69 )
                , ( "B4", 71 )
                , ( "C5", 72 )
                , ( "D5", 74 )
                , ( "E5", 76 )
                ]
          )
        , ( "Pentatonic"
          , Scale "Pentatonic"
                [ ( "C4", 60 )
                , ( "D4", 62 )
                , ( "E4", 64 )
                , ( "G4", 67 )
                , ( "A4", 69 )
                , ( "C5", 72 )
                , ( "D5", 74 )
                , ( "E5", 76 )
                , ( "G5", 79 )
                , ( "A5", 81 )
                ]
          )
        , ( "Blues"
          , Scale "Blues"
                [ ( "C4", 60 )
                , ( "Eb4", 63 )
                , ( "F4", 65 )
                , ( "Gb4", 66 )
                , ( "G4", 67 )
                , ( "B4", 70 )
                , ( "C5", 72 )
                , ( "Eb5", 75 )
                , ( "F5", 77 )
                , ( "Gb5", 78 )
                ]
          )
        , ( "Major Arpeggio"
          , Scale "Major Arpeggio"
                [ ( "C3", 48 )
                , ( "E3", 52 )
                , ( "G3", 55 )
                , ( "C4", 60 )
                , ( "E4", 64 )
                , ( "G4", 67 )
                , ( "C5", 72 )
                , ( "E5", 76 )
                , ( "G5", 79 )
                , ( "C6", 84 )
                ]
          )
        , ( "Minor Arpeggio"
          , Scale "Minor Arpeggio"
                [ ( "A2", 45 )
                , ( "C3", 48 )
                , ( "E3", 52 )
                , ( "A3", 57 )
                , ( "C4", 60 )
                , ( "E4", 64 )
                , ( "A4", 69 )
                , ( "C5", 72 )
                , ( "E5", 76 )
                , ( "A5", 81 )
                ]
          )
        , ( "Chromatic"
          , Scale "Chromatic"
                [ ( "C4", 60 )
                , ( "C#4", 61 )
                , ( "D4", 62 )
                , ( "D#4", 63 )
                , ( "E4", 64 )
                , ( "F4", 65 )
                , ( "F#4", 66 )
                , ( "G4", 67 )
                , ( "G#4", 68 )
                , ( "A4", 69 )
                , ( "A#4", 70 )
                , ( "B4", 71 )
                , ( "C5", 72 )
                , ( "C#5", 73 )
                , ( "D5", 74 )
                , ( "D#5", 75 )
                , ( "E5", 76 )
                , ( "F5", 77 )
                , ( "F#5", 78 )
                , ( "G5", 79 )
                , ( "G#5", 80 )
                , ( "A5", 81 )
                , ( "A#5", 82 )
                , ( "B5", 83 )
                , ( "C6", 84 )
                ]
          )
        ]


pentatonic : Scale
pentatonic =
    Scale "Pentatonic"
        [ ( "C4", 60 )
        , ( "D4", 62 )
        , ( "E4", 64 )
        , ( "G4", 67 )
        , ( "A4", 69 )
        , ( "C5", 72 )
        , ( "D5", 74 )
        , ( "E5", 76 )
        , ( "G5", 79 )
        , ( "A5", 81 )
        ]


pianoKeys : List Note -> List Note
pianoKeys notes =
    List.map2 (\note k -> { note | key = k })
        notes
        [ "a"
        , "s"
        , "d"
        , "f"
        , "g"
        , "h"
        , "j"
        , "k"
        , "l"
        , ";"
        , "q"
        , "w"
        , "e"
        , "r"
        , "t"
        , "y"
        , "u"
        , "i"
        , "o"
        , "p"
        , "["
        , "]"
        , "1"
        , "2"
        , "3"
        , "4"
        , "5"
        , "6"
        , "7"
        , "8"
        , "9"
        , "0"
        ]


type alias Scale =
    { name : String
    , notes : List ( String, Float )
    }


scaleInit : Scale -> List Note
scaleInit scale =
    List.map (\note -> Note (Tuple.first note) (Tuple.second note) False)
        scale.notes
