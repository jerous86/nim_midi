import os

import nim_midi/reader

if commandLineParams().len==0:
    echo "nim_midi_reader MIDIFILES"
    quit(1)

for smf in commandLineParams():
    let f=smf.parseMidiFile
    echo f
    echo "Duration: ", f.calculateDuration
    echo f.toString

