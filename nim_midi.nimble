# Package

version       = "0.1"
author        = "jerous"
description   = "Read and write midi files"
license       = "MIT"
srcDir        = "src"

requires "nim >= 1.2.0"

task docgen, "generate docs":
  exec "nim doc --out:docs/ --project --index:on  src/nim_midi.nim"

task reader, "Builds a simple midi file reader that displays all midi events":
  exec "nim c --out:nim_midi_reader test/nim_midi_reader.nim"

