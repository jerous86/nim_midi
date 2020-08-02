## This module defines the types used in midi files. Check
## http://www.music.mcgill.ca/~ich/classes/mumt306/StandardMIDIfileformat.html
## for a detailed format of a midi file.

import tables
import endians
import math
import strformat
import sequtils
import strutils


type
  Ticks* = uint32 ## Unit of time for the events in a MIDI file

type
  TimeSignature* = tuple[n:uint8, d:uint8, clocks:uint8, n32Notes:uint8] ## \
    ## Time signature where `n` is the nominator, `d` the denominator written as the log_2 of the real denominator 
    ## signature (hence, only powers of two are possible).
    ## `clocks` expresses the number of MIDI clocks in a metronome click.
    ## `n32Notes` expresses the number of 32nd-notes in a MIDI quarter note (24 MIDI clocks)


type
  Scale* = enum Major, Minor
  KeySignature* = tuple[k:int8,scale:Scale] ## Key signature, where `-k` is the number of flats, if `k<0`.
    ## If `k>0`, k expresses the number of sharps.
    ## For `k=0` we are in the key of C.
    

const defaultKeySignature: KeySignature = (k:int8(0),scale:Major) ## The default key signature, if none is found in the MIDI file
func hasSharps*(ks:KeySignature): bool = ks.k>0 ## Returns true if the key signature contains sharps
func hasFlats*(ks:KeySignature): bool = ks.k<0 ## Returns true if the key signature contains flats


func keySigName*(ks:KeySignature): string =
  ## Gives a textual representation of the key signature
  let names:array[-7..7, string]=
    case ks.scale
    of Major: ["C#","F#","B","E","A","D","G","C","F","Bb","Eb","Ab","Db","Gb","Cb"]
    of Minor: ["A#","D#","G#","C#","F#","B","E","A","D","G","C","F","Bb","Eb","Ab"]
  names[ks.k]


type
  Note* = uint8 ## A MIDI note, ranging from C-1 to G9
  

func transpose*(n:Note, t:int): Note =
  ## Transposes a note by `t` semi-notes
  (assert(int(n)+t>=0 and int(n)+t<255); Note(int(n)+t))


func toOctave0*(n:Note): Note =
  ## Converts note `n` to the range 0..11, mapping it to octave 0
  n mod 12


func octave*(n:Note): uint8 =
  ## Returns the octave belonging to note `n`
  (n div 12) - 1

func noteName*(n:Note, ks:KeySignature=defaultKeySignature, withOctave:bool=true): string =
  ## Returns a full textual representation (i.e. note name and octave) of note `n`, taking the key signature `ks` into account
  const noteNamesSharps = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"]
  const noteNamesFlats = ["C","Db","D","Eb","E","F","Gb","G","Ab","A","Bb","B"]
  (if ks.hasSharps:noteNamesSharps else:noteNamesFlats)[n.toOctave0] & (if withOctave: $n.octave else: "")

func midiNameToNote*(n:string): Note =
  ## Parses a textual representation of note `n` to a MIDI note.
  ## If the octave is not specified, octave 5 is used
  ## http://www.richardbrice.net/midi_notes.htm
  runnableExamples:
    assert midiNameToNote("C0")==0
    assert midiNameToNote("A1")==21 ## Lowest note on a regular piano
    assert midiNameToNote("C5")==60 ## Middle C
    assert midiNameToNote("C")==60 ## Middle C
    assert midiNameToNote("C9")==108 ## Highest note on a regular piano
    assert midiNameToNote("G10")==127
  const noteValues = {"C":0, "C#":1, "Db":1, "D":2, "D#":3, "Eb":3, "E":4, "F":5, "F#":6,
    "Gb":6, "G":7, "G#":8, "Ab":8, "A":9, "A#":10, "Bb":10, "B": 11}.toTable
  assert n.len>=1
  try:
    if n[0..1] in noteValues:
      let n2=n[0..1]
      let o=n[2..^1].parseInt
      Note(noteValues[n2]+o*12)
    else:
      let n2=n[0..0]
      let o=n[1..^1].parseInt
      Note(noteValues[n2]+o*12)
  except:
    let o=5
    Note(noteValues[n]+o*12)


type
  Channel* = uint8
  Port* = uint8
  
  MidiStatus* = enum
    NoteOff=0x80,
    NoteOn=0x90,
    NoteAfterTouch=0xA0,
    CtrlChange=0xB0,
    PgmChange=0xC0,
    ChannelAfterTouch=0xD0,
    PitchBend=0xE0,
    
  MetaEvent* {.pure.} = enum
    SeqNum = 0x00,
    Text = 0x01,
    Copyright = 0x02,
    TrackName = 0x03,
    InstrumentName = 0x04,
    Lyrics = 0x05,
    Marker = 0x06,
    CuePoint = 0x07,
    ChannelPrefix = 0x20,
    MidiPort= 0x21,
    EndOfTrack = 0x2F,
    SetTempo = 0x51,
    SMPTEOffset = 0x54,
    TimeSignature = 0x58,
    KeySignature = 0x59,
    SeqSpecific = 0x7F
  
  Format* = enum ## Organisation of the MIDI file.
      SingleTrack, ## the file contains a single multi-channel track
      MultipleTracks,## the file contains one or more simultaneous tracks of a sequence 
      MultipleSongs ## the file contains one or more sequentially independent single-track patterns
    
    
    
    
    
  
  MThdChunk* = object ## MIDI header chunk representation
    format*: Format
    num_tracks*: uint16 ## Number of tracks in the MIDI file
    division*: uint16 ## Meaning of delta-times. 
       ## If the MSB is 0, then bit 14..0 represent the number of delta time ticks make up a quarter note.
       ## For instance, if division is 96, then a time interval of an eighth-note between two events in the file would be 48.
       ## If the MSB is 1, then delta times in a file correspond to subdivisions of a second, in a way consistent with SMPTE and MIDI Time Code.
       ## See the MIDI specs for more information.
  
  EventType* {.pure.} = enum MidiEvent, SysexEvent, MetaEvent
  
  
  Event* = object ## A MIDI event
    delta_time*: Ticks ## Time passed since the last event
    abs_time*: Ticks ## Ticks passed since the beginning of the file (i.e. 0).
      ## This is calculated while reading the file. It is never used by this library, but for
      ## textual representation.
    case etype*: EventType
      of MidiEvent:
        mi_status*: MidiStatus 
        mi_channel*: Channel
        mi_par1*: uint8
        mi_par2*: uint8
      of SysexEvent:
        se_c*: uint8 # Either 0xF0 or 0xF7
        se_bytes*: seq[uint8]
      of EventType.MetaEvent:
        me_type*: MetaEvent
        me_bytes*: seq[uint8]
    
  
  MTrkChunk* = object ## Track chunk
    events*: seq[Event] ## All the events pertaining to the track
  
  MidiFile* = object
    header*: MThdChunk
    tracks*: seq[MtrkChunk]

func `==`*(l,r:Event):bool =
  if l.etype!=r.etype: false
  # We should not use abs_time
  #elif not (l.delta_time==r.delta_time and l.abs_time==r.abs_time): false
  elif not (l.delta_time==r.delta_time): false
  else:
    case l.etype
      of MidiEvent: l.mi_status==r.mi_status and l.mi_channel==r.mi_channel and l.mi_par1==r.mi_par1 and l.mi_par2==r.mi_par2
      of SysexEvent: l.se_c==r.se_c and l.se_bytes==r.se_bytes
      of EventType.MetaEvent: l.me_type==r.me_type and l.me_bytes==r.me_bytes
  
func indent(s:seq[string]): seq[string] = s.mapIt("\t" & it)


func isTimeSignature*(e:Event): bool = e.etype==EventType.MetaEvent and e.me_type==MetaEvent.TimeSignature
func getTimeSignature*(e:Event): TimeSignature =
  assert e.isTimeSignature
  (uint8(e.me_bytes[0]), uint8(2^uint8(e.me_bytes[1])), uint8(e.me_bytes[2]), uint8(e.me_bytes[3]))

func isKeySignature*(e:Event): bool = e.etype==EventType.MetaEvent and e.me_type==MetaEvent.KeySignature
func getKeySignature*(e:Event): KeySignature =
  assert e.isKeySignature
  (k:cast[int8](e.me_bytes[0]), scale: (if cast[uint8](e.me_bytes[1])==0: Major else: Minor))

func toString*(s:seq[uint8]): string =
  ## Display bytes as a string
  s.mapIt(char(it)).join

func toHexString*(s:seq[uint8]): string =
  ## Display bytes as a hex string
  "0x[" & s.mapIt(&"{int(it):X}").join(" ") & "]"

func toUint32*(s:seq[uint8]): uint32 =
  ## Convert a sequence of bytes to an uint32
  var s2=s # must be mutable
  while s2.len<4: s2.insert(uint8(0),0)
  bigEndian32(addr(result), addr(s2[0]))
  
func toUint16*(s:seq[uint8]): uint16 =
  ## Converts a sequence of bytes to an uint16
  var s2=s # must be mutable
  bigEndian16(addr(result), addr(s2[0]))

func isTempo*(e:Event): bool = e.etype==EventType.MetaEvent and e.me_type==MetaEvent.SetTempo
func getTempo*(e:Event): uint32 =
  assert e.isTempo
  toUint32(e.me_bytes[0..2])
func getBPM*(e:Event): uint =
  ## Converts a midi SetTempo event to a beats per minute
  assert e.isTempo
  uint(float(60_000_000)/float(e.getTempo))

func toString*(h:MThdChunk): string
func toString*(e:Event): string
func toString*(t:MtrkChunk): string
func toString*(m:MidiFile): string


func toString*(h:MThdChunk): string =
  let str_div=(if h.division<0x7FFF: &"{h.division} ticks/(quarter note)" else: "TODO")
  &"Format: {h.format}, with {h.num_tracks} tracks, {str_div}"
  
func toString*(e:Event): string =
  let info=case e.etype
    of MidiEvent: 
      let info2=case e.mi_status
        of NoteOff, NoteOn, NoteAfterTouch: &"{e.mi_par1.noteName()} (Value {e.mi_par2})"
        of CtrlChange: &"controller number {e.mi_par1}, new value {e.mi_par2}"
        of PgmChange: &"new program number {e.mi_par1}"
        of ChannelAfterTouch: &"value {e.mi_par1}"
        of PitchBend: &"{e.mi_par1}  {e.mi_par2}"
      &"{e.mi_status} {info2} (channel {e.mi_channel})"
    of SysexEvent: &"System Exclusive {e.se_bytes.toHexString}"
    of EventType.MetaEvent: 
      let bytes=e.me_bytes
      var info2="(Unknown meta event)"
      info2=case e.me_type
        of SeqNum: $toUint16(bytes)
        of Text, Copyright, TrackName, InstrumentName, Lyrics, Marker, CuePoint: &"\"{toString(bytes)}\" ({toHexString(bytes)})"
        of MidiPort: &"Midi port: {Port(bytes[0])}"
        of ChannelPrefix: &"Channel {Channel(bytes[0])}"
        of EndOfTrack: ""
        of SetTempo: &"{e.getTempo}"
        of SMPTEOffset: &"{uint8(bytes[0])}h {uint8(bytes[1])}m {uint8(bytes[2])}s {uint8(bytes[3])} fr {uint8(bytes[4])} ff"
        of MetaEvent.TimeSignature: 
          let (n,d,c,nc)=e.getTimeSignature
          &"{n}/{d}; {c} (MIDI clocks)/(metronome click); {nc} (1/32 notes)/(23 MIDI clicks)"
        of MetaEvent.KeySignature:
          let ks=e.getKeySignature
          &"{ks.keySigName} -- (ks.scale)"
        of SeqSpecific: $bytes
      &"{e.me_type} {info2}"
  &"+{e.delta_time} -> {e.abs_time}: {info}"

func toString*(t:MtrkChunk): string = "TRACK\n" & t.events.map(toString).indent.join("\n")
func toString*(m:MidiFile): string = &"HEADER: {m.header}\nTracks ({m.tracks.len}):\n" & m.tracks.map(toString).join("\n")

func calculateDuration*(m:MidiFile): float =
    ## Returns the time of the last note event in seconds, taking into account tempo changes
    let ppq=m.header.division
    result=0.0
    var curTempo:uint=0
    for t in m.tracks:
        var sec=0.0
        
        #seconds = ticks * ((<Tempo in latest Set Tempo event>/<PPQ from the header>)/ 1.000.000)
        var lastTempoChange:Ticks=0
        for e in t.events:
            if e.isTempo:
                sec += float(e.abs_time-lastTempoChange) * (float(curTempo)/float(ppq)/1e6)
                curTempo=e.getTempo
                lastTempoChange=e.abs_time
        block:
            let e=t.events[^1]
            sec += float(e.abs_time-lastTempoChange) * (float(curTempo)/float(ppq)/1e6)
        result=max(result, sec)
    
    return result

