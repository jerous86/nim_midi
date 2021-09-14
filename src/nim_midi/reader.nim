import streams
import strformat
import bitops
import endians

import types

export types

  
proc readVarLen(s:var Stream): uint32 =
  ## Reads a MIDI variable length quantity from stream `s`.
  var value=s.readUint8
  result=uint32(value)

  if bitand(result,0x80)!=0:
    result = bitand(result,0x7F)
    while true:
      value=s.readUint8
      result = result.rotateLeftBits(7) + uint32(bitand(value,0x7f))
      if bitand(value,0x80)==0: break

proc readBytes(s:var Stream, n:uint): seq[uint8] =
  if n==0: return
  result=newSeq[uint8](n)
  let read=s.readData(addr(result[0]), int(n))
  assert read==int(n), &"{read} == {n}"
  
proc readMagicBytes(s:var Stream, expected:seq[uint8]) =
  let read=s.readBytes(uint(expected.len))
  assert(read==expected, &"{read} == {expected}")

proc readUInt32BE(s:var Stream): uint32 = (var x=s.readUInt32; bigEndian32(addr(result), addr(x)))
proc readUInt16BE(s:var Stream): uint16 = (var x=s.readUInt16; bigEndian16(addr(result), addr(x)))

proc parseHeader(s:var Stream): MThdChunk =
  let length=s.readUInt32BE
  assert length==6
  result.format=Format(s.readUInt16BE)
  result.num_tracks=s.readUInt16BE
  result.division=s.readUInt16BE

#func debug(s:string) = debugEcho s
func debug(s:string) = discard
  
proc parseEvent(s:var Stream, prevC:uint8): (Event,uint8) =
  let delta_time=s.readVarLen
  let c=s.readUint8
  var event:Event
  
  result[1]=c
  
  debug &"delta_time {delta_time}: parseEvent:c=0x{c:X}, prevC=0x{prevC:X}"
  if c==uint8(0xFF):
    event=Event(delta_time:delta_time, etype:EventType.MetaEvent)
    event.me_type=MetaEvent(s.readUint8)
    event.me_bytes=(let length=s.readVarLen; s.readBytes(length))
    debug &"    {event.etype} {event.me_type} {event.me_bytes.toHexString}"
  elif c==uint8(0xF0) or c==uint8(0xF7):
    event=Event(delta_time:delta_time, etype:SysexEvent)
    event.se_c=c
    event.se_bytes=(let length=s.readVarLen; s.readBytes(length))
    debug &"    {event.etype} {event.se_bytes.toHexString}"
  else:
    debug &" --> status:0x{bitand(uint8(0xF0),c):x}  channel:0x{bitand(uint8(0x0F),c):x}"
    event=Event(delta_time:delta_time, etype:MidiEvent)
    if c<=uint(0x7F): # I.e. MSB is zero. This means we reuse the status of the last event!
      event.mi_status=MidiStatus(bitand(uint8(0xF0), prevC))
      event.mi_channel=bitand(uint8(0x0F), prevC)
      s.setPosition(s.getPosition-1)
      result[1]=prevC
    else:
      event.mi_status=MidiStatus(bitand(uint8(0xF0),c))
      event.mi_channel=bitand(uint8(0x0F),c)
      
    case event.mi_status
      of NoteOff, NoteOn, NoteAfterTouch, CtrlChange, PitchBend:
        event.mi_par1=s.readUint8
        event.mi_par2=s.readUint8
      of PgmChange, ChannelAfterTouch:
        event.mi_par1=s.readUint8
        event.mi_par2=0 # None
    
    # Special case handling
    if event.mi_status==NoteOn and event.mi_par2==0:
      # By definition, a note-on message with vv=0 is equivalent to the message: "note-off vv=40"
      # We prefer to show NoteOff, as it's more clear for visual inspection
      event.mi_status=NoteOff
      event.mi_par2=40
    debug &"    {event.etype} {event.mi_status} { event.mi_par1} {event.mi_par2}"
    
  result[0]=event
  
  
proc parseTrack(s:var Stream): MtrkChunk =
  let EOT=Event(delta_time:0, etype:EventType.MetaEvent, me_type:EndOfTrack)
  
  s.readMagicBytes(@[uint8(0x4D), uint8(0x54), uint8(0x72), uint8(0x6B)]) # MTrk
  let byte_len=int(s.readUInt32BE)
  let start_of_track=s.getPosition
  
  debug &"Bytes for new track: {byte_len}: {start_of_track} --> {start_of_track+byte_len}"
  var prevC:uint8=0
  while s.getPosition<start_of_track+byte_len:
    let (event,c)=parseEvent(s,prevC)
    prevC=c
    result.events.add(event)
    result.events[^1].abs_time=(if result.events.len>=2: result.events[^2].abs_time else: 0)+result.events[^1].delta_time
    debug &"Event {result.events.len} ({s.getPosition}/{start_of_track+byte_len}): {result.events[^1]}"
  assert result.events[^1].me_type==EOT.me_type
  debug "End of track!"

proc parseMidiFile*(s:var Stream): MidiFile =
  ## Parses the MIDI file given by the Stream `s`
  s.readMagicBytes(@[uint8(0x4D), uint8(0x54), uint8(0x68), uint8(0x64)]) # Equals MThd
  result.header = s.parseHeader
  for i in uint16(0)..result.header.num_tracks-1:
    result.tracks.add(s.parseTrack)

proc parseMidiFile*(f:string): MidiFile =
  ## Opens file `f` as a stream which is passed to parseMidiFile
  try:
      var s:Stream=openFileStream(f)
      assert s!=nil
      result=s.parseMidiFile
      s.close()
  except:
    stderr.write getCurrentExceptionMsg()
    quit(1)

