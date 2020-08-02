import streams
#import strformat
import math
import bitops
import endians

import types
export types
  
proc writeVarLen(s:var Stream, v:uint32) =
  ## Encodes a 32-bit integer as a MIDI variable length quantity
  func rshift7(x:uint32): uint32 = (x - x.bitand(0x7F)).rotateRightBits(7)
  var v2=v
  
  var bytes:seq[uint8]
  if true:
    bytes.add uint8(v2.bitand(0x7F))
    v2=v2.rshift7
  
  while v2>0:
    bytes.add uint8(v2.bitand(0x7F).bitor(0x80))
    v2=v2.rshift7
  
  # Write in reverse
  for i in 0..bytes.len-1: s.write(uint8(bytes[bytes.len-1-i]))

proc writeUInt32BE(s:var Stream, x:uint32) = (var x_out:uint32=0; var x_in=x; bigEndian32(addr(x_out), addr(x_in)); s.write(x_out))
proc writeUInt16BE(s:var Stream, x:uint16) = (var x_out:uint16=0; var x_in=x; bigEndian16(addr(x_out), addr(x_in)); s.write(x_out))
proc writeUInt8(s:var Stream, x:uint8) = s.write(x)
proc writeBytes(s:var Stream, xs:seq[uint8]) = ( for x in xs: s.write(x) )

proc writeHeader(s:var Stream, m:MThdChunk) =
  s.write("MThd")
  s.writeUInt32BE(6)
  s.writeUInt16BE(uint16(m.format))
  s.writeUInt16BE(uint16(m.num_tracks))
  s.writeUInt16BE(uint16(m.division))

proc writeEvent(s:var Stream, e:Event, prevEvent:Event) = 
  func getStatusChannel(e:Event): uint8 = 
    if e.etype==EventType.MidiEvent: uint8(e.mi_channel)^4+uint8(e.mi_status)
    else: 0

  # Disabled this assert, as we should not be using absolute times.
  #assert e.delta_time==e.abs_time-prevEvent.abs_time, &"{e.delta_time} == {e.abs_time} - {prevEvent.abs_time}\n\te = {e}\n\tprevEvent = {prevEvent}"
  s.writeVarLen(e.delta_time)
  
  case e.etype
    of EventType.MetaEvent:
      s.writeUInt8(0xFF)
      s.writeUInt8(uint8(e.me_type))
      s.writeVarLen(uint32(e.me_bytes.len))
      s.writeBytes(e.me_bytes)
    of EventType.SysexEvent:
      s.writeUInt8(e.se_c)
      s.writeVarLen(uint32(e.se_bytes.len))
      s.writeBytes(e.se_bytes)
    of EventType.MidiEvent:
      if prevEvent.getStatusChannel!=e.getStatusChannel:
        s.writeUInt8(e.getStatusChannel)
      s.writeUInt8(e.mi_par1)
      case e.mi_status
        of NoteOff, NoteOn, NoteAfterTouch, CtrlChange, PitchBend: s.writeUInt8(e.mi_par2)
        of PgmChange, ChannelAfterTouch: discard
  
proc writeTrack(s:var Stream, t:MtrkChunk) = 
  let EOT=Event(delta_time:0, etype:EventType.MetaEvent, me_type:EndOfTrack, me_bytes: @[])
  s.write("MTrk")
  let len_pos=s.getPosition
  s.writeUInt32BE(0)
  let start_pos=s.getPosition
  assert len_pos+4==start_pos
  
  for i,e in t.events:
    # TODO use NoteOn with v==0 for NoteOff? That compresses more.
    s.writeEvent(e, (if i>=1: t.events[i-1] else: EOT))
  
  assert t.events[^1].me_type==EOT.me_type
  let end_pos=s.getPosition
  
  s.setPosition(len_pos)
  s.writeUInt32BE(uint32(end_pos-start_pos))
  s.setPosition(end_pos)
  
proc writeMidiFile*(s:var Stream, m:MidiFile) =
  ## Writes the MIDI file `m` to the stream `s`
  s.writeHeader(m.header)
  for t in m.tracks:
    s.writeTrack(t)

proc writeMidiFile*(f:string, m:MidiFile) =
  ## Writes `m` to the filepath `f`
  var s:Stream=newFileStream(f, fmWrite)
  s.writeMidiFile(m)

