import nim_midi/types
import nim_midi/reader
import nim_midi/writer

export types
export reader
export writer


when isMainModule:
    import os
    import strformat
    import strutils
    import sequtils
    import sugar

    let args=commandLineParams()
    var i=0
    var stack:seq[MidiFile]
    
    proc peekArg(): string = args[i]
    proc popArg(): string = (result=peekArg(); i.inc)
    
    proc help() =
        echo "all commands do something with the stack, a collection of midi files"
        echo ""
        echo "nim_midi COMMAND*"
        echo ""
        echo "COMMAND can be any of the following:"
        echo "read FILE1 ... FILEN -- reads midi files FILE1 to FILEN onto the stack"
        echo "write FILE -- writes the top of the stack to FILE"
        echo "concatall -- replaces all the midi files on the stack with one file, where track i is the contatenation of all track i of all the midi files"
        echo "delete INDEX -- removes the midi file at stack position INDEX"
        echo "pop -- removes the top of the stack"
        echo "clear -- clears the stack"
        echo "print -- prints all for each file and track the events"
        echo "info -- prints number of tracks and number of events per track for all files on the stack"
        echo "help -- print this help"
        
    const EOT=Event(delta_time:0, etype:EventType.MetaEvent, me_type:EndOfTrack)
    
    while i<args.len:
        let cmd=args[i].toLower.replace("-","").replace("_","")
        i.inc
        case cmd
        # Stack manipulation
        of "r","read": 
            while i<args.len and peekArg().fileExists:
                stack.add popArg().parseMidiFile
        of "concatall": # concatall -- merges all tracks of all entries on the stack into one file by concatenation
            var mf:MidiFile
            mf.header=stack[0].header
            for m in stack:
                for t in 0..<m.tracks.len:
                    while mf.tracks.len<=t:
                        mf.tracks.add MTrkChunk()
                    mf.tracks[t].events.add m.tracks[t].events.filter(e=>not(e.etype==EventType.MetaEvent and e.me_type==EndOfTrack))
            
            for i in 0..<mf.tracks.len:
                mf.tracks[i].events.add EOT
            
            stack.setLen 0
            stack.add mf
        of "del", "delete": # delete INDEX -- remove INDEX from the stack
            stack.delete popArg().parseInt
        of "pop": discard stack.pop
        of "clear": stack.setLen 0
            
        of "print":
            for i,m in stack:
                echo ""
                echo &"FILE {i}"
                for t_i,t in m.tracks:
                    echo &"  Track {t_i}/{m.tracks.len-1}"
                    for e in t.events:
                        echo "    ",e
        
        of "info":
            for i,m in stack:
                echo &"{i}\t{m.tracks.len} tracks"
                for t in 0..<m.tracks.len:
                    echo &"\t\t{t}: {m.tracks[t].events.len} events"
        
        of "write": # write FILENAME -- write the top of the stack to FIILENAME
            popArg().writeMidiFile(stack[0])
            
        of "help": 
            help()
        else:
            help()
            assert false, "Invalid command " & cmd

