module CS334
open FSound.Signal
open FSound.IO

// return a pitch generator
let gen pitch = modulate (triangle 15000.0 pitch) (adsr 0.05 1.0 0.05 0.3 0.1 0.05)

// one octave of white keys
let (c, d, e, f, g, a, b) = (gen 261.63, gen 293.66, gen 329.63, gen 349.23, gen 392.00, gen 440.00, gen 493.88)

// prepend a note-timestamp tuple to our list of notes
let addNote note noteSeq = 
    match noteSeq with
    | head :: tail -> let (time, _) = head
                      (time + 0.25, note) :: noteSeq
    | _            -> (0.0, note) :: noteSeq


let arranged noteSeq = [arrange (List.rev noteSeq)]

let writeFile output fileName = 
    (List.map(generate 44100.0 60.0)) output |> streamToWav 44100 2 fileName