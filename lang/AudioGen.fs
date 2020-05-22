module AudioGen
open FSound.Signal
open FSound.IO

(* ARPEGGIFY CROSS-PLATFORM LIBRARY FOR GENERATING .WAV FILES USING FSound Library *)

// return a pitch generator
let gen pitch = modulate (triangle 15000.0 pitch) (adsr 0.05 1.0 0.05 0.3 0.1 0.05)

// create generators for two octaves
let generators =
   [| for i in 0 .. 23 -> gen (2.0 ** (float i/12.0) * 261.64) |]

// Add new frequency generator to a certain time of our track
let addPitch pitch noteSeq = 
    match noteSeq with
    | head :: tail -> let (time, _) = head
                      (time + 0.25, generators.[pitch]) :: noteSeq
    | _            -> (0.0, generators.[pitch]) :: noteSeq


// prepare note list to be written to wav
let arranged noteSeq = [arrange (List.rev noteSeq)]

// Writes output to wav file
let writeFile output length fileName = 
    (List.map(generate 44100.0 length)) output |> streamToWav 44100 2 fileName

// Entry point to this library, creates wav file of a specified list of notes
let generateAudio output fileName = 
    let notes = (List.fold (fun acc elem -> addPitch elem acc) [] output)
    let (times,_) = List.unzip notes
    let length = List.max times
    writeFile (arranged notes) (length + 1.0) fileName