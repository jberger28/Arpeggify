module ProjectInterpreter
open ProjectParser
open AudioGen

// Calculate amount to modify note by
let evalSymbol s = 
    match s with 
    | Sharp -> 1
    | Flat -> -1

// return numeric representation of note, notes represented by their distance in half steps from C = 261.64 HZ
let evalNote n = 
    match n with
    | C -> 0
    | D -> 2
    | E -> 4
    | F -> 5
    | G -> 7
    | A -> 9
    | B -> 11   

// return number representing root
let rec evalRoot r = 
    match r with 
    | Note n -> evalNote n
    | Accidental (n,s) -> evalRoot n + evalSymbol s

// eval extension - return array of notes within a given chord
let evalExt e = 
    match e with
    | Major7 -> [4; 7; 11]
    | Minor7 -> [3; 7; 10]
    | Dom7 -> [4; 7; 10]

// Return list of note arrays, each one corresponding to the notes within a specified chord
let rec evalChord cs = 
    match cs with
    | head :: tail ->
        let (r,e) = head
        let root = evalRoot r
        let notesInChord = List.toArray(root :: (List.map (fun x -> x + root)(evalExt e)))
        notesInChord :: evalChord tail
    | _ -> []

// Return an (int[] * Rhythm) list, matching each chord's notes with that chord's length
let rec evalPhrase (ps: Phrase list) = 
    match ps with
    | head :: tail ->
       let (cs,rs) = head
       let notes = evalChord cs
       List.append (List.zip notes rs) (evalPhrase tail)               
    | _ -> []

// TODO: ENDING EVALUATOR

// Choose appropriate notes to create arpeggios, AKA, where the magic happens
let rec arpeggiate (evaled: (int [] * Rhythm) list) = 
    match evaled with 
    | head :: tail -> 
        match head with
            | (notes, number) -> 
                let list = [0 .. number-1] // list represents each note played to satisfy rhythm
                List.append (List.map(fun x -> notes.[x % 4]) list) (arpeggiate tail)
    | _ -> []

// evaluate tune
let eval tune filename = 
    let (ps,ending) = tune
    generateAudio (arpeggiate (evalPhrase ps)) filename
    // eval ending too