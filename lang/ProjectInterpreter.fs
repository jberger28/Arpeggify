module ProjectInterpreter
open ProjectParser
open AudioGen

    
(*
let rec eval e noteSeq = 
    match box e with
    | :? (Phrase list * Ending) -> failwith "hi"
    | :? Ending -> failwith "hi"
    | :? Phrase -> failwith "hi"
    | :? Chord -> failwith "hi"
    | :? Extension -> failwith "hi"
    | :? Symbol -> evalSymbol e
    | :? Note -> evalNote e
    | _ -> failwith "error"
    *)

// calculate new note
let evalSymbol s = 
    match s with 
    | Sharp -> 1
    | Flat -> -1

// return numeric representation of note
let evalNote n = 
    match n with
    | C -> 0
    | D -> 2
    | E -> 4
    | F -> 5
    | G -> 7
    | A -> 9
    | B -> 11   

// return number
let rec evalRoot r = 
    match r with 
    | Note n -> evalNote n
    | Accidental (n,s) -> evalRoot n + evalSymbol s

// eval extension - return list of notes
let evalExt e = 
    match e with
    | Major7 -> [4; 7; 11]
    | Minor7 -> [3; 7; 10]
    | Dom7 -> [4; 7; 10]

let rec evalChord cs = 
    match cs with
    | head :: tail ->
        let (r,e) = head
        let arp = (evalRoot r) :: []
        let root = evalRoot r
        List.append (root :: (List.map (fun x -> x + root)(evalExt e))) (evalChord tail)
    | _ -> []

// return int * int list
let rec evalPhrase ps = 
    match ps with
    | head :: tail ->
       let (c,r) = head
       //let cs = evalChord c
       //let rs = [4; 4; 4; 4]
       List.append(List.zip (evalChord c) [4; 4; 4; 4;4; 4; 4; 4;4; 4; 4; 4;4; 4; 4; 4]) (evalPhrase tail)                
    | _ -> []

// ENDING

let eval tune = 
    match tune with 
    | Some t -> 
        let (ps, ending) = t
        let output = evalPhrase ps
        let (notes, _) = List.unzip output
        let noteSeq = []
        List.fold (fun acc elem -> addPitch elem acc) [] notes
    | None -> failwith "Error"

    (*
let eval (t:Tune) = 
    let (pl, ending) = t
    let phrases = evalPhrase pl
    let fini = eval ending
    failwith "not done"
    *)