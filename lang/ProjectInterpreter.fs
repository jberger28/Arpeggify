module ProjectInterpreter
open ProjectParser
open AudioGen

type Env = Map<string, TuneBuilder>

type EvalResult = 
| Ran of t: TuneBuilder
| Fail of message: string

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

   //FIX THIS
// Return an (int[] * Rhythm) list, matching each chord's notes with that chord's length
let rec evalPhrase (ps: Phrase list) = 
    match ps with
    | head :: tail ->
        match head with
        | PhraseLit (cs, rs) -> 
            let notes = evalChord cs
            List.append (List.zip notes rs) (evalPhrase tail) 
        | _ -> failwith "Error: phrase list cannot be evaluated correctly"              
    | _ -> []
    
   
let rec evalPhraseAssign (p: Phrase) = 
    match p with
    | PhraseLit (a,b) -> failwith "hi"
    | _ -> failwith "hew"
   
let rec evalAssignment typeName varName (tb: TuneBuilder) env = 
    match typeName with
    | TVar ->
        let t = tb
        match t with
        | Tune t -> 
            match t with
            | TuneLit ps -> 
                tb, Map.add varName tb env
            | TuneVar vs -> 
                let retrieve p : Phrase = 
                    match Map.tryFind p env with
                    | Some phrase -> 
                        match phrase with
                        | Phrase phrase -> phrase
                        | _ -> failwith "Error: variable not of type Tune"
                    | None -> failwith "Error: variable is not defined"
                let tune = vs |> List.map retrieve |> TuneLit |> Tune
                tune, Map.add varName tune env
        | _ -> failwith "Type mismatch: Attempting to assign something other than a tune to a Tune varaible"
    | PVar ->
        let p = tb
        match p with
        | Phrase ph ->
            match ph with
            | PhraseLit(cs, rs) -> 
                if List.length cs = List.length rs then
                    tb, Map.add varName tb env
                else failwith "Error: Attempting to add different length chords and rhythms to a phrase"
            | PhraseVar (c, r) ->
                match Map.tryFind c env with
                | Some chords ->
                    match chords with
                    | Chords chords -> 
                        match Map.tryFind r env with
                        | Some rhythms ->
                            match rhythms with
                            | Rhythms rhythms -> 
                                if List.length chords = List.length rhythms then 
                                    let p = (chords,rhythms) |> PhraseLit |> Phrase
                                    p, Map.add varName p env
                                else failwith "Error: Attempting to assign a different length chords and rhythms to a phrase"
                            | _ -> failwith "Error: variabe not of type Rhythms"
                        | None -> failwith "Error: variable not yet defined"
                    | _ -> failwith "Error: variable not of type Chords"
                | None -> failwith "Error: variable not yet defined"
        | _ -> failwith "Type mismatch: attempting to assign something other than a phrase to variable"
    | CVar -> 
        let chords = tb
        match chords with
        | Chords chords -> tb, Map.add varName tb env
        | _ -> failwith "Error: atttempting to assign something other than chords to a Chords variable"
    | RVar ->
        let r = tb
        match r with
        | Rhythms r -> tb, Map.add varName tb env
        | _ -> failwith "Error: attempting to assign something other than rhythms to a Rhythms variable"


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


let rec evalExpr (e: Expr) (env: Map<string, TuneBuilder>) = 
    match e with
    | Assignment ((typeName,varName), tb) -> evalAssignment typeName varName tb env
    | Seq (e1, e2) ->
        let _, env' = evalExpr e1 env
        evalExpr e2 env'              


let eval e env = 
   let (tune, _) = evalExpr e env
   match tune with
   | Tune tune -> 
       match tune with
       | TuneLit tune ->
            generateAudio (arpeggiate (evalPhrase tune)) "output.wav"
            0
        | _ -> failwith "should never happen"
   | _ -> failwith "Error: Last line of program must be a tune assignment"

   
(* old
// evaluate tune
let eval tune filename = 
    let (ps,ending) = tune
    generateAudio (arpeggiate (evalPhrase ps)) filename
    // eval ending too
    *)