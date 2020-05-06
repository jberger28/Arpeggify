module ProjectParser
open Parser

// Simple Example:
// (E-7,A7,D-7,G7)(4,4,4,4)||

type Note = 
| A 
| B 
| C 
| D
| E 
| F 
| G

type Symbol = 
| Sharp
| Flat

type Root = 
| Note of Note
| Accidental of Root * Symbol

type Extension = 
| Major7
| Minor7
| Dom7

type Chord = Root * Extension

type Rhythm = int

type Phrase = Chord list * Rhythm list

type Ending = 
    | End 
    | Repeat of int

type Tune = Phrase list * Ending


let commaSep p = pseq (pmany0 (pleft p (pchar ','))) (pmany1 (p)) (fun (a,b) -> List.append a b)
let parensAndCommas p = pbetween (pchar '(') (pchar ')') (commaSep p)

// note parsers
let parseA = pchar 'A' |>> (fun _ -> A)
let parseB = pchar 'B' |>> (fun _ -> B)
let parseC = pchar 'C' |>> (fun _ -> C)
let parseD = pchar 'D' |>> (fun _ -> D)
let parseE = pchar 'E' |>> (fun _ -> E)
let parseF = pchar 'F' |>> (fun _ -> F)
let parseG = pchar 'G' |>> (fun _ -> G)
let note = parseA <|> parseB <|> parseC <|> parseD <|> parseE <|> parseF <|> parseG |>> (fun e -> Note e)

// accidental parsers
let sharp = pchar '#' |>> (fun _ -> Sharp)
let flat = pchar 'b' |>> (fun _ -> Flat)

let symbol = sharp <|> flat
let accidental = (pseq note symbol (fun (a,b) -> (a,b))) |>> (fun e -> Accidental e)

let root = accidental <|> note

// chord extension parsers
let major7 = pstr "Ma7" |>> (fun _ -> Major7)
let minor7 = pstr "-7" |>> (fun _ -> Minor7)
let dom7 = pchar '7' |>> (fun _ -> Dom7)
let extension = major7 <|> minor7 <|> dom7

let chord: Parser<Chord> = pseq root extension (fun (a,b) -> (a,b))
//let rhythm: Parser<Rhythm> = psat (fun c -> int c >= 0 && int c <= 8) |>> (fun e -> int e)
let rhythm: Parser<Rhythm> = pdigit |>> (fun e -> System.Char.GetNumericValue e |> int) // FIX THIS
let phrase: Parser<Phrase> = pseq (parensAndCommas chord) (parensAndCommas rhythm) (fun (a,b) -> (a,b))
let finish = pstr "||" |>> (fun _ -> End)
let repeat = pleft pdigit (pstr ":|") |>> (fun e -> Repeat (System.Char.GetNumericValue e |> int))
let ending = finish <|> repeat
let tune: Parser<Tune> = pseq (pmany1 phrase) ending (fun (a,b) -> (a,b))

let grammar = pleft tune peof

let parse input : Tune option = 
        //let input' = debug input
        let input' = prepare input
        match grammar input' with
        | Success(res,_)     -> Some res
        | Failure(pos, rule) ->
            printfn "Invalid expression."
            let msg = sprintf "Cannot parse input at pos %d in rule '%s':" pos rule
            let diag = diagnosticMessage 20 pos input msg
            printf "%s" diag
            None