module ProjectParser
open Parser

(* ARPEGGIFY GRAMMAR *)
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

type Rhythm = int

type TypeName =
| TVar
| PVar
| CVar
| RVar

type Chord = Root * Extension

type Phrase = 
| PhraseLit of Chord list * Rhythm list
| PhraseVar of string list

type Tune = 
| TuneLit of Phrase list
| TuneVar of string list

type TuneBuilder =
| Tune of Tune
| Phrase of Phrase
| Chords of Chord list
| Rhythms of Rhythm list

type Expr = 
| Assignment of (TypeName * string) * TuneBuilder
| Seq of Expr * Expr

(* END ARPEGGIFY GRAMMAR *)

let expr, exprImpl = recparser()

// Parser for items separated by commas, accounting for whitespace
let commaSep p = pseq (pmany0 (pleft (pleft p pws0) (pleft (pchar ',') pws0))) (pmany1 (p)) (fun (a,b) -> List.append a b) 
                <!> "comma-sep"

// Parser for items separated by commas and enclosed between parens
let parensAndCommas p = pbetween (pleft (pchar '(') pws0) (pright pws0 (pchar ')')) (commaSep p) <!> "parens and commas"

let alphanum = pmany1 (pletter <|> pdigit) |>> stringify <!> "alphanum"

// note parsers
let parseA = pchar 'A' |>> (fun _ -> A) <!> "A"
let parseB = pchar 'B' |>> (fun _ -> B) <!> "B"
let parseC = pchar 'C' |>> (fun _ -> C) <!> "C"
let parseD = pchar 'D' |>> (fun _ -> D) <!> "D"
let parseE = pchar 'E' |>> (fun _ -> E) <!> "E"
let parseF = pchar 'F' |>> (fun _ -> F) <!> "F"
let parseG = pchar 'G' |>> (fun _ -> G) <!> "G"

let note = parseA <|> parseB <|> parseC <|> parseD <|> parseE <|> parseF <|> parseG |>> (fun e -> Note e) <!> "note"

// accidental parsers ie. sharp and flat
let sharp = pchar '#' |>> (fun _ -> Sharp) <!> "sharp"
let flat = pchar 'b' |>> (fun _ -> Flat) <!> "flat"

let symbol = sharp <|> flat <!> "symbol"
let accidental = (pseq note symbol (fun (a,b) -> (a,b))) |>> (fun e -> Accidental e) <!> "accidental"

let root = accidental <|> note <!> "root"

// chord extension parsers
let major7 = pstr "Ma7" |>> (fun _ -> Major7) <!> "major7"
let minor7 = pstr "-7" |>> (fun _ -> Minor7) <!> "minor7"
let dom7 = pchar '7' |>> (fun _ -> Dom7) <!> "dom7"
let extension = major7 <|> minor7 <|> dom7 <!> "extension"

let chord: Parser<Chord> = pseq root extension (fun (a,b) -> (a,b)) <!> "chord"
let pchords = parensAndCommas chord |>> Chords <!> "pchords"

let rhythm: Parser<Rhythm> = pdigit |>> (fun e -> System.Char.GetNumericValue e |> int) <!> "rhythm"
let prhythms = parensAndCommas rhythm |>> Rhythms <!> "prhythms"


let phraselit = pseq (pleft (parensAndCommas chord) (pbetween pws0 pws0 (pchar ','))) (parensAndCommas rhythm) (fun (a,b) -> (a,b)) |>> PhraseLit <!> "phraselit"
let phrasevar = parensAndCommas alphanum |>> PhraseVar <!> "phrasevar"
let phrase: Parser<TuneBuilder> = phraselit <|> phrasevar |>> Phrase <!> "phrase"

let tunelit = parensAndCommas phraselit <|> parensAndCommas phrasevar |>> TuneLit <!> "tunelit"
let tunevar: Parser<Tune> = parensAndCommas alphanum |>> TuneVar <!> "tunevar"

let tune = tunelit <|> tunevar |>> Tune <!> "tune"


let tunebuilder = tune <|> phrase <|> pchords <|> prhythms <!> "tunebuilder"

let tvar = pstr "Tune" |>> (fun _ -> TVar) <!> "tunevar"
let pvar = pstr "Phrase" |>> (fun _ -> PVar) <!> "phrasevar"
let cvar = pstr "Chords" |>> (fun _ -> CVar) <!> "chordsvar"
let rvar = pstr "Rhythms" |>> (fun _ -> RVar) <!> "rhythmsvar"

let operator = pbetween pws0 pws0 (pchar '=') <!> "operator"
let typename = tvar <|> pvar <|> cvar <|> rvar <!> "typename"
let lhs = pseq (pleft typename pws0) (pleft alphanum operator) (fun (a,b) -> (a,b)) <!> "lhs"
let assignment = pseq lhs tunebuilder  (fun (a,b) -> (a,b)) |>> Assignment <!> "assignment"

let sequence =
    pmany1 ((pleft expr pnl) <|> expr)
    |>> (fun es ->
            es
            |> List.rev
            |> List.reduce (fun acc e -> Seq(e, acc))
        )
    <!> "sequence"

exprImpl := assignment <!> "expr"

let grammar = pleft sequence peof

// Parse an arpeggify program
let parse input : Expr option = 
        let input' = debug input
        //let input' = prepare input
        match grammar input' with
        | Success(res,_)     -> Some res
        | Failure(pos, rule) ->
            printfn "Invalid expression."
            let msg = sprintf "Cannot parse input at pos %d in rule '%s':" pos rule
            let diag = diagnosticMessage 20 pos input msg
            printf "%s" diag
            None