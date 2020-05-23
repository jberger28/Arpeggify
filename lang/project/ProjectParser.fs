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
| PhraseVar of string * string

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

(* Parser for items separated by commas, allowing for whitespace *)
let commaSep p = pseq (pmany0 (pleft (pleft p pws0) (pleft (pchar ',') pws0))) (pmany1 (p)) (fun (a,b) -> List.append a b) 
                <!> "comma-sep"

(* Parser for items separated by commas and enclosed between parens, allowing for variable whitespace *)
let parensAndCommas p = pbetween (pleft (pchar '(') pws0) (pright pws0 (pchar ')')) (commaSep p) <!> "parens and commas"

(* Parser for items separated by commas, ending with END *)
let tuneCommaSep p = pleft (pmany1 (pleft (pleft p pws0) (pleft (pchar ',') pws0))) (pmany1 (pstr "END")) <!> "tune comma sep"

(* Parser for items separated by commas and enclosed between parens, allowing for variable whitespace *)
let tuneParensAndCommas p = pbetween (pleft (pchar '(') pws0) (pright pws0 (pchar ')')) (tuneCommaSep p) <!> "tune parens and commas"

(* A string of letters *)
let alpha = pmany1 pletter |>> stringify <!> "alpha"

(* Note parsers *)
let parseA = pchar 'A' |>> (fun _ -> A) <!> "A"
let parseB = pchar 'B' |>> (fun _ -> B) <!> "B"
let parseC = pchar 'C' |>> (fun _ -> C) <!> "C"
let parseD = pchar 'D' |>> (fun _ -> D) <!> "D"
let parseE = pchar 'E' |>> (fun _ -> E) <!> "E"
let parseF = pchar 'F' |>> (fun _ -> F) <!> "F"
let parseG = pchar 'G' |>> (fun _ -> G) <!> "G"
let note = parseA <|> parseB <|> parseC <|> parseD <|> parseE <|> parseF <|> parseG |>> (fun e -> Note e) <!> "note"

(* Accidental parsers, ie. sharp and flat *)
let sharp = pchar '#' |>> (fun _ -> Sharp) <!> "sharp"
let flat = pchar 'b' |>> (fun _ -> Flat) <!> "flat"
let symbol = sharp <|> flat <!> "symbol"
let accidental = (pseq note symbol (fun (a,b) -> (a,b))) |>> (fun e -> Accidental e) <!> "accidental"

(* Root parser, a note or note with accidental *)
let root = accidental <|> note <!> "root"

(* Chord extension parsers *)
let major7 = pstr "Ma7" |>> (fun _ -> Major7) <!> "major7"
let minor7 = pstr "-7" |>> (fun _ -> Minor7) <!> "minor7"
let dom7 = pchar '7' |>> (fun _ -> Dom7) <!> "dom7"
let extension = major7 <|> minor7 <|> dom7 <!> "extension"

(* Parser for one chord, a root followed by an extension *)
let chord: Parser<Chord> = pseq root extension (fun (a,b) -> (a,b)) <!> "chord"

(* Parser for paren-enclosed, comma-separated chords *)
let pchords = parensAndCommas chord |>> Chords <!> "pchords"
 
(* Parser for one rhythm, a positive number *)
let rhythm: Parser<Rhythm> = pmany1 pdigit |>> stringify |>> int <!> "rhythm"

(* Parser for multiple paren-enclosed, comma-separated rhythms *)
let prhythms = parensAndCommas rhythm |>> Rhythms <!> "prhythms"

(* Phrase from literal *)
let phraselit = pbetween (pleft (pchar '(') pws0) (pright pws0 (pchar ')')) (pseq (pleft (parensAndCommas chord) (pbetween pws0 pws0 (pchar ','))) (parensAndCommas rhythm) (fun (a,b) -> (a,b))) |>> PhraseLit <!> "phraselit"

(* Phrase from variables *)
let phrasevar = pbetween (pleft (pchar '(') pws0) (pright pws0 (pchar ')')) (pseq (pleft alpha (pright pws0 (pchar ',')))(pright pws0 alpha) (fun (a,b) -> (a,b))) |>> PhraseVar <!> "phrasevar"

(* A phrase, either from a literal or from rhythm and chord variables *)
let phrase: Parser<TuneBuilder> = phraselit <|> phrasevar |>> Phrase <!> "phrase"

(* Tune from phrase literals *)
let tunelit = tuneParensAndCommas phraselit <|> tuneParensAndCommas phrasevar |>> TuneLit <!> "tunelit"

(* Tune as combination of phrase variables *)
let tunevar: Parser<Tune> = tuneParensAndCommas alpha |>> TuneVar <!> "tunevar"

(* Tune from phrase literals or variables *)
let tune = tunelit <|> tunevar |>> Tune <!> "tune"

(* Tune builder, a tune, phrase, list of chords, or list of rhythms *)
let tunebuilder = tune <|> phrase <|> pchords <|> prhythms <!> "tunebuilder"

(* Allowed variable types *)
let tvar = pstr "Tune" |>> (fun _ -> TVar) <!> "tunevar"
let pvar = pstr "Phrase" |>> (fun _ -> PVar) <!> "pvar"
let cvar = pstr "Chords" |>> (fun _ -> CVar) <!> "chordsvar"
let rvar = pstr "Rhythms" |>> (fun _ -> RVar) <!> "rhythmsvar"
let typename = tvar <|> pvar <|> cvar <|> rvar <!> "typename"

(* Equals sign in between variable whitespace *)
let operator = pbetween pws0 pws0 (pchar '=') <!> "operator"

(* Left-hand side of assignment, ie. type name and variable name*)
let lhs = pseq (pleft typename pws0) (pleft alpha operator) (fun (a,b) -> (a,b)) <!> "lhs"
let assignment : Parser<Expr> = pseq lhs tunebuilder  (fun (a,b) -> (a,b)) |>> Assignment <!> "assignment"

(* Sequence operator, one or more expressions *)
let sequence =
    pmany1 ((pleft assignment (pmany1 pnl)) <|> assignment)
    |>> (fun es ->
            es
            |> List.rev
            |> List.reduce (fun acc e -> Seq(e, acc))
        )
    <!> "sequence"

(* If not a sequence, then an assignment *)
exprImpl := assignment <!> "expr"

(* Check for sequence first *)
let grammar = pleft sequence peof <!> "grammar"

(* Parse an arpeggify program *)
let parse input : Expr option = 
        let input' = prepare input
        match grammar input' with
        | Success(res,_)     -> Some res
        | Failure(pos, rule) ->
            printfn "Invalid expression."
            let msg = sprintf "Cannot parse input at pos %d in rule '%s':" pos rule
            let diag = diagnosticMessage 20 pos input msg
            printf "%s" diag
            None