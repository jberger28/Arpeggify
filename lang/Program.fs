open ProjectParser
open ProjectInterpreter
open System.IO

(* ARPEGGIFY DRIVER *)
[<EntryPoint>]
let main argv =
    printfn "## ARPEGGIFY INTERPRETER ##"

    if argv.Length <> 2 && argv.Length <> 3 then
        usage
        exit 1

    // Set beats per minute equal to command-line arg or default value
    let bpm = 
        if argv.Length = 3 then
            match System.Int32.TryParse argv.[2] with
            | true,i when i > 0 -> i
            | _ -> 
                printfn "\nProgram failed: BPM must be a positive integer"  
                usage
                exit 1                         
        else 200 // default value

    // verify user entered a .wav file as output
    let output = argv.[1]
    if output.[(output.Length - 4)..(output.Length - 1)] <> ".wav" then 
        fail "Output file must be a \".wav\" file"

    // attept to read arpeggifh program from a file
    let file = argv.[0]

    let input =
        try
            File.ReadAllText file
        with
        | _ -> 
           sprintf "Error opening file \"%s\"" argv.[0] |> fail

    // Parse and evaluate program
    match parse input with
    | Some t ->
        let result = eval t Map.empty output bpm
        if result = 0 then
            printfn "\nSuccess!"
            printfn "Output written to \"%s\"" output
            printfn "\n## END ARPEGGIFY INTERPRETER ##"
            0
        else 
            fail "Unable to execute"
    | None -> 
        fail "Unable to parse, exiting interpreter"