open ProjectParser
open ProjectInterpreter
open System.IO

(* ARPEGGIFY DRIVER *)
[<EntryPoint>]
let main argv =
    printfn "-- ARPEGGIFY INTERPRETER --"
    if argv.Length <> 2 then
        // provide usage info
        printfn "Usage: dotnet run <program.arp> <output.wav>\n"
        printfn "arpeggify arpeggiates the tune provided in the last line of a program, writing the musical output to a wav file\n"
        printfn "To run the example program \'blues.arp\' type:\ndotnet run blues.arp blues.wav"
        printfn "\n## END ARPEGGIFY INTERPRETER ##"
        exit 1

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
        let result = eval t Map.empty output
        if result = 0 then
            printfn "\nSuccess!"
            printfn "Output written to \"%s\"" output
            printfn "\n-- END ARPEGGIFY INTERPRETER --"
            0
        else 
            fail "Unable to execute"
    | None -> 
        fail "Unable to parse, exiting interpreter"