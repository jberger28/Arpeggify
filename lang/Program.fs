open ProjectParser
open ProjectInterpreter
open System.IO

// Simple Example Program
// (E-7,A7,D-7,G7)(4,4,4,4)||

(*
chords c = (E-7, A7, D-7, G7) 
rhythms r = (4, 4, 4, 4)
phrase p = (c,r)
tune t = p
*)

(* ARPEGGIFY DRIVER *)
[<EntryPoint>]
let main argv =
    if argv.Length <> 1 then
        printfn "Usage: dotnet run \"<A Valid Tune>\" <output.wav>\n\nArpeggify will arpeggiate the specified chord progression\n\nExample Usages:\ndotnet run \"(E-7,A7,D-7,G7)(4,4,4,4)||\" foo.wav\ndotnet run \"(G-7,AbMa7,G-7,AbMa7)(3,4,3,4)||\" foo.wav"
        exit 1

    let file = argv.[0]
    let input = File.ReadAllText file
    let parsed = parse input
    printfn "Parsed: %A" parsed
    match parsed with
    | Some t ->
        let result = eval t Map.empty
        if result = 0 then
            printfn "Program ran successfully!"
    | None -> failwith "Unable to Parse"
    0

    (*
    match parse input with
    | Some t -> printfn "Parsed successfully!"
                eval t Map.empty
    | None -> exit 1
    0
    *)

    (* old 
    match (parse argv.[0]) with
    | Some t -> eval t argv.[1]
                printfn "Generated file: \"%s\"!" argv.[1] // print success message
                0
    | None -> printfn "Invalid input"
              exit 1
             *)