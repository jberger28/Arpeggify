open ProjectParser
open ProjectInterpreter
// Simple Example Program
// (E-7,A7,D-7,G7)(4,4,4,4)||

(* ARPEGGIFY DRIVER *)
[<EntryPoint>]
let main argv =
    if argv.Length <> 2 then
        printfn "Usage: dotnet run \"<A Valid Tune>\" <output.wav>\n\nArpeggify will arpeggiate the specified chord progression\n\nExample Usages:\ndotnet run \"(E-7,A7,D-7,G7)(4,4,4,4)||\" foo.wav\ndotnet run \"(G-7,AbMa7,G-7,AbMa7)(3,4,3,4)||\" foo.wav"
        exit 1

    match (parse argv.[0]) with
    | Some t -> eval t argv.[1]
                printfn "Generated file: \"%s\"!" argv.[1] // print success message
                0
    | None -> printfn "Invalid input"
              exit 1
             