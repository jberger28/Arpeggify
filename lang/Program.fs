open System
open AudioGen
open ProjectParser
open ProjectInterpreter

[<EntryPoint>]
let main argv =
    let parsed = parse argv.[0]
    //printf "%A" (parse argv.[0])
    printf "%A" parsed

    //writeFile (arranged(addNote b (addNote f (addNote d (addNote e (addNote c [])))))) "test.wav"
    //printfn "Success"

    //writeFile (arranged(addPitch 4 (addPitch 5 (addPitch 7 (addPitch 9 (addPitch 11 (addPitch 7 (addPitch 4 (addPitch 0 []))))))))) "boop.wav"
    0