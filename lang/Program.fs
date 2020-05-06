open System
open AudioGen
open ProjectParser
open ProjectInterpreter

[<EntryPoint>]
let main argv =
    //printf "%A" (parse argv.[0])
    //writeFile (arranged(addNote b (addNote f (addNote d (addNote e (addNote c [])))))) "test.wav"
    //printfn "Success"

    writeFile (arranged(addPitch 14 (addPitch 12 (addPitch 5 (addPitch 4 (addPitch 2 (addPitch 0 []))))))) "boop.wav"
    printfn "yeet"
    0