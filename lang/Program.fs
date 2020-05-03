open System
open AudioGen
open ProjectParser
open ProjectInterpreter

[<EntryPoint>]
let main argv =
    writeFile (arranged(addNote b (addNote f (addNote d (addNote e (addNote c [])))))) "test.wav"
    printfn "Success"
    0