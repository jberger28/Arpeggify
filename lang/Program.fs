open System
open FSound.Signal
open FSound.IO
open CS334

[<EntryPoint>]
let main argv =
    writeFile (arranged(addNote b (addNote f (addNote d (addNote e (addNote c [])))))) "test.wav"
    printfn "Success"
    0