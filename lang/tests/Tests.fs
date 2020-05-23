namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open ProjectParser
open ProjectInterpreter
open Parser

[<TestClass>]
type TestClass () =

    (* Test that chords parse correctly *)
    [<TestMethod>]
    member this.ChordsAssignmentParses () = 
        let chords  = 
            (Note C, Dom7) :: (Accidental (Note C, Sharp), Major7) :: (Note D, Minor7)::
                (Accidental (Note E, Flat), Dom7)::(Accidental (Note G, Flat), Minor7) :: []
                |> Chords

        let expected = ((CVar, "chords"), chords) |> Assignment

        let input =  prepare "Chords chords  = (  C7, C#Ma7, D-7,  Eb7, Gb-7    )"
        match sequence input with
        | Success (p,_) -> Assert.AreEqual(expected, p)
        | Failure _ -> Assert.IsTrue false


    [<TestMethod>]
    member this.RhythmsAssignmentParses () = 
        let rhythms = [1;2;3;4;5;6;7;8;9;10;100] |> Rhythms

        let expected = ((RVar, "rhythms"), rhythms) |> Assignment

        let input =  debug "Rhythms rhythms  = (  1,  2, 3 , 4 , 5, 6 ,   7, 8, 9, 10,100 )"
        match sequence input with
        | Success (p,_) -> Assert.AreEqual(expected, p)
        | Failure _ -> Assert.IsTrue false