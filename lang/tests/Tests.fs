namespace tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open Parser
open ProjectParser
open ProjectInterpreter

[<TestClass>]
type TestClass () =

    (* Test that chords assignment parse correctly *)
    [<TestMethod>]
    member this.ChordsAssignmentParses () = 
        let chords  = 
            (Note C, Dom7) :: (Accidental (Note C, Sharp), Major7) :: (Note D, Minor7) ::
                (Accidental (Note E, Flat), Dom7)::(Accidental (Note G, Flat), Minor7) :: []
                |> Chords

        let expected = ((CVar, "chords"), chords) |> Assignment

        let input =  prepare "Chords chords  = (  C7, C#Ma7, D-7,  Eb7, Gb-7    )"
        match sequence input with
        | Success (p,_) -> Assert.AreEqual(expected, p)
        | Failure _ -> Assert.IsTrue false


    (* Test that rhythms assignment parses correctly *)
    [<TestMethod>]
    member this.RhythmsAssignmentParses () = 
        let rhythms = [1;2;3;4;5;6;7;8;9;10;100] |> Rhythms

        let expected = ((RVar, "rhythms"), rhythms) |> Assignment

        let input = prepare "Rhythms rhythms  = (  1,  2, 3 , 4 , 5, 6 ,   7, 8, 9, 10,100 )"
        match sequence input with
        | Success (p,_) -> Assert.AreEqual(expected, p)
        | Failure _ -> Assert.IsTrue false

    (* Test that phrase assignment parses correctly *)
    [<TestMethod>]
    member this.PhraseAssignmentParses () = 
        let chords = 
            (Note C, Dom7) :: (Accidental (Note C, Sharp), Major7) :: (Note D, Minor7) ::
                (Accidental (Note E, Flat), Dom7)::(Accidental (Note G, Flat), Minor7) :: []

        let rhythms = [1;2;3;4;5]

        let phrase = PhraseLit (chords, rhythms) |> Phrase

        let expected = ((PVar, "phrase"), phrase) |> Assignment

        let input = prepare "Phrase phrase = ((C7, C#Ma7, D-7,  Eb7, Gb-7), (1,2,3,4,5))"
        match sequence input with
        | Success (p,_) -> Assert.AreEqual(expected, p)
        | Failure _ -> Assert.IsTrue false

    (* Test that tune assignment parses correctly *)
    [<TestMethod>]
    member this.TuneAssignmentParses () = 
        let chords1 = 
            (Note C, Dom7) :: (Accidental (Note C, Sharp), Major7) :: (Note D, Minor7) :: []

        let rhythms1 = [6;7;8]
        let phrase1 = PhraseLit (chords1, rhythms1)
        let chords2 = (Note C, Dom7) :: (Accidental (Note C, Sharp), Major7) :: (Note D, Minor7) :: [] |> List.rev
        let rhythms2 = [1;2;3]
        let phrase2 = PhraseLit(chords2, rhythms2)

        let tune = TuneLit [phrase1; phrase2] |> Tune
        let expected = ((TVar, "tune"), tune) |> Assignment

        let input = prepare "Tune tune = (((C7, C#Ma7, D-7), (6,7,8)), ((D-7, C#Ma7, C7), (1,2,3)), END)"
        match sequence input with
        | Success (p,_) -> Assert.AreEqual(expected, p)
        | Failure _ -> Assert.IsTrue false

    (* Test for interpreter, ensure assingment works correctly *)
    [<TestMethod>]
    member this.InterpretsCorrectly () =

        // Expected output, a tune
        let p1 = (([Note D, Minor7 ; Note G, Dom7]), ([3;4])) |> PhraseLit
        let p2 = (([Note C, Major7]), [8]) |> PhraseLit
        let phraseList = [p1;p2]
        let expected = phraseList |> TuneLit |> Tune

        // Input, what would normally be sent on from the parser
        let line1 : Expr = ((CVar, "cOne"), [Note D, Minor7 ; Note G, Dom7] |> Chords) |> Assignment        
        let line2: Expr = ((CVar, "cTwo"), [Note C, Major7] |> Chords) |> Assignment

        let line3: Expr = ((RVar, "rOne"), [3;4] |> Rhythms) |> Assignment
        let line4: Expr = ((RVar, "rTwo"), [8] |> Rhythms) |> Assignment

        let line5: Expr = ((PVar, "pOne"), (("cOne", "rOne") |> PhraseVar |> Phrase)) |> Assignment
        let line6: Expr = ((PVar, "pTwo"), (("cTwo", "rTwo") |> PhraseVar |> Phrase)) |> Assignment

        let line7: Expr = ((TVar, "t"), ((["pOne" ; "pTwo"] |> TuneVar) |> Tune))|> Assignment

        let inputSequence = Seq (line1, Seq (line2, Seq (line3, Seq (line4, Seq (line5, Seq (line6, line7))))))
        
        let (tune, _) = evalExpr inputSequence Map.empty
        Assert.AreEqual(expected, tune)
    