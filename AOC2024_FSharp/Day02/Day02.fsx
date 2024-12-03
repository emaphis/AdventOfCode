// Day 00 - Advent of Code - Red-Nosed Reports

#r "nuget: Unquote"

open System.IO
open Swensen.Unquote

/// Get data from the `..\data` directory and store it in a list of strings
let getData (fileName: string) =
    File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\..\Data\{fileName}"""
    |> List.ofSeq


let test02 = getData "test02.txt"
let data02 = getData "data02.txt"


type Report = {
    Levels : seq<int>
}

/// Parse a string of ints into a Report.
let parseLine(line: string): Report =
    let levels =
        line.Split(' ')
        |> Array.map int
        |> Array.toSeq
    { Levels = levels }


/// parsing function
let parseData (input: string list) =
    input
    |> List.map (fun line -> parseLine line)
    |> List.toSeq


//let testD = parseData test02
//let dataD = parseData data02


// Part 1

let isSafe (levels: seq<int>) =
    let within (lower:int, upper: int) pairs =
        pairs
        |> Seq.forall (fun pair -> 
                        let diff = abs (fst pair - snd pair)
                        lower <= diff && diff <= upper)

    let pairs = Seq.pairwise levels
    let allIncr = pairs |> Seq.forall (fun (x, y) -> x < y) 
    let allDecr = pairs |> Seq.forall (fun (x, y) -> x > y) 

    (allIncr || allDecr) && within (1, 3) pairs


/// Count all the safe test reports
let part1 data  =
    parseData data
    |> Seq.filter (fun report -> isSafe report.Levels)
    |> Seq.length

//printfn "%A" (part1 test02)
//part1 data02


// Part 2

/// Generate a seq of dampened levels
let dampener levels =
    seq {
        yield levels
        let length = (levels |> Seq.length) - 1
        for i in [0..length] -> levels |> Seq.removeAt i
    }

/// Check of report levels are safe after dampening.
let isSafeDampen levels =
    let dLevels = dampener levels
    dLevels |> Seq.exists isSafe

/// Count all the safe test reports after dampening
let part2 data  =
    parseData data
    |> Seq.filter (fun report -> isSafeDampen report.Levels)
    |> Seq.length


//part2 test02
//part2 data02


// Tests and examples

let tests () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
   // test <@ parseData test02 = [10; 20; 30; 40; 50] @>
    test <@ part1 data02 = 287 @>
    test <@ part2 test02 = 4 @>
    test <@ part2 data02 = 354 @>
    printfn "...done!"

do tests ()


// Output

let answers () =
    printfn $"Part 1: {part1 data02}"
    printfn $"Part 2: {part2 data02}"

do answers ()
