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


let testD = parseData test02
let dataD = parseData data02


// Part 1

let isSafe (levels: seq<int>) =
    let findPairs (levels : seq<int>) = Seq.zip levels (Seq.tail levels)
    
    let within (lower:int, upper: int) pairs =
        pairs
        |> Seq.forall (fun pair -> 
                        let diff = abs (fst pair - snd pair)
                        lower <= diff && diff <= upper)

    let pairs = findPairs levels
    let allIncr = pairs |> Seq.forall (fun (x, y) -> x < y) 
    let allDecr = pairs |> Seq.forall (fun (x, y) -> x > y) 

    (allIncr || allDecr) && within (1, 3) pairs

let report = Seq.head testD
isSafe report.Levels
printfn "%A" report


/// Sum a list of of strings representing integers
let part1 data  =
    data
    |> Seq.map (fun rpt -> isSafe(rpt.Levels))
    |> Seq.map (fun bl -> if bl then 1 else 0)
    |> Seq.sum

//printfn "%A" (part1 testD)
//part1 dataD


// Part 2

/// Sum a list of integers multiplied by 10
let part2 input  =
    let data = parseData input
    data
    |> Seq.map (fun rpt -> isSafe(rpt.Levels))
    |> Seq.map (fun bl -> if bl then 1 else 0)
    //|> Seq.sum


part2 test02
part2 data02


// Tests and examples

let tests () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
   // test <@ parseData test02 = [10; 20; 30; 40; 50] @>
    test <@ part1 dataD = 2 @>
    //test <@ part2 testD = 1500 @>
    //test <@ part2 dataD = 5100 @>
    printfn "...done!"

do tests ()

parseData test02
// Output

let answers () =
    printfn $"Part 1: {part1 dataD}"
    //printfn $"Part 2: {part2 dataD}"

do answers ()
