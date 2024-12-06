// Day 00 - Advent of Code - Example 00

#r "nuget: Unquote"

open System.IO
open Swensen.Unquote

/// Get data from the `..\data` directory and store it in a list of string
let getData (fileName: string) =
    File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\..\Data\{fileName}"""
    |> List.ofSeq

/// Test data
let testData = getData "test00.txt"

/// Production data
let prodData = getData "data00.txt"


/// Parse example data
let parseExample (input: string list) =
    input   // .Split('\n')
    |> List.map (fun str -> str.Trim())
    |> List.map int
    |> List.ofSeq


let tData = parseExample testData
let pData = parseExample prodData


// Part 1

/// Sum a list of of strings representing integers
let part1 input =
    let data = parseExample input
    data
    |> List.sum


part1 testData
part1 prodData

// Answer: 510


// Part 2

/// Sum a list of integers multiplied by 10
let part2 input =
    let data = parseExample input
    data
    |> List.map (fun n -> n * 10)
    |> List.sum


part2 testData
part2 prodData

// Answer: 5100

// Tests and examples

let tests () =
    printf "Testing.."
    test <@ parseExample testData = [10; 20; 30; 40; 50] @>
    test <@ parseExample prodData =  [100; 101; 102; 103; 104] @>
    test <@ part1 testData = 150 @>
    test <@ part1 prodData = 510 @>
    test <@ part2 testData = 1500 @>
    test <@ part2 prodData = 5100 @>
    printfn "...done!"

do tests ()


// Output

let answers () =
    printfn $"Part 1: {part1 prodData}"
    printfn $"Part 2: {part2 prodData}"

do answers ()
