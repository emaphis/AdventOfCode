// Day 00 - Advent of Code - Example 00

#r "nuget: Unquote"

open System.IO
open Swensen.Unquote

/// Get data from the `..\data` directory and store it in a list of string
let getData (fileName: string) =
    File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\..\Data\{fileName}"""
    |> List.ofSeq


let data00 = getData "data00.txt"
let test00 = getData "test00.txt"


/// Example parsing function
let parseExample (input: string list) =
    input   // .Split('\n')
    |> List.map (fun str -> str.Trim())
    |> List.map int
    |> List.ofSeq


parseExample test00
parseExample data00


// Part 1

/// Sum a list of of strings representing integers
let part1 input =
    let data = parseExample input
    data
    |> List.sum


part1 test00
part1 data00


// Part 2

/// Sum a list of integers multiplied by 10
let part2 input =
    let data = parseExample input
    data
    |> List.map (fun n -> n * 10)
    |> List.sum


part2 test00
part2 data00


// Tests and examples

let tests () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
    test <@ parseExample test00 = [10; 20; 30; 40; 50] @>
    test <@ parseExample data00 =  [100; 101; 102; 103; 104] @>
    test <@ part1 test00 = 150 @>
    test <@ part1 data00 = 510 @>
    test <@ part2 test00 = 1500 @>
    test <@ part2 data00 = 5100 @>
    printfn "...done!"

do tests ()


// Output

let answers () =
    printfn $"Part 1: {part1 data00}"
    printfn $"Part 2: {part2 data00}"

do answers ()
