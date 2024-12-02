// Day 01 - Advent of Code - Historian Hysteria

#r "nuget: Unquote"

open System.IO
open Swensen.Unquote

/// Get data from the `..\data` directory and store it in a list of string
let getData (fileName: string) =
    File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\..\Data\{fileName}"""
    |> List.ofSeq


let test01 = getData "dataTest01.txt"
let data01 = getData "data01.txt"


/// Parsing locations and returning the data in two lists.
let parseLocations (input: string list)  =
    let arrays =
        input
        |> List.map _.Split("   ")
        |> Array.transpose
    let first = arrays[0] |> Array.map int |> Array.toList
    let second = arrays[1] |> Array.map int |> Array.toList
    (first, second)


//parseLocations test01
//parseLocations data01


// Part 1

/// Calulate the distances between locations in two location lists
let part1 input =
    let (first, second) = parseLocations input
    List.zip (first |> List.sort) (second |> List.sort)
    |> List.map (fun (x,y) -> abs (x - y))
    |> List.sum


//part1 test01
//part1 data01


// Part 2

/// Calulate the distances between locations in two location lists
let part2 input =
    let (first, second) = parseLocations input
    [ for elem in first -> (elem, second |> List.filter (fun x -> x = elem) |> List.length)]
    |> List.map (fun (x,y) -> x*y)
    |> List.sum


//part2 test01
//part2 data01


// Tests and examples

let tests () =
    printf "Testing.."
    test <@ List.transpose [[3; 4]; [4; 3]; [2; 5]; [1; 3]; [3; 9]; [3; 3]]
        = [[3; 4; 2; 1; 3; 3]; [4; 3; 5; 3; 9; 3]] @>
    test <@ List.zip [1; 2; 3; 3; 3; 4] [3; 3; 3; 4; 5; 9]
        =  [(1, 3); (2, 3); (3, 3); (3, 4); (3, 5); (4, 9)] @>
    
    test <@ parseLocations test01 = ([3; 4; 2; 1; 3; 3], [4; 3; 5; 3; 9; 3]) @>
    test <@ part1 test01 = 11 @>
    test <@ part1 data01 = 1666427 @>
    test <@ part2 test01 = 31 @>
    test <@ part2 data01 = 24316233 @>
    printfn "...done!"

do tests ()


// Output

let answers () =
    printfn $"Part 1: {part1 data01}"
    printfn $"Part 2: {part2 data01}"

do answers ()
