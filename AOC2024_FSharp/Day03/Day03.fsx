// Day 03 - Advent of Code - Mull It Over

#r "nuget: Unquote"

open System.IO
open Swensen.Unquote
open System.Text.RegularExpressions

/// Get data from the `..\data` directory and store it in a list of string
/// Data file should have only one item.
let getData (fileName: string) =
    File.ReadAllText $"""{__SOURCE_DIRECTORY__}\..\Data\{fileName}"""


let test03 = getData "test03.txt"
let data03 = getData "data03.txt"


/// Parse data using the passed regex.
/// Retrieve only the numbers in pairs
let parseExample (regex: string) (input: string) =
   // let rgx = Regex("mul\((\d+),(\d+)\)")
    let rgx = Regex(regex)
    let data = rgx.Matches(input)
    data
    |> Seq.map (fun mch -> (int mch.Groups[1].Value, int mch.Groups[2].Value))


// Part 1

/// Matches "mul(9,9)" patters, should exclude bad examples
let regex01 = @"mul\((\d{1,3}),(\d{1,3})\)"

let testNumbers01 = parseExample regex01 test03
let dataNumbers01 = parseExample regex01 data03

//printfn "%A" testNumbers01

let multiplySeq numbers =
    numbers
    |> Seq.map (fun (x,y) -> x * y)
    |> Seq.sum

multiplySeq testNumbers01  // 161

/// Sum a list of of strings representing integers
let part1 regex input =
    let data = parseExample regex input
    multiplySeq data

let part01Test = part1 regex01 test03  // 161
let part01Ans = part1 regex01 data03  // 174561379


// Part 2

/// Sum a list of integers multiplied by 10
let part2 input =
    let data = parseExample input
    data
    |> List.map (fun n -> n * 10)
    |> List.sum


part2 test03
part2 data03


// Tests and examples

let tests () =
    printf "Testing.."
    test <@ parseExample test03 = [10; 20; 30; 40; 50] @>
    test <@ parseExample d
parseExample test03
parseExample data03ata03 =  [100; 101; 102; 103; 104] @>
    test <@ part1 test03 = 150 @>
    test <@ part1 data03 = 510 @>
    test <@ part2 test03 = 1500 @>
    test <@ part2 data03 = 5100 @>
    printfn "...done!"

do tests ()


// Output

let answers () =
    printfn $"Part 1: {part1 data03}"
    printfn $"Part 2: {part2 data03}"

do answers ()
