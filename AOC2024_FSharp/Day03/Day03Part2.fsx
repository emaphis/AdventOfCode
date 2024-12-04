// Day 03 Part 2 - Advent of Code - Mull It Over

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


// Part 1

/// Toke DU for a simple three instruction language
type Token =
    | Mul of int * int

/// Parse data returning tokes.
let parseTokens (input: string) =
    let regex = @"(mul)\((?:(\d+),(\d+))?\)"
    let rgx = Regex(regex)
    let data = rgx.Matches(input)

    data
    |> Seq.map (fun mtch ->
        match mtch.Groups[1].Value with
        | "mul" ->
            (int mtch.Groups[2].Value, int mtch.Groups[3].Value) |> Mul
        | _ -> failwith "Bad token")



//let testTokens = parseTokens test03
//let dataTokens = parseTokens  data03

//printfn "%A" testTokens

let processTokens (tokens: Token seq) =
    tokens
    |>Seq.map (fun tkns ->
    match tkns with
    | Mul (x, y) -> x * y
//    | _ -> 0
    )
    |> Seq.sum

//processTokens testTokens  // 161


let part2 input =
    let data = parseTokens input
    processTokens data

let part01Test = part2 test03  // 161
let part01Ans = part2  data03  // 174561379

// Answer: 174561379


// Tests and examples
let tests () =
    printf "Testing.."
    test <@ part2 test03 = 161 @>
    test <@ part2 data03 = 174561379 @>
    printfn "...done!"

do tests ()


// Output

let answers () =
    printfn $"Part 2: {part2 data03}"

do answers ()
