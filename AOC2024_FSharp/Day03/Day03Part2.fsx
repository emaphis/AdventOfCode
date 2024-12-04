// Day 03 Part 2 - Advent of Code - Mull It Over

#r "nuget: Unquote"

open System.IO
open System.Text.RegularExpressions
open Swensen.Unquote

/// Get data from the `..\data` directory and store it in a list of string
/// Data file should have only one item.
let getData (fileName: string) =
    File.ReadAllText $"""{__SOURCE_DIRECTORY__}\..\Data\{fileName}"""

let test03 = getData "test03b.txt"
let data03 = getData "data03.txt"


// Part 2

/// Toke DU for a simple three instruction language
type Token =
    | Do
    | Dont
    | Mul of int * int

/// Parse data returning tokes.
let parseTokens (input: string) =
    let regex = @"(mul|do|don't)\((?:(\d+),(\d+))?\)"
    let rgx = Regex(regex)
    let data = rgx.Matches(input)

    data
    |> Seq.map (fun mtch ->
        match mtch.Groups[1].Value with
        | "mul" ->
            (int mtch.Groups[2].Value, int mtch.Groups[3].Value) |> Mul
        | "do" -> Do
        | "don't" -> Dont
        | _ -> failwith "Bad token")



//let testTokens = parseTokens test03
//let dataTokens = parseTokens  data03

//printfn "%A" testTokens

let processTokens (tokens: Token seq) =
    tokens
    |>Seq.fold (fun (sum, domult) tkns ->
    match tkns with
    | Do  -> (sum, true)
    | Dont -> (sum, false)
    | Mul (x, y) ->
        let newSum =
            match domult with
            | true -> x * y + sum
            | false -> sum
        (newSum, domult)
    ) (0, true)
    |> fst

//processTokens testTokens  // 48


let part2 input =
    let data = parseTokens input
    processTokens data

let part01Test = part2 test03  // 48
let part01Ans = part2  data03  // 106921067

// Answer: 106921067


// Tests and examples
let tests () =
    printf "Testing.."
    test <@ part2 test03 = 48 @>
    test <@ part2 data03 = 106921067 @>
    printfn "...done!"

do tests ()


// Output

let answers () =
    printfn $"Part 2: {part2 data03}"

do answers ()
