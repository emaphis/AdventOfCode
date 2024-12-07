// Day 00 - Advent of Code - Example 00

// Use int64 for calculations to avoid overflows.


open System.IO

/// Get data from the `..\data` directory and store it in a list of string
let getData (fileName: string) =
    File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\..\Data\{fileName}"""
    |> List.ofSeq

/// Test data
let testData = getData "test07.txt"

/// Production data
let prodData = getData "data07.txt"


/// Parse each line into result and a list of int64 operands
let parseLine (line : string) =
    let temp = line.Split(": ")
    let result = temp[0] |> int64
    let operands= temp[1].Split(" ") |> Seq.map int64 |> Seq.toList
    (result, operands)

/// Parse example data
let parseExample (input: string list) =
    input 
    |> List.map parseLine

//let tData = parseExample testData
//let pData = parseExample prodData

type Operator = int64 -> int64 -> int64

let solve (operators: Operator list) (test, values) =
    let rec apply sum vals =
        match vals with
        | _ :: _ when sum > test -> None
        | i :: tail -> operators |> List.tryPick (fun opr -> apply (opr sum i ) tail)
        | [] -> if sum = test then Some test else None
    
    apply (List.head values) (List.tail values)


// Part 1

let solve1 = solve [(+); (*)]

/// 
let part1 input =
    parseExample input
    |> List.choose solve1
    |> List.sum

//part1 testData // 3749L
//part1 prodData // 20665830408335L

// Answer: 20665830408335L


// Part 2

/// Conatination operator
let (++) (l: int64) (r: int64) =
    sprintf "%d%d" l r |> int64

let solve2 = solve [(+); (*); (++)]

/// Sum a list of integers multiplied by 10 
let part2 input =
    parseExample input
    |> List.choose solve2
    |> List.sum


//part2 testData // 11387L
//part2 prodData // 354060705047464L

// Answer: 5100


// Output

#time
let test1 = part1 testData // 3749L
let prod1 = part1 prodData // 20665830408335L
let test2 = part2 testData // 11387L
let prod2 = part2 prodData // 354060705047464L
#time

let answers () =
    printfn $"Part 1 test: {test1}"
    printfn $"Part 1 prod: {prod1}"
    printfn $"Part 2 test: {test2}"
    printfn $"Part 2 rpod: {prod2}"

do answers ()
