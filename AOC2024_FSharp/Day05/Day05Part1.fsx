// Day 00 - Advent of Code - Example 00

#r "nuget: Unquote"

open System.IO
open Swensen.Unquote

/// Get data from the `..\data` directory and store it in a string
let getData (fileName: string) =
    File.ReadAllText $"""{__SOURCE_DIRECTORY__}\..\Data\{fileName}"""


let testString = getData "test05.txt"
let prodString = getData "data05.txt"


type Update = int
type Rule = Update * Update
type Updates = Update list

// Parsing

/// Parsing rules and updates data file
let parseData (input: string) =

    /// Parse each rule
    let parseRuleLine (line: string) : Rule =
        let split = line.Split('|')
        let first = split[0] |> int
        let second = split[1] |> int
        (first, second)

    /// Parse each update list
    let parseUpdateLine (line: string) : Updates =
        line.Split(',')
        |> Seq.map int
        |> Seq.toList

    let splitData = input.Split("\r\n\r\n")
    let ruleLines = splitData[0]
    let updateLines = splitData[1]

    let rules =
        ruleLines.Split("\n")
        |> Seq.map parseRuleLine
        |> List.ofSeq

    let updates =
        updateLines.Split("\n")
        |> Seq.map parseUpdateLine
        |> Seq.toList

    (rules, updates)


//let testData = parseData testString
//let prodData = parse prodString
//printfn "%A" (fst testData)
//printfn "%A" (snd testData)


// Processing data

let rec checkRules01 (rules: Rule list) (updates: Updates) =
    match updates with
    | [] -> true
    | [ _ ] -> true
    | update :: tail ->

        let rulesToCheck : Rule list = tail |> List.map (fun upd -> (upd, update))

        let notFound =
            rulesToCheck
            |> Seq.exists (fun rule -> rules |> Seq.contains rule)

        if notFound then false
        else
            checkRules01 rules tail

/// Find the middle update
let findMiddle (updates: Updates) =
    let length = updates |> List.length
    let center = length / 2
    updates[center]


/// Process book changes
let part1 inputData =
   let rules, updates = parseData inputData
   let validUpdates = updates |> List.filter (checkRules01 rules)
   validUpdates
   |> List.map findMiddle
   |> List.sum


//part1 testString   // 143
//part1 prodString   // 6949

// Answer: 6949


let tests () =
    printf "Testing.."
    test <@ part1 testString = 143 @>
    test <@ part1 prodString = 6949 @>
    printfn "...done!"

do tests ()


// Output

let answers () =
    printfn $"Test 1: {part1 testString}"
    printfn $"Part 1: {part1 prodString}"

do answers ()
