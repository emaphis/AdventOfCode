// Day 5 - Advent of Code - Print Queue

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

let checkRulesFor (rules: Rule list) (tail: Updates) (update: Update) =
    let rulesToCheck = tail |> Seq.map (fun upd -> (upd,update))
    rulesToCheck
    |> Seq.exists (fun rule -> rules |> Seq.contains rule)
    |> not

let rec checkRules02 (rules: Rule list) (updates: Updates) =
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
            checkRules02 rules tail

/// Find the middle update
let findMiddle (updates: Updates) =
    let length = updates |> List.length
    let center = length / 2
    updates[center]

let correct (rules: Rule list) (update: Updates) =
    let rec correct (rules : (int*int) list) updatesAcc update =
        match update with
        | [] -> updatesAcc |> List.rev
        | _ ->
            let nextUpdate =
                seq {
                    for update1 in  update do
                        let rest = update |> List.except [update1]
                        if checkRulesFor rules rest update1 then yield update1
                } |> Seq.head
            let rest = update |> List.except [nextUpdate]
            correct rules (nextUpdate :: updatesAcc) rest
    correct rules [] update


/// Process book changes
let part2 inputData =
   let rules, updates = parseData inputData

   let inValidUpdates =
        updates
        |> List.filter (fun update -> not (checkRules02 rules update))

   inValidUpdates
   |> List.map (correct rules)
   |> List.map findMiddle
   |> List.sum


//part2 testString   // 123
//part2 prodString   // 4145

// Answer: 4145


let tests () =
    printf "Testing.."
    test <@ part2 testString = 123 @>
    test <@ part2 prodString = 4145 @>
    printfn "...done!"

do tests ()


// Output

let answers () =
    printfn $"Test 1: {part2 testString}"
    printfn $"Part 1: {part2 prodString}"

do answers ()
