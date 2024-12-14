// --- Day 11: Plutonian Pebbles ---
// Part 2

open System.IO
open System.Collections.Generic


/// Get data from the `..\data` directory and store it in a string
let getData (fileName: string) =
    File.ReadAllText $"""{__SOURCE_DIRECTORY__}\..\Data\{fileName}"""


/// Test data
let testData = getData "test11.txt"

/// Production data
let prodData = getData "data11.txt"


/// Parse example data
let parseData (input: string) =
    input.Split(' ')
    |> List.ofArray
    |> List.map uint64


let splitHalf (str: string) =
    str |> Seq.splitInto 2
    |> Seq.map (Seq.map string >> String.concat "")
    |> Seq.map uint64
    |> Seq.toList


// Rules:
// A. If the stone is engraved with the number 0, it is replaced by a stone engraved with the number 1.
// B. If the stone is engraved with a number that has an even number of digits, it is replaced by two stones. The left half of the digits are engraved on the new left stone, and the right half of the digits are engraved on the new right stone. (The new numbers don't keep extra leading zeroes: 1000 would become stones 10 and 0.)
// C. If none of the other rules apply, the stone is replaced by a new stone; the old stone's number multiplied by 2024 is engraved on the new stone.
let rules stone =
    if stone = 0UL then [1UL]   // A.
    else
        let str = stone |> string
        if str.Length % 2 = 0
        then
            str |> splitHalf    // B.
        else
            [stone * 2024UL]    // C.


/// Memoized blinking
let blinkM (dict: Dictionary<uint64, uint64>) =
    for kvp in dict |> Seq.toArray do
        let value = kvp.Value
        if value > 0UL then
            dict[kvp.Key] <- dict[kvp.Key] - value
            for item in rules kvp.Key do
                match dict.ContainsKey item with
                | true -> dict[item] <- dict[item] + value
                | false -> dict.Add(item, value)



let createDictionary stones =
    stones
    |> Seq.groupBy id
    |> Seq.map (fun (key, value) -> key, value |> Seq.length |> uint64)
    |> dict
    |> Dictionary


/// Blink num times
let part2 input num =
    let stones = parseData input
    let dict = createDictionary stones

    // Process dictionary
    for n = 1 to num do
        blinkM dict

    dict
    |> Seq.toArray
    |> Seq.sumBy (fun item -> item.Value)



//#time
//let output1 = part2 prodData  25 // 186424
//#time
// original:  Real: 00:00:00.329, CPU: 00:00:00.578, GC gen0: 5, gen1: 2, gen2: 1
// memoized:  Real: 00:00:00.004, CPU: 00:00:00.015, GC gen0: 1, gen1: 0, gen2: 0

//#time
//let output3 = part2 testData  75 // 65601038650482UL
//#time
// Real: 00:00:00.004, CPU: 00:00:00.015, GC gen0: 1, gen1: 0, gen2: 0

#time
let output4 = part2 prodData  75 // 186424
#time
//Real: 00:00:00.130, CPU: 00:00:00.140, GC gen0: 27, gen1: 1, gen2: 0
//val output4: uint64 = 219838428124832UL



// Output

let answers () =
    printfn $"Part 2: {output4}"


do answers ()

// Answer: 219838428124832
