// --- Day 11: Plutonian Pebbles ---


open System.IO

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


let tData = parseData testData

//let pData = parseData prodData



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


/// Apply the rules to a list of stones
let blink stones =
    stones |> List.collect rules
    

/// Blink `num` of times
let rec blinking num acc =
    match num with
    | 0 ->  acc
    | _ -> blinking (num - 1) (blink acc)


/// Blink 25 times
let part1 input =
    let stones = parseData input
    let list = blinking 25 stones
    List.length list
        

part1 testData  // 55312
part1 prodData  // 186424

// Answer: 55312


// Part 2

/// Blink 75 times
let part2 input =
    let stones = parseData input
    let list = blinking 75 stones
    List.length list


//part2 testData
//part2 prodData

// Answer: 5100

// Tests and examples


// Output

let answers () =
    printfn $"Part 1: {part1 prodData}"
    //printfn $"Part 2: {part2 prodData}"

do answers ()
