// --- Day 13: Claw Contraption ---

open System
open System.IO


/// Get data from the `..\data` directory and store it in a list of string
let getData (fileName: string) =
    File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\..\Data\{fileName}"""


/// Test data
let testData = getData "test13.txt"

/// Production data
let prodData = getData "data13.txt"


///////////////////////////


type Pos = int64 * int64
type Machine = Pos * Pos * Pos


// Parse the data

let parseBlock token (part: string) : Pos =
    let blk = part.Split([| token; ',' |])
    ( Int64.Parse(blk[1]), Int64.Parse(blk[3]) )

let parseMachine (machine: string array) : Machine =
    let buttonA = parseBlock '+' machine[0]
    let buttonB = parseBlock '+' machine[1]
    let prize = parseBlock '=' machine[2]
    (buttonA, buttonB, prize)

/// Parse input data into an array of machines
let parseData (input: string array) =
    input
    |> Array.chunkBySize 4
    |> Array.map parseMachine


let calculateMachine (machine: Machine) =
    let a, b, prize = machine
    let ax, ay = a
    let bx, by = b
    let prx, pry = prize

    let denominator = ax * by - ay * bx
    let xx = (prx * by - pry * bx) / denominator
    let yy = (pry * ax - prx * ay) / denominator

    if xx * ax + yy * bx = prx && xx * ay + yy * by = pry then
        Some (xx * 3L + yy)
    else
        None


// Part 1

let part1 (input: string array) =
    let machines = parseData input
    machines
    |> Array.choose calculateMachine
    |> Array.sum


let test1 = part1 testData
let prod1 = part1 prodData

printfn $"Test Part 1 = {test1}"
printfn $"Prod Part 1 = {prod1}"

// test 480
// Total cost: 29877


// Part 2

let updateMachine (a, b, prize) : Machine =
    let prizeX, prizeY = prize
    (a, b, ( prizeX + 10000000000000L, prizeY + 10000000000000L ))


let part2 input =
    let machines = parseData input
    machines
    |> Array.map updateMachine
    |> Array.choose calculateMachine
    |> Array.sum


let test2 = part2 testData
let prod2 = part2 prodData

printfn $"Test Part 2 = {test2}"
printfn $"Prod Part 2 = {prod2}"

// 875318608908
// 99423413811305
