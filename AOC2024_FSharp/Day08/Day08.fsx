//--- Day 8: Resonant Collinearity ---

open System.IO


// data definitions

type Board = char array array
type Frequency = char
type Position = int * int
type Antenna = { Frequency: Frequency; X: int; Y: int }
type AntennasByFrequency = Frequency * Antenna list


// get data

/// Get data from the `..\data` directory and store it in a list of string
let getData (fileName: string) : Board =
    File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\..\Data\{fileName}"""
    |> Array.map Seq.toArray

// Test data
//let testData = getData "test08.txt"

// Production data
//let prodData = getData "data08.txt"




// function definitions

let isOnMap (board: Board) (position: Position) =
    let x, y = position
    0 <= y && y < board.Length && 0 <= x && x < board[y].Length


let getAntennas (board: Board) =
    [0..board.Length-1]
    |> List.collect (fun y ->
                                [0..board[y].Length-1]
                                |> List.map (fun x ->
                                    { Frequency = board[y][x]; X = x; Y = y }))
                                    |> List.where (fun ant -> ant.Frequency <> '.')


let gatherAntennasByFreqency (antennas: Antenna list) : AntennasByFrequency list =
    antennas
    |>List.groupBy _.Frequency



////////////////////////////////////////////////////////
// Part 1


let determineAntiNodes1 (antennas: Antenna list) =
    let determineAntiNodeOfPair (ant1: Antenna) (ant2: Antenna) =
        if ant1 = ant2 then []
        else
            let dx = ant2.X - ant1.X
            let dy = ant2.Y - ant1.Y
            [ (ant1.X - dx, ant1.Y - dy); (ant2.X + dx, ant2.Y + dy) ]
    let pairs =  List.allPairs antennas antennas
    pairs
    |> List.collect (fun (an1, an2) -> determineAntiNodeOfPair an1 an2)


let getAllAntiNodes1 (frequencies: AntennasByFrequency list) =
    frequencies
    |> List.collect (fun g ->
                                determineAntiNodes1 (snd g))
                                |> Seq.distinct

let part1 fileName =
    let board = getData fileName
    let antennas = getAntennas board
    let frequencies = gatherAntennasByFreqency antennas 
    let allAntiNodes = getAllAntiNodes1 frequencies
    let antiNodesOnMap = allAntiNodes |> Seq.where (isOnMap board)
    antiNodesOnMap |> Seq.length


//let testOut1 = part1 "test08.txt"
//let prodOut1 = part1 "data08.txt"
// test data: 14
// prod data: 394



//////////////////////////////////////////////////////////////
// part 2

let rec generateAntiNodes (board: Board) (x0: int) (y0: int) (dx: int) (dy: int) (n:int) acc =
    let pos = (x0 + n * dx, y0 + n * dy)
    if (isOnMap board pos) then generateAntiNodes board x0 y0 dx dy (n + 1) (pos :: acc)
    else acc

let determineAntiNodes2 (board: Board) (antennas: Antenna list) =
    let determineAntiNodeOfPair (ant1: Antenna) (ant2: Antenna) =
        if ant1 = ant2 then []
        else
            let dx = ant2.X - ant1.X
            let dy = ant2.Y - ant1.Y
            generateAntiNodes board ant1.X ant1.Y +dx +dy 1 [] @ generateAntiNodes board ant2.X ant2.Y -dx -dy 1 []
    let pairs = List.allPairs antennas antennas
    pairs
    |> List.collect (fun (an1, an2) -> determineAntiNodeOfPair an1 an2)

let getAllAntiNodes2 board frequencies =
    frequencies
    |> Seq.collect (fun g ->
                                determineAntiNodes2 board (snd g))
                                |> Seq.distinct

let part2 fileName =
    let board = getData fileName
    let antennas = getAntennas board
    let frequencies = gatherAntennasByFreqency antennas 
    let allAntiNodes = getAllAntiNodes2 board frequencies
    let antiNodesOnMap = allAntiNodes |> Seq.where (isOnMap board)
    antiNodesOnMap |> Seq.length



//let testOut1 = part2 "test08.txt"
//let prodOut1 = part2 "data08.txt"
// test data: 34
// prod data: 1277


// Output


let answers () =
    printfn $"Part 1 test: {part1 "test08.txt"}"
    printfn $"Part 1 prod: {part1 prodData}"
    printfn $"Part 2 test: {part2 testData}"
    printfn $"Part 2 prod: {part2 prodData}"

do answers ()
