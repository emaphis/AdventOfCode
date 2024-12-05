// Day 04 Part 2 - Advent of Code - Ceres Search

open System.IO

/// Get data from the `..\data` directory and store it in a list of string
let getData (fileName: string) =
    File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\..\Data\{fileName}"""
    |> List.ofSeq


let test04 = getData "test04.txt"
let data04 = getData "data04.txt"


/// A 2D board
type Board = list<list<char>>

/// Parse XMAS data to a 2D list of list of char: `list<list<char>>`
let parseData (input: string list) : Board =
    input
    |> List.map List.ofSeq

//let testBoard = parseData test04
//let xmasBoard = parseData data04


// Part 2

let numRows (board: Board) = Seq.length board - 1
let numCols (board: Board) = Seq.length board[0] - 1

/// Calculate relavant (in bounds) locations given a Board and a coordinate.
let elevantLocations (row, col) =
    let directions = [
        [(-1,-1);(1,1)]
        [(1,-1);(-1,1)]
    ]
    directions
    |> List.map (List.map (fun (rw, cl) -> row + rw, col + cl))


let lookup (board: Board) (row, col) = board[row][col]

let allALocations (board: Board)   =
    let nRows = numRows board;
    let nCols = numCols board;
    [ for row in 1 .. (nRows - 1) do
        for col in 1 .. (nCols - 1)  -> (row, col) ]
    |> List.filter (fun loc -> lookup board loc = 'A')

let lookups board cross =
    cross |> List.map (List.map (lookup board))
 
let isCross cross =
    [
        [['M';'S'];['M';'S']]
        [['M';'S'];['S';'M']]
        [['S';'M'];['M';'S']]
        [['S';'M'];['S';'M']]
    ]
    |> List.contains cross

    
/// Sum a list of of strings representing integers
let part2 data =
    let board = parseData data
    allALocations board
    |> List.map elevantLocations
    |> List.map (lookups board)
    |> List.filter isCross
    |> List.length


let outTest = part2 test04
let outXMAS = part2 data04  // 2003


// Output

let answers () =
    printfn $"Part 1: {part2 data04}"

do answers ()
