// Day 04 Part 1 - Advent of Code - Ceres Search

open System.IO

/// Get data from the `..\data` directory and store it in a list of string
let getData (fileName: string) =
    File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\..\Data\{fileName}"""
    |> List.ofSeq


let test04 = getData "test04.txt"
let data04 = getData "data04.txt"

/// A 2D baord
type Board = list<list<char>>
/// A location
type Location = int * int

/// Parse XMAS data to a 2D list of list of char: `list<list<char>>`
let parseData (input: string list) : Board =
    input
    |> List.map List.ofSeq

let testBoard = parseData test04
let xmasBoard = parseData data04


// Part 1

let numRows (board: Board) = Seq.length board - 1
let numCols (board: Board) = Seq.length board[0] - 1

/// Calculate relavant (in bounds) locations given a Board and a coordinate.
let relevantLocations (board: Board) row col =

    let isInBounds (row,col) =
        row >= 0 && col >= 0 && row <= numRows board && col <= numCols board

    let directions = [
            // Rows and Columns.
            [(0,0);(0,1);(0,2);(0,3)]       // Right
            [(0,0);(0,-1);(0,-2);(0,-3)]    // Left
            [(0,0);(1,0);(2,0);(3,0)]       // Up
            [(0,0);(-1,0);(-2,0);(-3,0)]    // Down

            // Diagonals.
            [(0,0);(1,1);(2,2);(3,3)]       // Up Right
            [(0,0);(-1,1);(-2,2);(-3,3)]    // Down Right
            [(0,0);(1,-1);(2,-2);(3,-3)]    // Up Left
            [(0,0);(-1,-1);(-2,-2);(-3,-3)] // Down Left
        ]

    // All locations even out of bounds
    let allLocations =
        directions
        |> List.map (List.map (fun (row', col') -> row + row', col + col'))

    // filter relavant (in bounds) locations
    allLocations
        |> List.filter (fun location -> location |> List.forall isInBounds)


//relevantLocations(testData, 0, 4)

let allLocations numRows numCols =
    [ for row in 0 .. numRows do
        for col in 0 .. numCols  -> (row, col) ]

let lookup (board: Board) (row, col) = board[row][col]

/// Sum a list of of strings representing integers
let part1 data =
    let board = parseData data
    allLocations (numRows board) (numCols board)
    |> List.collect (fun (x, y) -> relevantLocations board x y)
    |> List.map (fun locs -> locs |> List.map (lookup board))
    |> List.filter (fun word -> word = ['X';'M';'A';'S'])
    |> List.length

//let outTest =part1 test04
//let outXMAS = part1 data04  // 2549


// Output

let answers () =
    printfn $"Part 1: {part1 data04}"

do answers ()
