// --- Day 10: Hoof It ---

open System.IO

type Board = int array array
type Pos = { X : int; Y : int }

let makePos (x, y) = { X = x; Y = y}

let toInt (chr: char) = int (chr - '0')

/// Get data from the `..\data` directory and create a board
let getData (fileName: string) : Board =
    File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\..\Data\{fileName}"""
    |> Array.map (fun str -> str.ToCharArray() |> Array.map toInt)

/// Check if positon is inbounds of the board
let inBounds (board: Board) (pos: Pos) =
    let rowLim = board.Length - 1
    let colLim = board[0].Length - 1
    0 <= pos.X && pos.X <= rowLim && 0 <= pos.Y && pos.Y <= colLim

/// Get all the moves given a boad and a position
let rec getMoves (board: Board) (pos: Pos) =
    if board[pos.X][pos.Y] = 9 then
        [ makePos(pos.X, pos.Y) ]
    else
        [ makePos(-1, 0); makePos(1, 0); makePos(0, -1); makePos(0, 1) ]
        |> List.map (fun pos1 -> makePos(pos.X + pos1.X, pos.Y + pos1.Y))
        |> List.filter (fun pos2 -> inBounds board pos2 && board[pos.X][pos.Y] + 1 = board[pos2.X][pos2.Y])
        |> List.collect (fun pos3 -> (getMoves board pos3))


let produceIndexes (board: Board) =
    let rowLim = board.Length
    let colim = board[0].Length
    seq [ for x in 0 .. rowLim - 1 do
            for y in 0 .. colim - 1 do
                if board[x][y] = 0 then
                    makePos(x, y) ]


let part1 board =
    (produceIndexes board)
    |> Seq.sumBy (getMoves board >> Set.ofList >> Set.count)

let part2 board =
    (produceIndexes board)
    |> Seq.sumBy (getMoves board >> List.length)


let testBoard = getData "test10.txt"
let prodBoard = getData "data10.txt"


let test1 = part1 testBoard  // 36
let prod1 = part1 prodBoard  // 798

let test2 = part2 testBoard  // 81
let prod2 = part2 prodBoard  // 798


printfn $"Part 1 Test data: {test1}"
printfn $"Part 1 prod data: {prod1}"


printfn $"Part 2 Test data: {test2}"
printfn $"Part 2 prod data: {prod2}"
