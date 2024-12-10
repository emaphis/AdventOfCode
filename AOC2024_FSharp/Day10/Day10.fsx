// --- Day 10: Hoof It ---


open System.IO


type Board = int array array
type Pos = int * int

let toInt (chr: char) = int (chr - '0')

/// Get data from the `..\data` directory and store it in a list of string
let getData (fileName: string) : Board =
    File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\..\Data\{fileName}"""
    |> Array.map (fun str -> str.ToCharArray() |> Array.map toInt))

/// Check if positon is inbounds of the board
let inBounds (board: Board) (pos: Pos) =
    let x, y = pos
    let rowLim = board.Length
    let colLim = board[0].Length
    0 <= x && x < rowLim && 0 <= y && y < colLim

/// Get all the moves given a boad and a position
let rec getMoves (board: Board) (pos: Pos) =
    let x, y = pos
    if board[x][y] = 9 then
        [ (x, y) ]
    else
        [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
        |> List.map (fun (x1, y1) -> (x + x1, y + y1))
        |> List.filter (fun (x2, y2) -> inBounds board (x2, y2) && board[x][y] + 1 = board[x2][y2])
        |> List.collect (fun pos -> (getMoves board pos))


let indexes (board: Board) =
    let rowLim = board.Length
    let colim = board[0].Length
    seq [ for x in 0 .. rowLim - 1 do
            for y in 0 .. colim - 1 do
                if board[x][y] = 0 then
                    (x, y) ]


let part1 board =
    (indexes board)
    |> Seq.sumBy (getMoves board >> Set.ofList >> Set.count)

let part2 board =
    (indexes board)
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
