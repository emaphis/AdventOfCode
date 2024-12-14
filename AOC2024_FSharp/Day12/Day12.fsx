// Day 00 - Advent of Code - Example 00

#r "nuget: Unquote"

open System.IO
open Swensen.Unquote

type Garden = char array array
type Pos = int * int

let getX pos = fst pos
let getY pos = snd pos

//let toInt (chr: char) = int (chr - '0')

/// Get data from the `..\data` directory read data lines
let getData (fileName: string)  =
    File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\..\Data\{fileName}"""
    //|> Array.map (fun str -> str.ToCharArray())


let testData = getData "test12.txt"
let prodData = getData "data12.txt"



let rowLimit (garden: Garden) = garden.Length - 1
let colLimit (garden: Garden) = [0].Length - 1

/// Check if positon is inbounds of the board
let inBounds (garden: Garden) (pos: Pos) =
    let rowL = rowLimit garden
    let colL = colLimit garden
    let x, y = pos
    0 <= x && x <= rowL && 0 <= y && y <= colL

/// Get all the moves given a garden and a position
let rec getMoves (garden: Garden) (pos: Pos) =
    let x, y = pos
    if garden[x][y] = 9 then
        (x, y)
    else
        [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
        |> List.map (fun pos1 -> (x + getX pos1, y + getY pos1))
        |> List.filter (fun pos2 -> inBounds garden pos2 && garden[x][y] + 1 = garden[getX pos2][getY pos2])
        |> List.collect (fun pos3 -> (getMoves garden pos3))


let produceIndexes (garden: Garden) =
    let rowLim = garden.Length
    let colim = garden[0].Length
    seq [ for x in 0 .. rowLim - 1 do
            for y in 0 .. colim - 1 do
                if garden[x][y] = 0 then (x, y) ]


let part1 garden =
    (produceIndexes garden)
    |> Seq.sumBy (getMoves garden >> Set.ofList >> Set.count)
    //|> List.sum


part1 testData
part1 prodData

// Answer: yyy



let part2 garden =
    (produceIndexes garden)
    |> Seq.sumBy (getMoves garden >> Set.ofList >> Set.count)
    //|> List.sum


part2 testData
part2 prodData




// Output

let answers () =
    printfn $"Part 1: {part1 prodData}"
    printfn $"Part 2: {part2 prodData}"

do answers ()
