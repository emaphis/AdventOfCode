// --- Day 14: Restroom Redoubt ---

open System.IO
open System.Text.RegularExpressions


/// Get data from the `..\data` directory and store it in a list of string
let getData (fileName: string) =
    File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\..\Data\{fileName}"""
    |> Array.ofSeq


/// Test data
let testData = getData "test14.txt"
let testLim = 7, 11


/// Production data
let prodData = getData "data14.txt"
let prodLim = 103, 101



//****************************************************


let parseLine (patten: string) (line: string) =
    let matches = Regex.Matches(line, patten)
    matches
    |> Seq.map _.Groups
    |> Seq.map (fun g -> g |> Seq.skip 1 |> Seq.map (fun g -> int g.Value) |> Seq.toArray)
    

let parseData (lines: string array) =     
    lines |> Array.map (parseLine @"(-?\d+)" >> Array.concat)
    

let move (pi, pj) (vi, vj) limit sec =
    let R, C = limit
    let i = (pi + vi * sec) % R |> fun i -> if i < 0 then R + i else i
    let j = (pj + vj * sec) % C |> fun i -> if i < 0 then C + i else i
    (i, j)    

let getRobots (data: int array array) =
    data
    |> Array.map (fun nums ->
            let p = (nums[1], nums[0])
            let v = (nums[3], nums[2])
            p, v)
    
    
    
let part1(lines: string array, limits: int * int) =
    let data = parseData lines
    let robots =  getRobots data 

    let getArea (i, j) limit =
        let R, C = limit
        if i < R / 2 && j < C / 2 then 1
        elif i < R / 2 && j > C / 2 then 2
        elif i > R / 2 && j < C / 2 then 3
        elif i > R / 2 && j > C / 2 then 4
        else 0

    robots
    |> Array.map (fun (p, v) -> move p v limits 100)
    |> Array.groupBy (getArea limits)
    |> Array.filter (fun (n, _) -> n <> 0)
    |> Array.fold (fun s (_, nums) -> s * int64 nums.Length) 1L

    

let part2(lines: string array,  limits: int * int) =
    
    let data = parseData lines
    let robots =  getRobots data
    
    let rec nextMove secs =
        let nextPos = robots |> Array.map (fun (p, v) -> move p v limits secs)

        if Set.ofArray(nextPos).Count = robots.Length then
            secs
        else
            nextMove (secs + 1)

    nextMove 0


 
let test1 = part1(testData, testLim)
// 12
let prod1 = part1(prodData, prodLim)
// 231221760

let test2= part2(testData, testLim)
// 1
let prod2= part2(prodData, prodLim)
// 6771


