

open System.IO

// Get data from the `..\data` directory and store it in a list of string
let getData (fileName: string) =
    File.ReadAllText $"""{__SOURCE_DIRECTORY__}\..\Data\{fileName}"""


/// Test data
let testData = getData "test09.txt"
// 2333133121414131402

/// Production data
let prodData = getData "data09.txt"

// 2333133121414131402
// 23 33 13 31 21 41 41 31 40 2
// 00... 111... 3... 444. 5555. 6666. 777. 8888 99 

let parseData (data: string) =
    data
    |> _.ToCharArray()
    |> Array.map (fun chr -> int (chr - '0'))

//let parsedD = parseData testData
// [|2; 3; 3; 3; 1; 3; 3; 1; 2; 1; 4; 1; 4; 1; 3; 1; 4; 0; 2|]


type Block =
    | Occupied of int
    | Free

type Disk = Block array


let toBlocks (data: int array) : Block array =
    data
    |> Array.indexed  // number each block from 0
    |> Array.collect (fun (idx, count) ->
        let block = if idx % 2 = 0 then Occupied(idx / 2) else Free
        Array.replicate count block)

//let x1 = toBlocks parsedD


// Part 1

let rec compact1 left right (disk: Disk) =
    if left >= right then
        disk
    else
        match (disk[left], disk[right]) with
        | (Free, Free) -> compact1 left (right - 1) disk
        | (Free, Occupied id) -> disk |> Array.updateAt left (Occupied id) |> Array.updateAt right Free |> compact1 (left + 1) (right - 1)
        | (Occupied _, Free) -> compact1 (left + 1) (right - 1) disk
        | (Occupied _, Occupied _) -> compact1 (left + 1) right disk


let checksum (disk: Block array) =
    disk
    |> Array.indexed
    |> Array.sumBy (fun (i, b) ->
        match b with
        | Free -> 0L
        | Occupied id -> (int64 i) * (int64 id))

let part1 input =
    let data = parseData input
    let disk = toBlocks data
    disk
    |> compact1 0 (Array.length disk - 1)
    |> checksum

//let t1 = part1 testData  // 1928L
//let p1 = part1 prodData  // 6241633730082L


// Part 2

let rec compact2 r (disk: Block array) =
    if r <= 0 then
        disk
    else
        match disk[r] with
        | Free -> compact2 (r - 1) disk
        | Occupied id ->
            let length = disk[..r] |> Array.rev |> Array.takeWhile ((=) (Occupied id)) |> Array.length
            let newR = r - length
            let l = [ 0..r ] |> List.tryFind (fun l -> disk[l .. (l + length - 1)] |> Array.forall ((=) Free))

            match l with
            | None -> compact2 newR disk
            | Some l ->
                (disk, [ 0 .. (length - 1) ])
                ||> List.fold (fun acc d ->
                    acc
                    |> Array.updateAt (l + d) (Occupied id)
                    |> Array.updateAt (newR + 1 + d) Free)
                |> compact2 newR


let part2 input =
    let data = parseData input
    let disk = toBlocks data
    disk
    |> compact2  (Array.length disk - 1)
    |> checksum


//let t2 = part2 testData  // 2858L
//let p2 = part2 prodData  // 6265268809555L
