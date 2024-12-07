// Advent of Code --- Day 6 Part 2: Guard Gallivant ---

open System.IO

/// Get data from the `..\data` directory and store it in a list of string
let getData (fileName: string) =
    File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\..\Data\{fileName}"""
    |> List.ofSeq

/// Test data
let testData = getData "test06.txt"

/// Production data
let prodData = getData "data06.txt"

// Data definitions

type Direction = U | R | D | L
type Guard = { Location : int * int; Direction : Direction  }
type Reason = OutOfBounds | Loop
type State = { Guard : Guard; Visited : Set<Guard>; FinishReason : Reason option }


// function definitios

let turnRight dir =
    match dir with
    | U -> R
    | R -> D
    | D -> L
    | L -> U

/// Get movement offset given the direction
let offset dir =
    match dir with
    | U -> (-1,0)
    | D -> (1,0)
    | R -> (0,1)
    | L -> (0,-1)


let maxR grid =
    (grid |> Seq.length) - 1

let maxC (grid: 'a list) =
    (grid[0] |> Seq.length) - 1


/// get a list of token locations given a token
let getTokens (grid: string list) (token: char) =
    [ for (rowNumber, row) in grid |> Seq.indexed do
        for (columnNumber, cell) in row |> Seq.indexed do
            if cell = token then yield (rowNumber, columnNumber)
    ]

/// get a list of obstacle locations from the grid
let getObsticles grid =
    getTokens grid '#'
    |> Set.ofSeq

/// get the location of the Guard
let getGuard grid =
    getTokens grid '^'
    |> List.head

/// get the next guard for the next move
let nextGuard  obstacles (guard) =
    let { Location = (ros,col); Direction = dir } = guard

    let inFront =
        let (dr, dc) = offset dir
        (ros + dr, col + dc)

    if obstacles |> Set.contains inFront
    then { guard with Direction = turnRight dir}
    else { guard with Location = inFront }

/// The game loop
let rec patrol obstacles state guard  grid =
    let inBounds (row, col) =
        0 <= row && row <= (maxR grid) && 0 <= col && col <= (maxC grid)

    if state.Guard.Location |> inBounds |> not
    then { state with FinishReason = Some OutOfBounds}
    else
        let nextGuard1 = nextGuard obstacles state.Guard
        if state.Visited |> Set.contains nextGuard1 then
            { state with FinishReason = Some Loop }
        else
        let newState =
            { state with
                Guard = nextGuard1
                Visited = state.Visited |> Set.add state.Guard }
        patrol obstacles newState guard  grid


///////////////////////////////////

let part2 (grid: string list) =
    let obstacles = getObsticles grid
    let guard = getGuard grid
    let init : State = { Guard = { Location = guard; Direction = U}; Visited = Set.empty; FinishReason = None  }
    let finalState = (patrol obstacles init guard  grid)

    let visited =
        finalState.Visited
        |> Seq.map (fun grd -> grd.Location)
        |> List.ofSeq
        |> List.distinct

    let runs =
        visited
        |> List.map (fun loc -> obstacles |> Set.add loc)
        |> List.map (fun obstacles -> patrol obstacles init guard grid)
        |> List.filter (fun state -> state.FinishReason = Some Loop)

    runs |> Seq.length


let tst = part2 testData // 6
let ans = part2 prodData // 1995

printfn "Test:   %d" tst
printfn "Answer: %d" ans

// Answer:  1995
