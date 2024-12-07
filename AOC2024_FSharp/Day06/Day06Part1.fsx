// Advent of Code --- Day 6 Part 1: Guard Gallivant ---

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
type State = { Guard : Guard; Visited : Set<int * int>  }


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
let nextGuard (guard) obstacles =
    let { Location = (ros,col); Direction = dir } = guard
    
    let inFront =
        let (dr, dc) = offset dir
        (ros + dr, col + dc)
    
    if obstacles |> Set.contains inFront
    then { guard with Direction = turnRight dir}
    else { guard with Location = inFront }

/// The game loop
let rec patrol state guard obstacles grid =
    let inBounds (row, col) =
        0 <= row && row <= (maxR grid) && 0 <= col && col <= (maxC grid)

    if state.Guard.Location |> inBounds |> not
    then state
    else
        let newState =
         { state with
            Guard = nextGuard state.Guard obstacles
            Visited = state.Visited |> Set.add state.Guard.Location }
        patrol newState guard obstacles grid


///////////////////////////////////

let part1 (grid: string list) =
    let obstacles = getObsticles grid
    let guard = getGuard grid
    let init : State = { Guard = { Location = guard; Direction = U}; Visited = Set.empty }
    let finalState = (patrol init guard obstacles grid)

    finalState.Visited |> Set.count


let tst = part1 testData // 41
let ans = part1 prodData // 4890

printfn "Test:   %d" tst
printfn "Answer: %d" ans

// Answer: 4890
