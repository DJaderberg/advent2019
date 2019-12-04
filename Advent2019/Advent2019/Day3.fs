module Advent2019.Day3
open Xunit
open FsUnit.Xunit
open Advent2019.Input

type Move = Left of int | Right of int | Up of int | Down of int
type Location = {
    x: int
    y: int
}
type Section = {
    start: Location
    finish: Location
}

let minX section = min section.start.x section.finish.x
let maxX section = max section.start.x section.finish.x
let minY section = min section.start.y section.finish.y
let maxY section = max section.start.y section.finish.y
let isVertical section = maxX section = minX section
let isHorizontal section = not (isVertical section)

let toMove (input: string) : Move =
    match input.ToCharArray() |> Array.toList with
    | dir :: numbers ->
        let length = System.String.Concat numbers |> int
        match dir with
        | 'L' -> Left length
        | 'R' -> Right length
        | 'U' -> Up length
        | 'D' -> Down length
        
let toSection (state: Location * Section list) move: (Location * Section list) =
    let start = fst state
    let finish =
        match move with
        | Left i -> { x = start.x - i; y = start.y }
        | Right i -> { x = start.x + i; y = start.y }
        | Up i -> { x = start.x; y = start.y + i }
        | Down i -> { x = start.x; y = start.y - i }
    (finish, { start = start; finish = finish } :: (snd state))
    
let lineToSections seq =
    Array.map toMove seq
    |> Array.fold toSection ({ x = 0; y = 0 }, [])
    
let intersect (a : Section) (b : Section) =
    if isVertical a && isHorizontal b && maxY a > maxY b && minY a < maxY b && minX b < maxX a && maxX b > maxX a then
        Some { x = maxX a; y = maxY b }
    else if isVertical b && isHorizontal a && maxY b > maxY a && minY b < maxY a && minX a < maxX b && maxX a > maxX b then
        Some { x = maxX b; y = maxY a }
    else
        None

let intersects (baseline : Section list) (section : Section ) =
    Seq.map (intersect section) baseline |> Seq.choose id |> Seq.toList
    
let distance a b =
    abs (a.x - b.x) + abs (a.y - b.y)
    
let distanceOrigo = distance { x = 0; y= 0 }

let readLine (str : string) = str.Split([|','|])

let nearestIntersection seqs =
    match Seq.map lineToSections seqs |> Seq.map snd |> Seq.toList with
    | a :: b :: [] -> List.collect (intersects a) b |> List.minBy distanceOrigo |> distanceOrigo
    
let part1 lines =
    lines |> Seq.map readLine |> nearestIntersection

[<Fact>]
let ``Example 1`` () =
    let arrays = [|[|"R8";"U5";"L5";"D3"|];[|"U7";"R6";"D4";"L4"|]|]
    let result = nearestIntersection arrays
    result |> should equal 6
[<Fact>]
let ``Example 2`` () =
    let arrays = [|[|"R75";"D30";"R83";"U83";"L12";"D49";"R71";"U7";"L72"|];[|"U62";"R66";"U55";"R34";"D71";"R55";"D58";"R83"|]|]
    let result = nearestIntersection arrays
    result |> should equal 159
    
[<Fact>]
let ``Example 3`` () =
    let first = ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51";"U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]
    let result = part1 first
    result |> should equal 135
    
[<Fact>]
let ``Part 1`` () =
    let first = System.IO.File.ReadLines(day 3)
    let result = part1 first
    result |> should equal 221
