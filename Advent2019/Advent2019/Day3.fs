module Advent2019.Day3
open Xunit
open FsUnit.Xunit
open Advent2019.Input

type Move = Left of int | Right of int | Up of int | Down of int
type Location = {
    x: int
    y: int
}
type Intersection = Location * int
type Section = {
    start: Location
    finish: Location
    steps: int // Steps taken at start of section
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
        
let toSection (state: (Location * int) * Section list) move: ((Location * int) * Section list) =
    let start = fst (fst state)
    let (finish, l) =
        match move with
        | Left i -> ({ x = start.x - i; y = start.y }, i)
        | Right i -> ({ x = start.x + i; y = start.y }, i)
        | Up i -> ({ x = start.x; y = start.y + i }, i)
        | Down i -> ({ x = start.x; y = start.y - i }, i)
    let steps = snd (fst state)
    ((finish, steps + l), { start = start; finish = finish; steps = steps } :: (snd state))
    
let lineToSections seq =
    Array.map toMove seq
    |> Array.fold toSection (({ x = 0; y = 0 }, 0), [])
    
let partialSteps vertical horizontal =
    abs (horizontal.start.y - vertical.start.y) +
    abs (vertical.start.x -  horizontal.start.x)
    
let intersect (a : Section) (b : Section) =
    if isVertical a && isHorizontal b && maxY a > maxY b && minY a < maxY b && minX b < maxX a && maxX b > maxX a then
        Some ({ x = maxX a; y = maxY b }, (a.steps + b.steps + partialSteps a b))
    else if isVertical b && isHorizontal a && maxY b > maxY a && minY b < maxY a && minX a < maxX b && maxX a > maxX b then
        Some ({ x = maxX b; y = maxY a }, (b.steps + a.steps + partialSteps b a))
    else
        None

let intersects (baseline : Section list) (section : Section ) =
    Seq.map (intersect section) baseline |> Seq.choose id |> Seq.toList
    
let distance a b =
    abs (a.x - b.x) + abs (a.y - b.y)
    
let distanceOrigo section = distance { x = 0; y= 0 } { x = section.x; y = section.y }

let readLine (str : string) = str.Split([|','|])

let nearestIntersectionLocation seqs =
    match Seq.map lineToSections seqs |> Seq.map snd |> Seq.toList with
    | a :: b :: [] -> List.collect (intersects a) b |> List.map (fst >> distanceOrigo) |> List.min
let fewestStepsIntersection seqs =
    match Seq.map lineToSections seqs |> Seq.map snd |> Seq.toList with
    | a :: b :: [] -> List.collect (intersects a) b |> List.minBy snd |> snd
    
let part1 lines =
    lines |> Seq.map readLine |> nearestIntersectionLocation
    
let part2 lines =
    lines |> Seq.map readLine |> fewestStepsIntersection

[<Fact>]
let ``Example 1`` () =
    let arrays = [|[|"R8";"U5";"L5";"D3"|];[|"U7";"R6";"D4";"L4"|]|]
    let result = nearestIntersectionLocation arrays
    result |> should equal 6
[<Fact>]
let ``Example 2`` () =
    let arrays = [|[|"R75";"D30";"R83";"U83";"L12";"D49";"R71";"U7";"L72"|];[|"U62";"R66";"U55";"R34";"D71";"R55";"D58";"R83"|]|]
    let result = nearestIntersectionLocation arrays
    result |> should equal 159
    
[<Fact>]
let ``Example 3`` () =
    let first = ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51";"U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]
    let result = part1 first
    result |> should equal 135
    
[<Fact>]
let ``Part 1`` () =
    let lines = System.IO.File.ReadLines(day 3)
    let result = part1 lines
    result |> should equal 221
    
[<Fact>]
let ``Part 2 Example 1`` () =
    let arrays = [|[|"R8";"U5";"L5";"D3"|];[|"U7";"R6";"D4";"L4"|]|]
    let result = fewestStepsIntersection arrays
    result |> should equal 30
[<Fact>]
let ``Part 2 Example 2`` () =
    let arrays = [|[|"R75";"D30";"R83";"U83";"L12";"D49";"R71";"U7";"L72"|];[|"U62";"R66";"U55";"R34";"D71";"R55";"D58";"R83"|]|]
    let result = fewestStepsIntersection arrays
    result |> should equal 610
    
[<Fact>]
let ``Part 2Example 3`` () =
    let first = ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51";"U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]
    let result = part2 first
    result |> should equal 410
[<Fact>]
let ``Part 2`` () =
    let lines = System.IO.File.ReadLines(day 3)
    let result = part2 lines
    result |> should equal 18542
