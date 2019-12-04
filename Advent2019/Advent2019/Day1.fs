module Advent2019.Day1
    open Xunit
    open FsUnit.Xunit
    open Advent2019.Input

    let fuel mass = max 0 (mass / 3 - 2)
    let rec fuelRec oldFuel =
        let extraFuel = max 0 (fuel oldFuel)
        if extraFuel = 0 then 0 else extraFuel + fuelRec extraFuel
    let fuelWithOwnWeight mass =
        let baseFuel = fuel mass
        baseFuel + fuelRec (baseFuel)

    [<Fact>]
    let ``Example 1`` () = fuel 12 |> should equal 2
    [<Fact>]
    let ``Example 2`` () = fuel 14 |> should equal 2
    [<Fact>]
    let ``Example 3`` () = fuel 1969 |> should equal 654
    [<Fact>]
    let ``Example 4`` () = fuel 100756 |> should equal 33583
    [<Fact>]
    let ``Part 2 Example 1`` () = fuelWithOwnWeight 14 |> should equal 2
    [<Fact>]
    let ``Part 2 Example 2`` () = fuelWithOwnWeight 1969 |> should equal 966
    [<Fact>]
    let ``Part 2 Example 3`` () = fuelWithOwnWeight 100756 |> should equal 50346
        
    [<Fact>]
    let ``Part 1`` () =
        let file = day 1
        let lines = System.IO.File.ReadLines(file) |> Seq.map int
        (lines |> Seq.map fuel |> Seq.fold (+) 0) |> should equal 3323874
    [<Fact>]
    let ``Part 2`` () =
        let file = day 1
        let lines = System.IO.File.ReadLines(file) |> Seq.map int
        (lines |> Seq.map fuelWithOwnWeight |> Seq.fold (+) 0) |> should equal 4982961
