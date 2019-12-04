module Advent2019.Day2

    open Xunit
    open FsUnit.Xunit
    open Advent2019.Input
    open Advent2019.Computer
    let part1 memory =
        (execute memory 0).[0]
    
    let part2 memory =
        let expected = 19690720
        let options = seq {
            for noun in 0..100 do
                for verb in 0..100 do
                    let initialState = Array.copy memory
                    Array.set initialState 1 noun;
                    Array.set initialState 2 verb;
                    let finalState = execute initialState 0
                    let result = finalState.[0]
                    if result = expected then
                        yield (noun, verb)
        }
        Seq.head options
        
    [<Fact>]
    let ``Example 1`` () =
        let array = [|1;0;0;0;99|]
        part1 array |> should equal 2
        
    [<Fact>]
    let ``Example 4`` () =
        let array = [|1;1;1;4;99;5;6;0;99|]
        part1 array |> should equal 30
        
    [<Fact>]
    let ``Part 1`` () =
        loadMemory (day 2) |> part1 |> should equal 3654868
        
    [<Fact>]
    let ``Part 2`` () =
        loadMemory (day 2) |> part2 |> fun (noun, verb) -> noun * 100 + verb |> should equal 7014
