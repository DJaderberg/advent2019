module Advent2019.Computer

    let rec execute (memory : int array) (pc : int) =
        let op = memory.[pc]
        match op with
        | 1 -> Array.set memory memory.[pc + 3] (memory.[memory.[pc + 1]] + memory.[memory.[pc + 2]]); execute memory (pc + 4)
        | 2 -> Array.set memory memory.[pc + 3] (memory.[memory.[pc + 1]] * memory.[memory.[pc + 2]]); execute memory (pc + 4)
        | 99 -> memory
    
    let loadMemory file =
        System.IO.File.ReadLines(file)
        |> Seq.head
        |> fun s -> s.Split(',')
        |> Seq.map int
        |> Seq.toArray
