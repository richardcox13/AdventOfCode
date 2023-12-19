open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open AoC.Common

type Part = {
        A: int
        M: int
        S: int
        X: int
    }

type Rule =
    | Goto of name:string
    | GreaterTHan of Attribute:string * Value:int * Next:string
    | LessTHan of Attribute:string * Value:int * Next:string

type Workflow = {
        Name: string
        Rules: Rule[]
    }

// eg, "ex{x>10:one,m<20:two,a>30:R,A}"
let parseWorkflow (line: string) =
    let m = Regex.Match(line, @"(?<name>\p{Ll}+){(?<rules>.*)}")
    assert (m.Success)
    let name = m.Groups["name"].Value
    let rules = m.Groups["rules"].Value

    { Name = name; Rules = Array.empty<Rule> }

let parsePart (line: string) =
    { A = -1; M = -1; S = -1; X = -1}

// Returns (Workdlow[] * Part[])
let parseInput (input: string[]) =
    let mutable loadParts = false

    let workflows = new List<Workflow>()
    let parts = new List<Part>()
    for line in input do
        match (loadParts, line) with
        | (false, "") ->
            loadParts <- true
        | (false, l) ->
            workflows.Add(parseWorkflow l)
        | (true, l) ->
            parts.Add(parsePart l)

    (workflows |> Seq.toArray), (parts |> Seq.toArray)

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn $"Day 19 Part 1"
    printfn ""
    use diag = Utility.GetTracker ()
    let filename = args[0]
    printfn $"Input file {filename}"
    let input = File.ReadAllLines(filename)
    printfn ""

    let (workflowArray, parts) = parseInput input

    printfn $"There are {workflowArray.Length} workflows, and {parts.Length} parts"

    let result = -1
    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
