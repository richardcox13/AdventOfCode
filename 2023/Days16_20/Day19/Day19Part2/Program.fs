open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open AoC.Common

type Rule =
    | Goto of name:string
    | GreaterThan of Attribute:string * Value:int * Next:string
    | LessThan of Attribute:string * Value:int * Next:string
    override r.ToString() =
        match r with
        | Goto(name) -> $"Goto {name}"
        | GreaterThan(attr, value, name) -> $"if {attr}>{value} goto {name}"
        | LessThan(attr, value, name) -> $"if {attr}<{value} goto {name}"
    member r.ToString(f: string) =
        match (f, r) with
        | ("s", Goto(name)) -> " -> " + name
        | ("s", GreaterThan(attr,value,_)) ->
            $"{attr}>{value}"
        | ("s", LessThan(attr,value,_)) ->
            $"{attr}<{value}"
        | _ -> r.ToString()

type Workflow = {
        Name: string
        Rules: Rule[]
    }

let maxAttribute = 4000
let minAttribute = 1

// eg, "ex{x>10:one,m<20:two,a>30:R,A}"
let parseWorkflow (line: string) =
    let m = Regex.Match(line, @"(?<name>[ARa-z]+){(?<rules>.*)}")
    assert (m.Success)
    let name = m.Groups["name"].Value
    let ruleStr = m.Groups["rules"].Value
    let rules
        = ruleStr.Trim('{', '}').Split(',')
          |> Seq.map (fun r ->
                                let m = Regex.Match(r, @"(?:(?<attr>[amsx])(?<op>[<>])(?<val>\d+):)?((?<name>[ARa-z]+))")
                                assert (m.Success)
                                let n = m.Groups["name"].Value
                                if m.Groups["attr"].Success then
                                    let v = Int32.Parse(m.Groups["val"].Value)
                                    let a = m.Groups["attr"].Value
                                    if m.Groups["op"].Value = ">" then
                                        GreaterThan(a, v, n)
                                    else
                                        LessThan(a, v, n)
                                else
                                    Goto(n)
                            )
          |> Seq.toArray

    { Name = name; Rules = rules }

// Returns Workdlow[]
let parseInput (input: string[]) =
    let mutable loadParts = false

    let workflows = new List<Workflow>()
    for line in (input |> Seq.where (fun _ -> not loadParts)) do
        match line with
        | "" ->
            loadParts <- true
        | l ->
            workflows.Add(parseWorkflow l)

    (workflows |> Seq.toArray)

let pathToString (rules: Rule seq) =
    rules |> Seq.map (fun r -> r.ToString("s")) |> String.concat ", "

let XXgetAllAcceptPaths (workflows: Map<string, Workflow>) =
    let invertRule rule =
        match rule with
        | Goto(_) -> failwith "Cannot invert Goto rule"
        | GreaterThan(attr, value, name) -> LessThan(attr, value+1, "x-" + name)
        | LessThan(attr, value, name) -> GreaterThan(attr, value-1, "x-name")

    let invertRules rules =
        rules |> List.map (fun r -> invertRule r)

    // return rule[0]
    // NB. accymulator arg holds reverse order for efficiency
    let rec followPaths curWorkflowName (acc: Rule list) =
        seq {
            let w = workflows[curWorkflowName]
            for ruleIdx in 0 .. (w.Rules.Length-1) do
                let r = w.Rules[ruleIdx]
                match r with
                | Goto(name) ->
                    match name with
                    | "A" ->
                        yield (Some acc)
                    | "R" ->
                        yield None
                    | _ ->
                        assert (ruleIdx = w.Rules.Length-1)
                        let rulesToAdd
                            = invertRules (w.Rules[0 .. ruleIdx-1] |> List.ofArray)
                              |> List.rev
                        yield! (followPaths name (List.append rulesToAdd acc))
                | GreaterThan(_,_,name)
                | LessThan(_,_,name) ->
                    let rulesToAdd
                        = invertRules (w.Rules[0 .. ruleIdx-1] |> List.ofArray) 
                          |> List.rev
                    match name with
                    | "A" ->
                        let res = r :: (List.append rulesToAdd acc)
                        yield (Some res)
                    | "R" ->
                        yield None
                    | _ ->
                        let res = r :: (List.append rulesToAdd acc)
                        yield! (followPaths name res)
        } |> Seq.where (fun x -> x.IsSome)

    let start = "in"

    followPaths start []
      |> Seq.map (function | Some x -> x | _ -> failwith "oops")
      |> Seq.map (fun l -> l |> List.rev)

// We depennd on this being a reference type
// Min,Max is inclusive
type AttrRange = { Min: int; Max: int} static member Create() = { Min = minAttribute; Max = maxAttribute }

type PartRange =
    {
        A: AttrRange
        M: AttrRange
        S: AttrRange
        X: AttrRange
    }
    member x.GetAttr a =
        match a with
        | "a" -> x.A
        | "m" -> x.M
        | "s" -> x.S
        | "x" -> x.X
        | q -> failwith $"Invalid attribute '{q}'"
    static member Create () =
        { A = AttrRange.Create (); M = AttrRange.Create (); S = AttrRange.Create (); X = AttrRange.Create () }
    override x.ToString() =
        let s
            = [| "a"; "m"; "s"; "x" |]
              |> Seq.map (fun a ->
                    let t = x.GetAttr(a)
                    $"{a} in [{t.Min}, {t.Max}]"
                )
        "{" + (s |> String.concat "; ") + "}"

let r2s (rule: Rule) = rule.ToString("s")

// workflowName * rule * parts
type HistoryEntry = string * Rule * PartRange

let followPathsForPartRanges (workflows: Map<string, Workflow>) =
    let applyRuleToParts rule parts =
        match rule with
        | Goto(_) -> failwith $"Unexpected goto rule"
        | GreaterThan(attr,value,name) ->
            let ps = match attr with
                      | "a" -> { parts with A = {parts.A with Min = max parts.A.Min (value + 1)}}
                      | "m" -> { parts with M = {parts.M with Min = max parts.M.Min (value + 1)}}
                      | "s" -> { parts with S = {parts.S with Min = max parts.S.Min (value + 1)}}
                      | "x" -> { parts with X = {parts.X with Min = max parts.X.Min (value + 1)}}
                      | a -> failwith $"Unexpected attr name '{a}'"
            ps,name
        | LessThan(attr,value,name) ->
            let ps = match attr with
                      | "a" -> { parts with A = {parts.A with Max = min parts.A.Max (value - 1)}}
                      | "m" -> { parts with M = {parts.M with Max = min parts.M.Max (value - 1)}}
                      | "s" -> { parts with S = {parts.S with Max = min parts.S.Max (value - 1)}}
                      | "x" -> { parts with X = {parts.X with Max = min parts.X.Max (value - 1)}}
                      | a -> failwith $"Unexpected attr name '{a}'"
            ps,name

    let rec processRules wfName rules history parts =
        // TODO handle the cases
        let rule = rules |> List.head
        let (newParts, nextWfName) = applyRuleToParts rule parts
        let newHist = (wfName, rule, newParts) :: history

        Seq.singleton (nextWfName, newHist, newParts)

    let rec processWorkflow wfName (history: HistoryEntry list) (parts: PartRange) =
        let wf = workflows[wfName]
        seq {
            for (nextWf, hist, ps) in (processRules wfName (wf.Rules |> List.ofArray) history parts) do
                match nextWf with
                | "A" -> Some (hist, parts)
                | "R" -> None
                | n ->
                    yield! (processWorkflow nextWf hist ps)
        }

    processWorkflow "in" [] (PartRange.Create())

[<EntryPoint>]
let main(args) =
    printfn $"Working folder: {Environment.CurrentDirectory}"
    printfn $"Day 19 Part 2"
    printfn ""
    use diag = Utility.GetTracker ()
    let filename = args[0]
    printfn $"Input file {filename}"
    let input = File.ReadAllLines(filename)
    printfn ""

    let workflowArray = parseInput input
    printfn $"There are {workflowArray.Length} workflows"

    let workflows = Map (workflowArray |> Seq.map (fun w -> w.Name, w))

    let printHist hist =
        let h = hist |> List.rev

        let rec inner hh =
            match hh with
            | (wf: string, rule: Rule, ps: PartRange) :: rest ->
                let rs = rule.ToString("s")
                printfn $"  in \"{wf}\": {rs} gives {ps}"
                inner rest
            | [] -> ()

        inner h

    let res
        = followPathsForPartRanges workflows
          |> Seq.where (fun x -> x.IsSome)
          |> Seq.map (fun x -> x.Value)
          |> Seq.map (fun (hist, parts) ->
                printHist hist
                25L
             )
          |> Seq.sum

    (*let allPaths = getAllAcceptPaths workflows |> Seq.toArray*)

    (*let result
        = allPaths
          |> Array.indexed
          //|> Seq.take 1 //* ***** DEBUG ***** *
          |> Seq.map (fun (i,p) ->
                printfn $"{i}: {pathToString p}: "
                let x = calcPartRangeFromRulePath p
                printfn $"    {x:``#,0``} parts"
                x
            )
          |> Seq.sum*)

    let result = -1L
    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
