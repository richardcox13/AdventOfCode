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

let getAllAcceptPaths (workflows: Map<string, Workflow>) =
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
type AttrRange = { mutable Min: int; mutable Max: int} static member Create() = { Min = minAttribute; Max = maxAttribute }
type PartRange = Map<string, AttrRange>

let partRangeToString (part: PartRange) =
    let s
        = [| "a"; "m"; "s"; "x" |]
          |> Seq.map (fun a ->
                let t = part[a]
                $"{a} in [{t.Min}, {t.Max}]"
            )
    "{" + (s |> String.concat "; ") + "}"

let r2s (rule: Rule) = rule.ToString("s")

let calcPartRangeFromRulePath (rules: Rule list) =
    let applyRule rule (part: PartRange) =
        printf "    Applying rule  %s " (r2s rule)
        let res = match rule with
                  | Goto(_) -> failwith $"Goto found in rule list"
                  | LessThan(attr,value,_) ->
                     let a = part[attr]
                     a.Max <- value-1
                  | GreaterThan(attr,value,_) ->
                     let a = part[attr]
                     a.Min <- value+1
        printfn "gives %s" (partRangeToString part)
        res

    let getRange attr =
        if attr.Min > attr.Max then
            0L
        else
            int64 (attr.Max - attr.Min + 1)

    let rec iterate rules (part: PartRange) =
        match rules with
        | [] ->
            // Done... 
            let aa = getRange (part["a"])
            let mm = getRange (part["m"])
            let ss = getRange (part["s"])
            let xx = getRange (part["x"])
            printfn $"    There are {aa} a's; {mm} m's; {ss} s's; & {xx} x's"
            aa * mm * ss * xx
        | [r] ->
            applyRule r part
            iterate [] part
        | r :: rest ->
            applyRule r part
            iterate rest part
    
    let p = Map [|
        ("a", AttrRange.Create()); 
        ("m", AttrRange.Create()); 
        ("s", AttrRange.Create()); 
        ("x", AttrRange.Create()); 
    |]

    iterate rules p

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

    let allPaths = getAllAcceptPaths workflows |> Seq.toArray

    let result
        = allPaths
          |> Array.indexed
          //|> Seq.take 1 (* ***** DEBUG ***** *)
          |> Seq.map (fun (i,p) ->
                printfn $"{i}: {pathToString p}: "
                let x = calcPartRangeFromRulePath p
                printfn $"    {x:``#,0``} parts"
                x
            )
          |> Seq.sum

    printfn ""
    printfn $"Result = {result:``#,0``} ({result})"
    printfn ""
    0
