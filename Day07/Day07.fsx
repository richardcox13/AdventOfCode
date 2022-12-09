#time "on"

open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions

open System.Text.RegularExpressions

let testInputs = [|
    "$ cd /";
    "$ ls";
    "dir a";
    "14848514 b.txt";
    "8504156 c.dat";
    "dir d";
    "$ cd a";
    "$ ls";
    "dir e";
    "29116 f";
    "2557 g";
    "62596 h.lst";
    "$ cd e";
    "$ ls";
    "584 i";
    "$ cd ..";
    "$ cd ..";
    "$ cd d";
    "$ ls";
    "4060174 j";
    "8033020 d.log";
    "5626152 d.ext";
    "7214296 k";
|]

type Node = {
    Name: string;
    mutable Info: NodeInfo list
}
and NodeInfo = 
    | File of int
    | Folder of Node list

let (|Match|_|) pattern input =
    let m = Regex.Match(input, pattern) in
    if m.Success then Some ([| for g in m.Groups -> g.Value |]) else None

let root = { Name = "/" ; Info = [] }

let buildTree (input: string seq) =
    let mutable current = root
    let stack = Stack<Node>()
    stack.Push(current)
    let mutable inFolderListing = false

    for inp in input do
        if inFolderListing then
            match inp with
            | Match "^\$" _ ->
                inFolderListing <- false
                printfn "Listing end"
            | Match "dir (\w+)" m ->
                assert (m.Length = 2)
                printfn "Found folder %s" m[1]
            | Match "(\d+) ([0-9.a-z]+)" m ->
                assert (m.Length = 3)
                printfn "Foound file \"%s\" length %d" m[2] (int m[1])
            | _ -> ()
        // Do not use else, want to fall through
        if not inFolderListing then
            match inp with
            | "$ cd .." -> printfn "Move up"
            | Match "^\\$ cd ([0-9.a-z]+)" m ->
                assert (m.Length = 2)
                printfn "Move into folder \"%s\"" m[1]
            | "$ ls" -> 
                printfn "List start"
                inFolderListing <- true
            | _ -> ()

buildTree testInputs
