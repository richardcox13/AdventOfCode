#time "on"

open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions

type Operation = | Add of int
                 | Multiply of int
                 | Square

type Monkey = {
    // Use queue to pop from front as money examines and throws the items
    // and push on the back when caught
    Items: Queue<int>;
    mutable Inspections: int;
    Operation: Operation;
    TestDivisor: int;
    TestPass: int;
    TestFail: int;
}

let testInput = [|
    {
        Items = Queue([| 79; 98 |]);
        Inspections = 0;
        Operation = Multiply(19);
        TestDivisor = 23;
        TestPass = 2;
        TestFail = 3
    };
    {
        Items = Queue([| 54; 65; 75; 74 |]);
        Inspections = 0;
        Operation = Add(6);
        TestDivisor = 19;
        TestPass = 2;
        TestFail = 0
    };
    {
        Items = Queue([| 79; 60; 97 |]);
        Inspections = 0;
        Operation = Square;
        TestDivisor = 13;
        TestPass = 1;
        TestFail = 3
    };
    {
        Items = Queue([| 74 |]);
        Inspections = 0;
        Operation = Add(3);
        TestDivisor = 17;
        TestPass = 0;
        TestFail = 1
    };
|]


let monkeys = testInput

let oneMoneyOnrRound (m : Monkey) item =
    let updateItem i =
        match m.Operation with
        | Add n -> i+n
        | Multiply n -> i*n
        | Square -> i*i

    let newItem = (updateItem item) / 3
    if newItem % m.TestDivisor = 0 then
        monkeys[m.TestPass].Items.Enqueue(newItem)
    else
        monkeys[m.TestFail].Items.Enqueue(newItem)
    m.Inspections <- m.Inspections+1

let printMoney n m =
    printfn "Monkey #%d: Inspections: %d" n m.Inspections
    printfn "  Items: %s" (m.Items |> Seq.map (fun x -> x.ToString()) |> String.concat ", ")

let printAll msg =
    printfn ""
    printfn "%s" msg
    for m in (monkeys |> Seq.mapi (fun idx m -> (idx, m))) do
        printMoney (fst m) (snd m)

printAll "Before Any Inspections:"

oneMoneyOnrRound monkeys[0] (monkeys[0].Items.Dequeue())

printAll "After one Inspections:"
