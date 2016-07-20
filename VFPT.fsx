#r @"packages\FSharp.Compiler.Service\lib\net45/FSharp.Compiler.Service.dll"
#r @"bin/FSharp.Editing.dll"

open FSharp.Editing
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Core.Printf

let filename = System.IO.Path.Combine(__SOURCE_DIRECTORY__, @"tests\data\LanguageServiceSampleFile.fs")

let sourceTok = FSharpSourceTokenizer([], filename)

/// Tokenize a single line of F# code
let rec tokenizeLine (tokenizer:FSharpLineTokenizer) state (line:string) =
    seq {
        match tokenizer.ScanToken(state) with
        | Some tok, state ->
            yield state, Some tok
            // Print token name
            printfn "%s %i %i %s " tok.TokenName (tok.LeftColumn) (tok.FullMatchedLength) (line.Substring(tok.LeftColumn, tok.FullMatchedLength))
            // Tokenize the rest, in the new state
            yield! tokenizeLine tokenizer state line
        | None, state -> yield state, None
    }
let rec tokenizeLines state count lines = 
    seq {
        match lines with
        | line::lines ->
            let tokenizer = sourceTok.CreateLineTokenizer(line)
            let result = tokenizeLine tokenizer state line
            yield line, result
            for (state, tok) in result do
                if tok.IsNone then
                    yield! tokenizeLines state (count+1) lines
        | [] -> ()
    } 

let lines = System.IO.File.ReadAllLines(filename) |> Seq.toList


printfn "%A" <| tokenizeLines 0L 0 lines

let checker = FSharpChecker.Create()

let input = """
open System
open System.Collections.Generic

let foo a =
  printfn "%s" a

type Union = State | Of | TheUnion

let zzz () =
  let u = TheUnion
  match u with
"""

let getResult = Async.RunSynchronously
let file = "./foo.fsx"
let options = checker.GetProjectOptionsFromScript(file, input) |> getResult

let result = checker.ParseFileInProject(file, input, options) |> getResult