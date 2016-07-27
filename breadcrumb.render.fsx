#r @"packages/FSharp.Compiler.Service/lib/net45/FSharp.Compiler.Service.dll"
#load "breadcrumb.ast.fsx"
open System.IO
open Breadcrumb.ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Core.Printf
open Microsoft.FSharp.Compiler.Ast

let checker = FSharpChecker.Create()

let rec listFsharpFiles folder =
  seq {
    yield! Directory.EnumerateFiles(folder, "*.fs")
    yield! Directory.EnumerateFiles(folder, "*.fsx")
    yield! Directory.EnumerateFiles(folder, "*.fsi")
    for folder in folder |> Directory.GetDirectories do
      yield! listFsharpFiles folder
  }

let getParseTree file =
  let getResult = Async.RunSynchronously
  let input = file |> File.ReadAllText
  let options = checker.GetProjectOptionsFromScript(file, input) |> getResult
  let result : FSharpParseFileResults = checker.ParseFileInProject(file, input, options) |> getResult
  result.ParseTree |> Option.get

let prettyPrint file =
  let tree = getParseTree file
  let rec recur f level astitems  =
    seq {
      for i in astitems do
        yield f level i
        yield! recur f (level+1) (AstItem.GetChildren i)
    }

  tree 
  |> getModuleAndNamespaces 
  |> Seq.map (SynModuleOrNamespace.GetAstItems)
  |> Seq.concat
  |> recur (fun level item -> sprintf "%s%s" (String.replicate level "  ") (AstItem.GetText item) ) 0
  |> Seq.iter (printfn "%s")

let folder = __SOURCE_DIRECTORY__
for f in listFsharpFiles folder do
  printfn "=== %s ====" f
  prettyPrint f

(*
let printPos (p: pos) = sprintf "pos(%i,%i)" p.Line p.Column
let printRange (r: range) = sprintf "range(%s,%s)" (printPos r.Start) (printPos r.End)
fsi.AddPrinter printPos
fsi.AddPrinter printRange
fsi.PrintWidth <- 120*)