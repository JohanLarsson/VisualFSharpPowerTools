#time
#load @"paket-files/include-scripts/net45/include.fsharp.compiler.service.fsx"
#r "src/FSharp.Editing.VisualStudio/bin/Release/FSharp.Editing.dll"
#r "src/FSharp.Editing.VisualStudio/bin/Release/FSharp.Editing.VisualStudio.dll"
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

let getParseTree filename input =
  let getResult = Async.RunSynchronously

  let options = checker.GetProjectOptionsFromScript(filename, input) |> getResult
  let result : FSharpParseFileResults = checker.ParseFileInProject(filename, input, options) |> getResult
  result.ParseTree |> Option.get

let prettyPrint file =
  let tree =
    file 
    |> File.ReadAllText
    |> getParseTree "./foo.fsx"
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
(*
let folder = __SOURCE_DIRECTORY__
for f in listFsharpFiles folder |> Seq.take 1 do
  printfn "=== %s ====" f
  prettyPrint f*)

(*
let printPos (p: pos) = sprintf "pos(%i,%i)" p.Line p.Column
let printRange (r: range) = sprintf "range(%s,%s)" (printPos r.Start) (printPos r.End)
fsi.AddPrinter printPos
fsi.AddPrinter printRange
fsi.PrintWidth <- 120*)

open System.Windows.Forms
open System.Collections.Generic

let form = new Form(Width=800, Height = 300)

let textbox = new TextBox(Anchor = (AnchorStyles.Left|||AnchorStyles.Bottom|||AnchorStyles.Top), Width = 400, Height = 300)
textbox.Multiline <- true
let tree = new TreeView(Anchor = (AnchorStyles.Right|||AnchorStyles.Bottom|||AnchorStyles.Top), Width = 400, Height = 300, Left = 400)
form.Controls.Add(textbox)
form.Controls.Add(tree)

textbox.TextChanged.Add(fun _ ->
  let ast = getParseTree "./foo.fsx" textbox.Text
  tree.Nodes.Clear()


  let rootNode = new TreeNode("root")
  tree.Nodes.Add rootNode

  let rec makeNode (parent: TreeNode) astItem =
    let node = new TreeNode(AstItem.GetText astItem)
    node.Tag <- astItem
    parent.Nodes.Add(node) |> ignore
    for c in AstItem.GetChildren astItem do
      makeNode node c |> ignore
    node

  ast 
  |> getModuleAndNamespaces 
  |> Seq.map SynModuleOrNamespace.GetAstItems
  |> Seq.concat
  |> Seq.iter (makeNode rootNode >> ignore)

)

form.Show()