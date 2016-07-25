#r @"packages/FSharp.Compiler.Service/lib/net45/FSharp.Compiler.Service.dll"
#load "src/FSharp.Editing/Common/Utils.fs"
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Core.Printf
let checker = FSharpChecker.Create()

let input = """
namespace Paket
open System
open System.Collections.Generic
open System.Globalization

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Cultures =
    let private allLanguageNames =
        let allLanguageNames = 
            CultureInfo.GetCultures CultureTypes.AllCultures
            |> Array.map (fun c -> c.Name)
            |> Array.filter (String.IsNullOrEmpty >> not)
        HashSet<_>(allLanguageNames, StringComparer.OrdinalIgnoreCase)

    let isLanguageName text = 
        if String.IsNullOrWhiteSpace text then
            false
        else
            allLanguageNames.Contains text
"""

let getResult = Async.RunSynchronously
let file = "./foo.fsx"
let options = checker.GetProjectOptionsFromScript(file, input) |> getResult

let result : FSharpParseFileResults = checker.ParseFileInProject(file, input, options) |> getResult


let tree = result.ParseTree |> Option.get
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Ast

module String =
  let prepend p t = p + t

let longIdentToString (li: LongIdent) = li |> Seq.map (fun i -> i.idText) |> String.concat " "

let mapConcat f s = s |> Seq.map f |> Seq.concat
type AstItem =
| Namespace of SynModuleOrNamespace * AstChildren
| ModuleNs of SynModuleOrNamespace * AstChildren
| ModuleDecl of LongIdent * SynModuleDecls * AstChildren
| Let of SynBinding * AstChildren
| Exception of SynTypeDefnRepr
  static member GetBreadCrumbStyle =
    function
    | Namespace(_) -> ()// ...
    | _ -> ()
  static member GetText =
    function
    | Namespace (SynModuleOrNamespace.SynModuleOrNamespace(ident,_,_,_,_,_,_,_), _) ->
        ident 
        |> longIdentToString
        |> String.prepend "ns:"
    | ModuleNs (SynModuleOrNamespace.SynModuleOrNamespace(ident,_,_,_,_,_,_,_), _) ->
        ident 
        |> longIdentToString
        |> String.prepend "mod:"
    | ModuleDecl (ident, _, _) ->
        ident
        |> longIdentToString
        |> String.prepend "mod:"
    | Let(SynBinding.Binding(_,_,_,_,_,_,SynValData.SynValData(_,SynValInfo.SynValInfo(_,_),ident),pattern,_,_,_,_) as x , _)->
        match pattern with
        | SynPat.Named(_,ident,_,_,_) -> ident.idText |> String.prepend "let:"
        | SynPat.LongIdent(ident,idd,_,_,_,_) -> ident.Lid |> longIdentToString |> String.prepend "let:"
        | _ -> pattern.GetType().Name |> String.prepend "let:"
    | Exception(_) -> "exn???"
  static member GetChildren =
    function
    | Namespace (_,children) -> children
    | ModuleNs (_,children) -> children
    | ModuleDecl (_,_,children) -> children
    | Let (_,children) -> children
    | Exception (_) -> Seq.empty
    
and AstChildren = AstItem seq

type SynBinding with
  static member GetAstItems =
    function
    | SynBinding.Binding(_,_,_,_,_,_,_,_,_,_,_,_) as x -> Seq.singleton (Let (x, Seq.empty))

type SynTypeDefn with
  static member GetAstItems =
    function
    | SynTypeDefn.TypeDefn(SynComponentInfo.ComponentInfo(_,_,_,_,_,_,_,_),typeDef,_,_) ->
        match typeDef with
        | SynTypeDefnRepr.Exception(_) as x -> Seq.singleton (Exception x)
        | SynTypeDefnRepr.ObjectModel(_) -> Seq.empty
        | SynTypeDefnRepr.Simple(_) -> Seq.empty


and SynModuleDecl with
  static member GetAstItems =
    function
    | SynModuleDecl.ModuleAbbrev(_,_,_) -> failwithf "module abbrev"
    | SynModuleDecl.NestedModule(SynComponentInfo.ComponentInfo(_,_,_,ident,_,_,_,_), isRec, decls, _, _) as x ->
        let children =
          decls |> mapConcat SynModuleDecl.GetAstItems
        Seq.singleton (ModuleDecl (ident,decls, children))
    | SynModuleDecl.Let(_, bindings, _) -> bindings |> mapConcat SynBinding.GetAstItems
    | SynModuleDecl.DoExpr(_, _, _) ->  failwithf "do"
    | SynModuleDecl.Types(types, _) -> types |> mapConcat SynTypeDefn.GetAstItems
    | SynModuleDecl.Exception(_, _) ->  failwithf "excn"
    | SynModuleDecl.Open(_, _) -> Seq.empty
    | SynModuleDecl.Attributes(_, _) ->  failwithf "Attributes"
    | SynModuleDecl.HashDirective(_, _) ->  failwithf "HashDirective"
    | SynModuleDecl.NamespaceFragment(ns) -> SynModuleOrNamespace.GetAstItems ns

and SynModuleOrNamespace with
  static member GetAstItems = 
    function
    | SynModuleOrNamespace(_,_,isModule,decls,_,_,_,_) as x -> 
      let children =
        decls
        |> Seq.map (SynModuleDecl.GetAstItems)
        |> Seq.concat
      if isModule then
        Seq.singleton ( ModuleNs(x, children))
      else
        Seq.singleton (Namespace(x, children))

let getModuleAndNamespaces tree =
  match tree with
  | ParsedInput.ImplFile (ParsedImplFileInput(_,_,_,_,_,modulesAndNamespaces,_) as p) -> modulesAndNamespaces
  | _ -> []

let rec recur f level astitems  =
  seq {
    for i in astitems do
      yield f level i
      yield! recur f (level+1) (AstItem.GetChildren i)
  }
;;
tree 
|> getModuleAndNamespaces 
|> Seq.map (SynModuleOrNamespace.GetAstItems)
|> Seq.concat
|> recur (fun level item -> sprintf "%s%s" (String.replicate level "  ") (AstItem.GetText item) ) 0
|> Seq.iter (printfn "%s")

(*
let printPos (p: pos) = sprintf "pos(%i,%i)" p.Line p.Column
let printRange (r: range) = sprintf "range(%s,%s)" (printPos r.Start) (printPos r.End)
fsi.AddPrinter printPos
fsi.AddPrinter printRange
fsi.PrintWidth <- 120*)