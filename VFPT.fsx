#r @"packages\FSharp.Compiler.Service\lib\net45/FSharp.Compiler.Service.dll"
#r @"bin/FSharp.Editing.dll"
open FSharp.Editing
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

let result = checker.ParseFileInProject(file, input, options) |> getResult

let ast = result.ParseTree.Value

