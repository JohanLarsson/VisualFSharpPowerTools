#r @"packages/FSharp.Compiler.Service/lib/net45/FSharp.Compiler.Service.dll"
#load "src/FSharp.Editing/Common/Utils.fs"
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Core.Printf

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
| Lambda of SynExpr * AstChildren
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
    | Lambda(epxr, children) ->
        "\ ->"
    | Exception(_) -> "exn???"
  static member GetChildren =
    function
    | Namespace (_,children)
    | ModuleNs (_,children)
    | ModuleDecl (_,_,children)
    | Let (_,children)
    | Lambda (_, children) 
      -> children
    | Exception (_) -> Seq.empty
    
and AstChildren = AstItem seq
type SynExpr with
  static member GetAstItems =
    function
    | SynExpr.LetOrUse(_,_,bindings,_,_) -> bindings |> mapConcat SynBinding.GetAstItems
    | SynExpr.Paren(expr, leftParenRange, rightParenRange, range) -> Seq.empty
    | SynExpr.Quote(operator, isRaw, quotedSynExpr, isFromQueryExpression, range) -> Seq.empty
    | SynExpr.Const(constant, range) ->Seq.empty
    | SynExpr.Typed(expr, typeSig, range) -> Seq.empty
    | SynExpr.Tuple(exprs, commaRanges, range) -> Seq.empty
    | SynExpr.ArrayOrList(isList, exprs, range) -> Seq.empty
    | SynExpr.Record(baseInfo, copyInfo, recordFields, range) -> Seq.empty
    | SynExpr.New(isProtected, typeName, expr, range) -> Seq.empty
    | SynExpr.ObjExpr(objType, argOpt, bindings, extraImpls, newPos, range) -> Seq.empty
    | SynExpr.While(spWhile, whileBody, doBody, range) -> doBody |> SynExpr.GetAstItems
    | SynExpr.For(spFor, id, idBody, _, toBody, doBody, range) ->  doBody |> SynExpr.GetAstItems
    | SynExpr.ForEach(spFor, seqExprOnly, isFromSource, pattern, enumExpr, bodyExpr, range) -> bodyExpr |> SynExpr.GetAstItems
    | SynExpr.ArrayOrListOfSeqExpr(isList, elements, range) -> Seq.empty
    | SynExpr.CompExpr(isArrayOrList, isNotNakedRefCell, expr, range) -> expr |> SynExpr.GetAstItems
    | SynExpr.Lambda(fromMethod, inLambdaSeq, args, body, range) as x -> Seq.singleton (Lambda(x, body |> SynExpr.GetAstItems))
    | SynExpr.MatchLambda(_, _, clauses, spBind, range) -> Seq.empty
    | SynExpr.Match(spBind, matchExpr, clauses, isCexprExceptionMatch, range) -> Seq.empty
    | SynExpr.Do(expr, range) -> expr |> SynExpr.GetAstItems
    | SynExpr.Assert(expr, range) -> Seq.empty
    | SynExpr.App(exprAtomicFlag, isInfix, funcExpr, argExpr, range) -> funcExpr |> SynExpr.GetAstItems
    | SynExpr.TypeApp(expr, leftAngleRange, typeNames, commaRanges, rightAngleRange, typeArgs, range) -> Seq.empty
    | SynExpr.TryWith(tryExpr, _, _, _, range, spTry, spWith) ->  Seq.empty
    | SynExpr.TryFinally(tryExpr, finallyExpr, range, spTry, spFinally) ->  Seq.empty
    | SynExpr.Lazy(expr, range) ->  Seq.empty
    | SynExpr.Sequential(spSeq, isTrueSeq, expr1, expr2, range) ->  Seq.empty
    | SynExpr.IfThenElse(exprGuard, exprThen, optionalExprElse, spIfToThen, isFromErrorRecovery, ifToThen, range) -> 
      exprThen |> SynExpr.GetAstItems
      |> Seq.append (optionalExprElse |> function | None -> Seq.empty | Some elseExpr -> elseExpr |> SynExpr.GetAstItems)
    | SynExpr.Ident(_) ->  Seq.empty
    | SynExpr.LongIdent(isOptional, longIdent, altNameRefCell, range) ->  Seq.empty
    | SynExpr.LongIdentSet(dotId, expr, range) ->  Seq.empty
    | SynExpr.DotGet(expr, rangeOfDot, dotId, range) ->  Seq.empty
    | SynExpr.DotSet(expr, dotId, exprValue, range) ->  Seq.empty
    | SynExpr.DotIndexedGet(expr, indexExprs, _, range) ->  Seq.empty
    | SynExpr.DotIndexedSet(objectExpr, indexExprs, valueExpr, rangeOfLeftOfSet, rangeOfDot, range) -> Seq.empty
    | SynExpr.NamedIndexedPropertySet(_, _, _, range) ->  Seq.empty
    | SynExpr.DotNamedIndexedPropertySet(_, _, _, _, range) ->  Seq.empty
    | SynExpr.TypeTest(expr, typeName, range) ->  Seq.empty
    | SynExpr.Upcast(expr, typeSig, range) ->  Seq.empty
    | SynExpr.Downcast(expr, typeName, range) ->  Seq.empty
    | SynExpr.InferredUpcast(expr, range) ->  Seq.empty
    | SynExpr.InferredDowncast(expr, range) ->  Seq.empty
    | SynExpr.Null(range) ->  Seq.empty
    | SynExpr.AddressOf(_, _, _, range) ->  Seq.empty
    | SynExpr.TraitCall(_, _, _, range) ->  Seq.empty
    | SynExpr.JoinIn(_, inPos, _, range) ->  Seq.empty
    | SynExpr.ImplicitZero(range) ->  Seq.empty
    | SynExpr.YieldOrReturn(_, expr, range) ->  Seq.empty
    | SynExpr.YieldOrReturnFrom(_, expr, range) ->  Seq.empty
    | SynExpr.LetOrUseBang(spBind, isUse, isFromSource, pattern, rhsExpr, bodyExpr, range) ->
          bodyExpr |> SynExpr.GetAstItems
    | SynExpr.DoBang(expr, range) ->   expr |> SynExpr.GetAstItems
    | SynExpr.LibraryOnlyILAssembly(_, _, _, _, range) ->  Seq.empty
    | SynExpr.LibraryOnlyStaticOptimization(_, _, _, range) ->  Seq.empty
    | SynExpr.LibraryOnlyUnionCaseFieldGet(_, longId, _, range) ->  Seq.empty
    | SynExpr.LibraryOnlyUnionCaseFieldSet(_, longId, _, _, range) ->  Seq.empty
    | SynExpr.ArbitraryAfterError(debugStr, range) ->  Seq.empty
    | SynExpr.FromParseError(expr, range) ->  Seq.empty
    | SynExpr.DiscardAfterMissingQualificationAfterDot(expr, range) ->  Seq.empty
    | SynExpr.Fixed(expr, _) -> expr |> SynExpr.GetAstItems
and SynBinding with
  static member GetAstItems =
    function
    | SynBinding.Binding(_,_,_,_,_,_,_,_,_,expr,_,_) as x -> 
      Seq.singleton (Let (x, Seq.empty))
      |> Seq.append (expr |> SynExpr.GetAstItems)

and SynTypeDefn with
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
    | SynModuleDecl.ModuleAbbrev(_,_,_) -> Seq.empty
    | SynModuleDecl.NestedModule(SynComponentInfo.ComponentInfo(_,_,_,ident,_,_,_,_), isRec, decls, _, _) as x ->
        let children =
          decls |> mapConcat SynModuleDecl.GetAstItems
        Seq.singleton (ModuleDecl (ident,decls, children))
    | SynModuleDecl.Let(_, bindings, _) -> bindings |> mapConcat SynBinding.GetAstItems
    | SynModuleDecl.DoExpr(_, _, _) -> Seq.empty
    | SynModuleDecl.Types(types, _) -> types |> mapConcat SynTypeDefn.GetAstItems
    | SynModuleDecl.Exception(_, _) -> Seq.empty
    | SynModuleDecl.Open(_, _) -> Seq.empty
    | SynModuleDecl.Attributes(_, _) -> Seq.empty
    | SynModuleDecl.HashDirective(_, _) ->  Seq.empty
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

(*
let printPos (p: pos) = sprintf "pos(%i,%i)" p.Line p.Column
let printRange (r: range) = sprintf "range(%s,%s)" (printPos r.Start) (printPos r.End)
fsi.AddPrinter printPos
fsi.AddPrinter printRange
fsi.PrintWidth <- 120*)