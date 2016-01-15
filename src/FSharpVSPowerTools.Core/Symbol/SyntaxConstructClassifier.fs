﻿namespace FSharpVSPowerTools.SyntaxColoring

open System
open System.IO
open System.Threading
open FSharpVSPowerTools
open FSharpVSPowerTools.SourceCodeClassifier
//open FSharpVSPowerTools.ProjectSystem
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpVSPowerTools.AsyncMaybe
open FSharpVSPowerTools.UntypedAstUtils
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler
open System.Diagnostics

[<NoComparison>]
type private CheckingProject =
    { Options: FSharpProjectOptions
      Checked: bool }

//[<NoComparison>]
//type private SnapshotSpanWithLine =
//    { Span: SnapshotSpan 
//      Line: int }
//
//[<Sealed>]
//type private CategorizedSnapshotSpan (columnSpan: CategorizedColumnSpan<ITextSnapshot>, originalSnapshot: ITextSnapshot) =
//    let snapshotSpan: SnapshotSpanWithLine option Atom = Atom None 
//    member __.ColumnSpan = columnSpan
//    member __.GetSnapshotSpan targetSnapshot = 
//        snapshotSpan.Swap (fun oldSpan ->
//            oldSpan
//            |> Option.orTry (fun _ -> 
//                fromRange originalSnapshot (columnSpan.WordSpan.ToRange())
//                |> Option.map (fun span -> 
//                    { Span = span
//                      Line = span.Start.GetContainingLine().LineNumber }))
//            |> Option.map (fun span ->
//                if span.Span.Snapshot <> targetSnapshot then
//                    let newSpan = span.Span.TranslateTo(targetSnapshot, SpanTrackingMode.EdgeExclusive)
//                    { Span = newSpan; Line = newSpan.Start.GetContainingLine().LineNumber }
//                else span)) 
//        |> ignore
//        snapshotSpan.Value

//[<NoComparison>]
//type private CategorizedSnapshotSpans =
//    { Spans: CategorizedSnapshotSpan[]
//      Errors: FSharpErrorInfo[] }
//
//[<NoComparison>]
//type private FastStageData =
//    { Snapshot: ITextSnapshot
//      Spans: CategorizedSnapshotSpans
//      SingleSymbolsProjects: CheckingProject list } 
//
//[<NoComparison>]
//type private FastStage =
//    | NoData
//    | Updating of oldData:FastStageData option * currentSnapshot: ITextSnapshot
//    | Data of FastStageData
//
//[<NoComparison>]
//type private SlowStageData =
//    { Snapshot: ITextSnapshot
//      UnusedSpans: Map<WordSpan, CategorizedSnapshotSpan>
//      IsUpdating: bool }

[<NoComparison>]
type private SlowStage =
    | NoData of isUpdating: bool
//    | Data of SlowStageData

//type UnusedDeclarationTag() =
//    interface ITag

//type SyntaxConstructClassifier
//    (
////        doc: ITextDocument,
////        buffer: ITextBuffer,
////        classificationRegistry: IClassificationTypeRegistryService,
////        vsLanguageService: VSLanguageService,
////        serviceProvider: IServiceProvider,
////        projectFactory: ProjectFactory,
//        includeUnusedReferences: bool,
//        includeUnusedOpens: bool
//    ) as self =


module Classifier =

    //let typeName = self.GetType().Name
 //   let log (f: unit -> string) = Logging.logInfo (fun _ -> "[" + typeName + "] " + f()) 
//    let debug msg = Printf.kprintf (fun x -> Debug.WriteLine ("[" + typeName + "] " + x)) msg

    let getClassificationType = memoize <| fun cat ->
        match cat with
        | Category.ReferenceType -> Some SyntaxConstants.fsharpReferenceType
        | Category.ValueType -> Some SyntaxConstants.fsharpValueType
        | Category.PatternCase -> Some SyntaxConstants.fsharpPatternCase
        | Category.Function -> Some SyntaxConstants.fsharpFunction
        | Category.MutableVar -> Some SyntaxConstants.fsharpMutableVar
        | Category.Quotation -> Some SyntaxConstants.fsharpQuotation
        | Category.Module -> Some SyntaxConstants.fsharpModule
        | Category.Unused -> Some SyntaxConstants.fsharpUnused
        | Category.Printf -> Some SyntaxConstants.fsharpPrintf
        | Category.Escaped -> Some SyntaxConstants.fsharpEscaped
        | Category.Operator -> Some SyntaxConstants.fsharpOperator
        | _ -> None
//        |> Option.map classificationRegistry.GetClassificationType

//    let classificationChanged = Event<_>()
////    let fastState = Atom FastStage.NoData
//    let slowState = Atom (SlowStage.NoData false)
//    let slowStageCancellationToken = Atom None
//    let unusedDeclarationChanged = Event<_>()
//    let unusedDeclarationState = Atom None
////
////    let disposeCancellationToken (currentToken: Atom<CancellationTokenSource option>) =
////        currentToken.Value
////        |> Option.iter (fun token ->
////            token.Cancel()
////            token.Dispose())
////
////    let dte = serviceProvider.GetDte()
////
////    let getCurrentProject() =
////        maybe {
////            // If there is no backing document, an ITextDocument instance might be null
////            let! _ = Option.ofNull doc
////            let! item = dte.GetProjectItem doc.FilePath
////            return! projectFactory.CreateForProjectItem buffer doc.FilePath item }
////
////    let isCurrentProjectForStandaloneScript() =
////        getCurrentProject() |> Option.map (fun p -> p.IsForStandaloneScript) |> Option.getOrElse false
////
////    let includeUnusedOpens() =
////        includeUnusedOpens
////        // Don't check for unused opens on generated signatures
////        && not (isSignatureExtension(Path.GetExtension doc.FilePath)
////                && isCurrentProjectForStandaloneScript())
////
////    let includeUnusedReferences() =
////        includeUnusedReferences
////        // Don't check for unused declarations on generated signatures
////        && not (isSignatureExtension(Path.GetExtension doc.FilePath)
////                && isCurrentProjectForStandaloneScript())
////
////    let isSlowStageEnabled() = includeUnusedOpens() || includeUnusedReferences()
////    let getCurrentSnapshot() =
////        maybe {
////            let! doc = Option.ofNull doc
////            let! buffer = Option.ofNull doc.TextBuffer
////            return buffer.CurrentSnapshot }
////
////    let triggerClassificationChanged snapshot reason =
////        let span = SnapshotSpan(snapshot, 0, snapshot.Length)
////        classificationChanged.Trigger(self, ClassificationChangedEventArgs span)
////        debug "ClassificationChanged event has been triggered by %s" reason
////
////    let triggerUnusedDeclarationChanged snapshot =
////        let span = SnapshotSpan(snapshot, 0, snapshot.Length)
////        unusedDeclarationChanged.Trigger(self, SnapshotSpanEventArgs span)
////
////    let getOpenDeclarations filePath project ast getTextLineOneBased (pf: Profiler) = 
////        async {
////            let! entities = pf.TimeAsync "GetAllEntities" <| fun _ ->
////                vsLanguageService.GetAllEntities(filePath, project)
////            
////            return! pf.TimeAsync "getOpenDeclarations" <| fun _ -> 
////                async {
////                  let qualifyOpenDeclarations line endCol idents = async {
////                      let lineStr = getTextLineOneBased (line - 1)
////                      let! tooltip =
////                          vsLanguageService.GetOpenDeclarationTooltip(
////                              line, endCol, lineStr, Array.toList idents, project, doc.FilePath)
////                      return
////                          match tooltip with
////                          | Some tooltip -> OpenDeclarationGetter.parseTooltip tooltip
////                          | None -> []
////                  }
////                  
////                  let! openDecls = OpenDeclarationGetter.getOpenDeclarations ast entities qualifyOpenDeclarations
////                  return
////                      (entities
////                       |> Option.map
////                           (Seq.groupBy (fun e -> e.FullName)
////                            >> Seq.map (fun (key, es) -> key, es |> Seq.map (fun e -> e.CleanedIdents) |> Seq.toList)
////                            >> Dict.ofSeq),
////                      openDecls)
////                }
////        }
////
////    let uiContext = SynchronizationContext.Current
////
////    let checkAst message (ast: ParsedInput) =
////        if ast.Range.IsEmpty then
////            debug "%s Empty AST" message
////            None
////        else Some()

//    let isTypeCheckerCategory = function
//        | Category.ReferenceType
//        | Category.ValueType
//        | Category.PatternCase
//        | Category.Function
//        | Category.MutableVar
//        | Category.Module -> true
//        | Category.Quotation
//        | Category.Unused
//        | Category.Printf
//        | Category.Escaped
//        | Category.Operator
//        | Category.Other -> false

//    let mergeSpans (oldSpans: CategorizedSnapshotSpans) (newSpans: CategorizedSnapshotSpans) =
//        let getLineRange includingUnused (spans: CategorizedSnapshotSpan[]) =
//            let typeCheckerSpans = 
//                spans 
//                |> Array.filter (fun x -> 
//                    isTypeCheckerCategory x.ColumnSpan.Category
//                    || (includingUnused && x.ColumnSpan.Category = Category.Unused))
//            typeCheckerSpans,
//            typeCheckerSpans
//            |> Array.map (fun x -> x.ColumnSpan.WordSpan.Line)
//            |> function [||] -> -1, -1 | lines -> Array.min lines, Array.max lines
//
//        // we take into account new Unused spans, but do not old ones.
//        let newTcSpans, (newStartLine, newEndLine) = getLineRange true newSpans.Spans
//        let oldTcSpans, (oldStartLine, oldEndLine) = getLineRange false oldSpans.Spans
//        let isNewRangeLarger = newStartLine <= oldStartLine && newEndLine >= oldEndLine
//
//        // returns `true` if both first and last spans are still here, which means
//        // that new spans are not produced from partially valid source file.
//        let haveFirstAndLastSpansNotChanged() =
//            let sameWordSpan x y =
//                x.SymbolKind = y.SymbolKind
//                && x.StartCol = y.StartCol
//                && x.EndCol = y.EndCol
//            match newTcSpans, oldTcSpans with
//            | [||], [||] -> true
//            | _, [||] | [||], _ -> false
//            | x, y ->
//                sameWordSpan x.[0].ColumnSpan.WordSpan y.[0].ColumnSpan.WordSpan
//                && sameWordSpan x.[x.Length - 1].ColumnSpan.WordSpan y.[y.Length - 1].ColumnSpan.WordSpan
//        
//        match newSpans.Errors with
//        | [||] ->
//            log (fun _ -> "Replace spans entirely because new spans has no errors.")
//            newSpans
//        | _ ->
//            log (fun _ -> sprintf "FCS returns errors:\n %+A" newSpans.Errors)
//
//            if isNewRangeLarger then
//                log (fun _ -> 
//                    sprintf "Replace spans entirely because new span range is wider than old one (old lines = %d..%d, new lines = %d..%d)." 
//                            oldStartLine oldEndLine newStartLine newEndLine)
//                newSpans
//            elif haveFirstAndLastSpansNotChanged() then
//                log (fun _ -> "Replace spans entirely because first and last spans have not changed.")
//                newSpans
//            else
//                log (fun _ -> sprintf "Merging spans (new range %A <= old range %A)." 
//                                      (newStartLine, newEndLine) (oldStartLine, oldEndLine))
//                let spans = 
//                    seq { 
//                        yield! oldSpans.Spans |> Seq.takeWhile (fun x -> x.ColumnSpan.WordSpan.Line < newStartLine)
//                        yield! oldSpans.Spans |> Seq.skipWhile (fun x -> x.ColumnSpan.WordSpan.Line <= newEndLine) 
//                        yield! newSpans.Spans
//                    }
//                    |> Seq.sortBy (fun x -> x.ColumnSpan.WordSpan.Line)
//                    |> Seq.toArray
//                { Spans = spans; Errors = newSpans.Errors }
//            
//    let updateUnusedDeclarations (CallInUIContext callInUIContext) =
//        let worker (project, snapshot) =
//            asyncMaybe {
//                let pf = Profiler()
//                debug "UpdateUnusedDeclarations"
//
//                let! symbolsUses = pf.TimeAsync "GetAllUsesOfAllSymbolsInFile" <| fun _ ->
//                    vsLanguageService.GetAllUsesOfAllSymbolsInFile(
//                        snapshot, doc.FilePath, project, AllowStaleResults.No, includeUnusedOpens(), pf)
//
//                let getSymbolDeclLocation fsSymbol = projectFactory.GetSymbolDeclarationLocation fsSymbol doc.FilePath project
//
//                let! symbolsUses =
//                    if includeUnusedReferences() then
//                        vsLanguageService.GetUnusedDeclarations(symbolsUses, project, getSymbolDeclLocation, pf)
//                    else async { return symbolsUses }
//                    |> liftAsync
//
//                let! lexer = vsLanguageService.CreateLexer(doc.FilePath, snapshot, project.CompilerOptions)
//                let getTextLineOneBased i = snapshot.GetLineFromLineNumber(i).GetText()
//
//                let! checkResults = pf.Time "ParseAndCheckFileInProject" <| fun _ ->
//                    vsLanguageService.ParseAndCheckFileInProject(doc.FilePath, project)
//                     
//                let! ast = checkResults.ParseTree
//                do! checkAst "Slow stage" ast
//
//                let! entities, openDecls =
//                    if includeUnusedOpens() then
//                        getOpenDeclarations doc.FilePath project checkResults.ParseTree getTextLineOneBased pf
//                    else async { return None, [] }
//                    |> liftAsync
//
//                let spans = pf.Time "getCategoriesAndLocations" <| fun _ ->
//                    getCategoriesAndLocations (symbolsUses, checkResults, lexer, getTextLineOneBased, openDecls, entities)
//                    |> Array.sortBy (fun x -> x.WordSpan.Line)
//                    |> Array.map (fun x -> CategorizedSnapshotSpan (x, snapshot))
//
//                let notUsedSpans =
//                    spans
//                    |> Array.filterMap
//                        (fun x -> x.ColumnSpan.Category = Category.Unused)
//                        (fun x -> x.ColumnSpan.WordSpan, x)
//                    |> Map.ofArray
//
//                let spans = { Spans = spans; Errors = checkResults.Errors }
//
//                fastState.Swap (function
//                    | FastStage.Data data ->
//                        FastStage.Data { data with Snapshot = snapshot
//                                                   Spans = mergeSpans data.Spans spans
//                                                   SingleSymbolsProjects = [] }
//                    | state -> state)
//                    |> ignore
//
//                debug "UpdateUnusedDeclarations: fastState swapped"
//                slowState.Swap (fun _ -> SlowStage.Data { Snapshot = snapshot; UnusedSpans = notUsedSpans; IsUpdating = false }) |> ignore
//                debug "UpdateUnusedDeclarations: slowState swapped"
//                pf.Stop()
//                log (fun _ -> sprintf "[Unused symbols and opens stage] %O" pf.Elapsed)
//                do! callInUIContext <| fun _ -> triggerClassificationChanged snapshot "UpdateUnusedDeclarations" 
//                    |> liftAsync
//
//                // Switch back to UI thread before firing events
//                do! Async.SwitchToContext(uiContext) |> liftAsync
//                unusedDeclarationState.Swap(fun _ -> Some (snapshot, notUsedSpans |> Map.toArray |> Array.map fst)) |> ignore
//                triggerUnusedDeclarationChanged snapshot
//            } |> Async.Ignore
//
//        match getCurrentProject(), getCurrentSnapshot() with
//        | Some project, Some snapshot ->
//            match fastState.Value, slowState.Value with
//            | (FastStage.NoData | FastStage.Updating _), _ -> async.Return()
//            | _, SlowStage.NoData (isUpdating = true) -> async.Return()
//            | FastStage.Data _, slowStage ->
//                match slowStage with
//                | SlowStage.Data { IsUpdating = true } -> async.Return()
//                | SlowStage.Data { Snapshot = oldSnapshot } when oldSnapshot = snapshot -> async.Return()
//                | SlowStage.NoData (isUpdating = true) -> async.Return()
//                | _ ->
//                    slowState.Swap (function
//                        | SlowStage.Data data -> SlowStage.Data { data with IsUpdating = true }
//                        | SlowStage.NoData _ -> SlowStage.NoData true) |> ignore
//                    async {
//                        try do! worker (project, snapshot)
//                        finally
//                            // no matter what's happend in `worker`, we should reset `IsUpdating` flag to `false`
//                            // in order to prevent Slow stage to stop working as it would think that a previous 
//                            // `worker` is still running.
//                            slowState.Swap (function
//                               | SlowStage.NoData _ -> SlowStage.NoData false
//                               | SlowStage.Data x -> SlowStage.Data { x with IsUpdating = false })
//                            |> ignore
//                    }
//        | _ -> async.Return()
//
//    let updateSyntaxConstructClassifiers force ((CallInUIContext callInUIContext) as ciuc) = 
//        let snapshot = getCurrentSnapshot()
//        let needUpdate =
//            match snapshot, force, fastState.Value with
//            | None, _, _ -> false
//            | _, true, _ -> true
//            | _, _, FastStage.NoData -> true
//            | Some snapshot, _, FastStage.Updating (_, oldSnapshot) -> oldSnapshot <> snapshot
//            | Some snapshot, _, FastStage.Data { Snapshot = oldSnapshot } -> oldSnapshot <> snapshot
//
//        snapshot |> Option.iter (fun snapshot ->
//            fastState.Swap (fun oldState ->
//                let oldData =
//                    match oldState with
//                    | FastStage.Data data -> Some data
//                    | FastStage.Updating (data, _) -> data
//                    | _ -> None
//                Updating (oldData, snapshot)) |> ignore)
//
//        if needUpdate then
//            asyncMaybe {
//                let! currentProject = getCurrentProject()
//                let! snapshot = snapshot
//                debug "Effective update"
//                let pf = Profiler()
//
//                let! checkResults = pf.TimeAsync "ParseFileInProject" <| fun _ ->
//                    vsLanguageService.ParseAndCheckFileInProject(doc.FilePath, currentProject)
//
//                let! ast = checkResults.ParseTree
//                do! checkAst "Fast stage" ast
//                let! lexer = vsLanguageService.CreateLexer(doc.FilePath, snapshot, currentProject.CompilerOptions)
//
//                let! allSymbolsUses =
//                    vsLanguageService.GetAllUsesOfAllSymbolsInFile(
//                        snapshot, doc.FilePath, currentProject, AllowStaleResults.No, false, pf)
//
//                let getTextLineOneBased i = snapshot.GetLineFromLineNumber(i).GetText()
//
//                let spans = pf.Time "getCategoriesAndLocations" <| fun _ ->
//                    getCategoriesAndLocations (allSymbolsUses, checkResults, lexer, getTextLineOneBased, [], None)
//                    |> Array.sortBy (fun x -> x.WordSpan.Line)
//                    |> Array.map (fun x -> CategorizedSnapshotSpan (x, snapshot))
//
//                let spans =
//                    match slowState.Value with
//                    | SlowStage.Data { UnusedSpans = oldUnusedSpans } ->
//                        spans
//                        |> Array.filter (fun s ->
//                            not (oldUnusedSpans |> Map.containsKey s.ColumnSpan.WordSpan))
//                        |> Array.append (oldUnusedSpans |> Map.toArray |> Array.map snd)
//                        |> Array.sortBy (fun x -> x.ColumnSpan.WordSpan.Line)
//                    | _ -> spans
//
//                let spans = { Spans = spans; Errors = checkResults.Errors }
//
//                let! singleSymbolsProjects =
//                    async {
//                        if includeUnusedReferences() then
//                            let getSymbolDeclLocation fsSymbol = projectFactory.GetSymbolDeclarationLocation fsSymbol doc.FilePath currentProject
//                            let singleDefs = UnusedDeclarations.getSingleDeclarations allSymbolsUses
//                            return!
//                                singleDefs
//                                |> Async.Array.map (fun symbol ->
//                                     vsLanguageService.GetSymbolDeclProjects getSymbolDeclLocation currentProject symbol)
//                                |> Async.map (
//                                       Array.choose id
//                                    >> Array.concat
//                                    >> Array.distinct
//                                    >> Array.map (fun opts ->
//                                        { Options = opts
//                                          // we mark standalone FSX's fake project as already checked
//                                          // because otherwise the slow stage never completes
//                                          Checked = currentProject.IsForStandaloneScript })
//                                    >> Array.toList)
//                        else return [] } |> liftAsync
//
//                fastState.Swap (fun oldState ->
//                    let spans =
//                        match oldState with
//                        | FastStage.Data oldData
//                        | FastStage.Updating (Some oldData, _) -> mergeSpans oldData.Spans spans
//                        | _ -> spans
//
//                    FastStage.Data
//                        { Snapshot = snapshot
//                          Spans = spans
//                          SingleSymbolsProjects = singleSymbolsProjects }) |> ignore
//
//                do! callInUIContext <| fun _ -> triggerClassificationChanged snapshot "UpdateSyntaxConstructClassifiers" 
//                    |> liftAsync
//
//                if isSlowStageEnabled() then
//                    if currentProject.IsForStandaloneScript || not (includeUnusedReferences()) then
//                        do! updateUnusedDeclarations ciuc |> liftAsync
//                    else
//                        let! currentProjectOpts = vsLanguageService.GetProjectCheckerOptions currentProject |> liftAsync
//                        vsLanguageService.CheckProjectInBackground currentProjectOpts
//
//                pf.Stop()
//                log (fun _ -> sprintf "[Normal stage] %O elapsed" pf.Elapsed)
//            } |> Async.Ignore
//        else async.Return ()

//    let events: EnvDTE80.Events2 option = tryCast dte.Events
//    let onBuildDoneHandler = EnvDTE._dispBuildEvents_OnBuildProjConfigDoneEventHandler (fun project _ _ _ _ ->
//        maybe {
//            let! selfProject = getCurrentProject()
//            let builtProjectFileName = Path.GetFileName project
//            let referencedProjectFileNames = selfProject.GetAllReferencedProjectFileNames()
//            if referencedProjectFileNames |> List.exists ((=) builtProjectFileName) then
//                debug "Referenced project %s has been built, updating classifiers..." builtProjectFileName
//                let callInUIContext = CallInUIContext.FromCurrentThread()
//                updateSyntaxConstructClassifiers true callInUIContext |> Async.StartInThreadPoolSafe
//        } |> ignore)
//
//    do events |> Option.iter (fun e -> e.BuildEvents.add_OnBuildProjConfigDone onBuildDoneHandler)
//
//    let docEventListener =
//        new DocumentEventListener ([ViewChange.bufferEvent doc.TextBuffer], 200us, updateSyntaxConstructClassifiers false)
//
//    let projectCheckedSubscription =
//        // project check results needed for Unused Declarations only.
//        if includeUnusedReferences() then
//            Some (vsLanguageService.RawChecker.ProjectChecked.Subscribe (fun projectFileName ->
//                match isSlowStageEnabled(), fastState.Value with
//                | true, FastStage.Data ({ SingleSymbolsProjects = projects } as fastData) ->
//                    let projects =
//                        match projects |> List.partition (fun p -> p.Options.ProjectFileName = projectFileName) with
//                        | [], rest -> rest
//                        | matched, rest ->
//                            (matched |> List.map (fun p -> { p with Checked = true })) @ rest
//                    fastState.Swap (fun _ -> FastStage.Data { fastData with SingleSymbolsProjects = projects }) |> ignore
//
//                    match projects |> List.tryFind (fun p -> not p.Checked) with
//                    | Some { Options = opts } ->
//                        // there is at least one yet unchecked project, start compilation on it
//                        vsLanguageService.CheckProjectInBackground opts
//                    | None ->
//                        // all the needed projects have been checked in background, let's calculate
//                        // Slow Stage (unused symbols and opens)
//                        let ctx = CallInUIContext.FromCurrentThread()
//                        updateUnusedDeclarations ctx |> Async.StartInThreadPoolSafe
//                | _ -> ()))
//        else None
//
//    let getClassificationSpans (targetSnapshotSpan: SnapshotSpan) =
//        match fastState.Value with
//        | FastStage.Data { FastStageData.Spans = spans }
//        | FastStage.Updating (Some { FastStageData.Spans = spans }, _) ->
//            let spanStartLine = targetSnapshotSpan.Start.GetContainingLine().LineNumber
//            let widenSpanStartLine = max 0 (spanStartLine - 10)
//            let spanEndLine = targetSnapshotSpan.End.GetContainingLine().LineNumber
//            spans.Spans
//            // Locations are sorted, so we can safely filter them efficiently.
//            // Skip spans that's not are potential candidates for return (we widen the range 
//            // because spans may shift to up after translation).
//            |> Seq.skipWhile (fun span -> span.ColumnSpan.WordSpan.Line < widenSpanStartLine)
//            |> Seq.choose (fun snapshotSpan ->
//                maybe {
//                    let! clType = getClassificationType snapshotSpan.ColumnSpan.Category
//                    let! span = snapshotSpan.GetSnapshotSpan targetSnapshotSpan.Snapshot
//                    return clType, span
//                })
//            |> Seq.takeWhile (fun (_, span) -> span.Line <= spanEndLine)
//            // Because we may translate spans above, some of them may not be contained in the requested `SnapshotSpan`.
//            |> Seq.filter (fun (_, span) -> targetSnapshotSpan.Contains span.Span)
//            |> Seq.map (fun (clType, span) -> ClassificationSpan (span.Span, clType))
//            |> Seq.toArray
//        | FastStage.NoData ->
//            // Only schedule an update on signature files
//            if isSignatureExtension(Path.GetExtension(doc.FilePath)) then
//                // If not yet schedule an action, do it now.
//                let callInUIContext = CallInUIContext.FromCurrentThread()
//                updateSyntaxConstructClassifiers false callInUIContext |> Async.StartInThreadPoolSafe
//            [||]
//        | FastStage.Updating _ -> [||]
//
//    interface IClassifier with
//        // It's called for each visible line of code
//        member __.GetClassificationSpans span =
//            upcast (protectOrDefault (fun _ -> getClassificationSpans span) [||])
//
//        [<CLIEvent>]
//        member __.ClassificationChanged = classificationChanged.Publish
//
//    interface ITagger<UnusedDeclarationTag> with
//        member __.GetTags spans =
//            let getTags (_spans: NormalizedSnapshotSpanCollection) =
//                unusedDeclarationState.Value
//                |> Option.map (fun (snapshot, data) ->
//                    data
//                    |> Array.choose (fun wordSpan ->
//                        fromRange snapshot (wordSpan.ToRange())
//                        |> Option.map (fun span -> TagSpan(span, UnusedDeclarationTag()) :> ITagSpan<_>)))
//                |> Option.getOrElse [||]
//            protectOrDefault (fun _ -> getTags spans :> _) Seq.empty
//
//        [<CLIEvent>]
//        member __.TagsChanged = unusedDeclarationChanged.Publish
//
//    interface IDisposable with
//        member __.Dispose() =
//            projectCheckedSubscription |> Option.iter (fun sub -> sub.Dispose())
//            events |> Option.iter (fun e -> e.BuildEvents.remove_OnBuildProjConfigDone onBuildDoneHandler)
//            disposeCancellationToken slowStageCancellationToken
//            (docEventListener :> IDisposable).Dispose()
