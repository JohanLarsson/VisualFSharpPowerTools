namespace FSharp.Editing.VisualStudio.Navigation
open System
open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Media

open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text

open FSharp.Editing
open FSharp.Editing.VisualStudio

type BreadcrumbBarVisual = FsXaml.XAML< @"Gui/BreadcrumbBar.xaml">

type BreadcrumbBarMargin(view:IWpfTextView) =
    let visual = BreadcrumbBarVisual()

    do
        // setting up some dummy data, remove this
        visual.DataContext <- [
                                "module foo "
                                "let bar"
                              ]
    do 
       LeftMargin.tryBindLeftPadding view visual |> ignore

    interface IWpfTextViewMargin with
        member __.VisualElement = upcast visual
        member __.MarginSize = visual.ActualHeight
        member __.Enabled = true

        member x.GetTextViewMargin name =
            match name with
            | Constants.BreadcrumbBarMarginName -> upcast x
            | _ -> Unchecked.defaultof<_>

    interface IDisposable with
        member __.Dispose() = ()

