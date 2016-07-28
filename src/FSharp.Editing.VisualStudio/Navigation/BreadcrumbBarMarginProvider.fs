namespace FSharp.Editing.VisualStudio.Navigation

open System.ComponentModel.Composition;
open Microsoft.VisualStudio.Text.Editor;
open Microsoft.VisualStudio.Utilities;

[<Export(typeof<IWpfTextViewMarginProvider>)>]
[<Name(FSharp.Editing.VisualStudio.Constants.BreadcrumbBarMarginName)>]
[<MarginContainer(PredefinedMarginNames.Top)>]
[<ContentType("F#")>]
type BreadcrumbMarginProvider() =
    interface IWpfTextViewMarginProvider with
        member x.CreateMargin(wpfTextViewHost, _) =
            new BreadcrumbBarMargin(wpfTextViewHost.TextView) :> _
