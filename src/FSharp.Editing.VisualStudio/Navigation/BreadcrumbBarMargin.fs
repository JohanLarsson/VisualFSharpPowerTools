namespace FSharp.Editing.VisualStudio.Navigation
open System
open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Media

open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text

open FSharp.Editing.VisualStudio

type BreadcrumbBarVisual = FsXaml.XAML< @"Gui/BreadcrumbBar.xaml">

type BreadcrumbBarMargin(view: IWpfTextView) =
    let visual = BreadcrumbBarVisual()
    let tryBindLeftMargin = 
        let parent = VisualTreeHelper.GetParent view.VisualElement 
        let leftMargin =
            if (parent :? Grid) then 
                let grid = parent :?> Grid
                grid.Children
                |> Seq.cast<IWpfTextViewMargin>
                |> Seq.tryFind (fun m -> m.GetType().Name = "LeftMargin")
            else
                None

        if (leftMargin != null) then
            let binding = Binding("ActualWidth") 
            binding.Source <- leftMargin.VisualElement
            binding.Mode <- BindingMode.OneWay
            binding.Converter <- ActualWidthToLeftPaddingConverter
            BindingOperations.SetBinding this, LeftPaddingProperty, binding
    tryBindLeftMargin

    static let LeftPaddingProperty = DependencyProperty.RegisterAttached("LeftPadding", typeof<Thickness>, typeof<BreadcrumbBarMargin>, PropertyMetadata(Thickness(0.0, 0.0, 0.0, 0.0)))

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