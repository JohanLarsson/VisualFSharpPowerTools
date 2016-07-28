namespace FSharp.Editing.VisualStudio

open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Media

module LeftMargin = 
    open Microsoft.VisualStudio.Text.Editor

    type internal Marker = interface end

    let PaddingProperty = DependencyProperty.RegisterAttached("Padding", typeof<Thickness>, typeof<Marker>.DeclaringType, PropertyMetadata(Thickness(0., 0., 0., 0.)))
    
    let SetPadding (element: UserControl, value : Thickness) = 
        element.SetValue(PaddingProperty, value)
    
    let GetPadding (element: UserControl) : Thickness = 
        element.GetValue(PaddingProperty) :?> _
    
    let tryBindLeftPadding (view:IWpfTextView) (visual:FrameworkElement)=
        // Climbing the visual tree here, slight hack.
        // Potential for improvement but probably not an issue.
        let parent = if (obj.ReferenceEquals (view, null)) then 
                        null 
                     else 
                        VisualTreeHelper.GetParent view.VisualElement

        let leftMargin =
            if (parent :? Grid) then 
                let grid = parent :?> Grid
                grid.Children
                |> FSharp.Editing.Seq.ofType<IWpfTextViewMargin>
                |> Seq.tryFind (fun m -> m.GetType().Name = "LeftMargin") // LeftMargin is internal I think
            else
                None

        let actualWidthToLeftPaddingConverter = 
            { new IValueConverter with 
                member x.Convert(value, _, _, _) =
                    let left =
                        match value with
                        | :? float as d -> d
                        | _ -> 0.0
                    Thickness(left, 0., 0., 0.) |> box
                member x.ConvertBack(_, _, _, _) =
                    failwithf "Only for one way bindings"
             }

        let bindLeftPadding (leftMargin: IWpfTextViewMargin) =
            let binding = Binding("ActualWidth") 
            binding.Source <- leftMargin.VisualElement
            binding.Mode <- BindingMode.OneWay
            binding.Converter <- actualWidthToLeftPaddingConverter
            BindingOperations.SetBinding(visual, PaddingProperty, binding)

        match leftMargin with
            | Some x -> Some(bindLeftPadding x)
            | None -> None

