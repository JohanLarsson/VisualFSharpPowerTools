﻿using System.Diagnostics;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Shell;
using EnvDTE;
using System;
using FSharp.Editing.VisualStudio.Symbol;
using FSharp.Editing.VisualStudio.ProjectSystem;
using FSharp.Editing.VisualStudio;

namespace FSharpVSPowerTools
{
    [Export(typeof(IViewTaggerProvider))]
    [ContentType("F#")]
    [TagType(typeof(HighlightUsageTag))]
    [TextViewRole(PredefinedTextViewRoles.Interactive)]
    public class HighlightUsageTaggerProvider : IViewTaggerProvider
    {
        readonly IServiceProvider _serviceProvider;
        readonly ITextDocumentFactoryService _textDocumentFactoryService;
        readonly ProjectFactory _projectFactory;
        readonly VSLanguageService _fsharpVsLanguageService;

        [ImportingConstructor]
        public HighlightUsageTaggerProvider(
            [Import(typeof(SVsServiceProvider))] IServiceProvider serviceProvider,
            ITextDocumentFactoryService textDocumentFactoryService,
            ProjectFactory projectFactory,
            VSLanguageService fsharpVsLanguageService)
        {
            _serviceProvider = serviceProvider;
            _textDocumentFactoryService = textDocumentFactoryService;
            _projectFactory = projectFactory;
            _fsharpVsLanguageService = fsharpVsLanguageService;
        }

        public ITagger<T> CreateTagger<T>(ITextView textView, ITextBuffer buffer) where T : ITag
        {
            // Only provide highlighting on the top-level buffer
            if (textView.TextBuffer != buffer) return null;

            var generalOptions = Setting.getGeneralOptions(_serviceProvider);
            if (generalOptions == null || !generalOptions.HighlightUsageEnabled) return null;

            ITextDocument doc;
            if (_textDocumentFactoryService.TryGetTextDocument(buffer, out doc))
            {
                return textView.Properties.GetOrCreateSingletonProperty(
                    () => new HighlightUsageTagger(doc, textView, _fsharpVsLanguageService, _serviceProvider, _projectFactory)) as ITagger<T>;
            }

            return null;
        }
    }
}