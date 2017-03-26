%%
%% This is file `./pseudoTeX/TeX2WordForapa6.bas',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% apa6.dtx  (with options: `bas')
%% ----------------------------------------------------------------------
%% 
%% apa6 - A LaTeX class for formatting documents in compliance with the
%% American Psychological Association's Publication Manual, 6th edition
%% 
%% Copyright (C) 2011-2016 by Brian D. Beitzel <brian at beitzel.com>
%% 
%% This work may be distributed and/or modified under the
%% conditions of the LaTeX Project Public License (LPPL), either
%% version 1.3c of this license or (at your option) any later
%% version.  The latest version of this license is in the file:
%% 
%% http://www.latex-project.org/lppl.txt
%% 
%% Users may freely modify these files without permission, as long as the
%% copyright line and this statement are maintained intact.
%% 
%% This work is not endorsed by, affiliated with, or probably even known
%% by, the American Psychological Association.
%% 
%% ----------------------------------------------------------------------
%% 
Attribute VB_Name = "TeX2WordForapa6"
Sub FormatTex2WordDocument()

    Dim strRunningHead, r, E
    Dim myrange As Range

    strRunningHead = InputBox("Please type the running head:", "Running Head")
    If strRunningHead = "" Then Exit Sub

    Selection.EndKey Unit:=wdStory
    Selection.InsertBreak Type:=wdPageBreak
    Selection.TypeText Text:="References" & vbCrLf
    Selection.MoveUp Unit:=wdLine, Count:=1
    Selection.ParagraphFormat.LeftIndent = InchesToPoints(0)
    Selection.ParagraphFormat.Alignment = wdAlignParagraphCenter

    Selection.HomeKey Unit:=wdStory

    Call FormatTex2WordPageHeader(strRunningHead)
    Call FormatAndMoveTex2WordTables
    Call FormatAndMoveTex2WordFigures

    Selection.EndKey Unit:=wdStory
    Selection.InsertBreak Type:=wdPageBreak
    Selection.TypeText Text:="Appendix" & vbCrLf
    Selection.MoveUp Unit:=wdLine, Count:=1
    Selection.ParagraphFormat.LeftIndent = InchesToPoints(0)
    Selection.ParagraphFormat.Alignment = wdAlignParagraphCenter

    ' touch up temporary citations
    Selection.HomeKey Unit:=wdStory

    Selection.Find.ClearFormatting
    Selection.Find.Replacement.ClearFormatting
    With Selection.Find
        .Text = "{\"
        .Replacement.Text = "{"
        .Forward = True
        .Wrap = wdFindContinue
        .Format = False
        .MatchCase = False
        .MatchWholeWord = False
        .MatchWildcards = False
        .MatchSoundsLike = False
        .MatchAllWordForms = False
    End With
    Selection.Find.Execute Replace:=wdReplaceAll

    With Selection.Find
        .Text = "@}"
        .Replacement.Text = "}"
        .Forward = True
        .Wrap = wdFindContinue
        .Format = False
        .MatchCase = False
        .MatchWholeWord = False
        .MatchWildcards = False
        .MatchSoundsLike = False
        .MatchAllWordForms = False
    End With
    Selection.Find.Execute Replace:=wdReplaceAll

    With Selection.Find
        .Text = "{e.g.,\"
        .Replacement.Text = "{e.g.`, \"
        .Forward = True
        .Wrap = wdFindContinue
        .Format = False
        .MatchCase = False
        .MatchWholeWord = False
        .MatchWildcards = False
        .MatchSoundsLike = False
        .MatchAllWordForms = False
    End With
    Selection.Find.Execute Replace:=wdReplaceAll

    With Selection.Find
        .Text = "{i.e.,\"
        .Replacement.Text = "{i.e.`, \"
        .Forward = True
        .Wrap = wdFindContinue
        .Format = False
        .MatchCase = False
        .MatchWholeWord = False
        .MatchWildcards = False
        .MatchSoundsLike = False
        .MatchAllWordForms = False
    End With
    Selection.Find.Execute Replace:=wdReplaceAll

    With Selection.Find
        .Text = "{cf.\"
        .Replacement.Text = "{cf. \"
        .Forward = True
        .Wrap = wdFindContinue
        .Format = False
        .MatchCase = False
        .MatchWholeWord = False
        .MatchWildcards = False
        .MatchSoundsLike = False
        .MatchAllWordForms = False
    End With
    Selection.Find.Execute Replace:=wdReplaceAll

    With Selection.Find
        .Text = "@p. "
        .Replacement.Text = "@"
        .Forward = True
        .Wrap = wdFindContinue
        .Format = False
        .MatchCase = False
        .MatchWholeWord = False
        .MatchWildcards = False
        .MatchSoundsLike = False
        .MatchAllWordForms = False
    End With
    Selection.Find.Execute Replace:=wdReplaceAll

    With Selection.Find
        .Text = "@pp. "
        .Replacement.Text = "@"
        .Forward = True
        .Wrap = wdFindContinue
        .Format = False
        .MatchCase = False
        .MatchWholeWord = False
        .MatchWildcards = False
        .MatchSoundsLike = False
        .MatchAllWordForms = False
    End With
    Selection.Find.Execute Replace:=wdReplaceAll

    ' for table footnotes
    With Selection.Find
        .Text = "[para,flushleft] "
        .Replacement.Text = ""
        .Forward = True
        .Wrap = wdFindContinue
        .Format = False
        .MatchCase = False
        .MatchWholeWord = False
        .MatchWildcards = False
        .MatchSoundsLike = False
        .MatchAllWordForms = False
    End With
    Selection.Find.Execute Replace:=wdReplaceAll

    ' remove extra spaces at the end of paragraphs
    With Selection.Find
        .Text = " ^p"
        .Replacement.Text = "^p"
        .Forward = True
        .Wrap = wdFindContinue
        .Format = False
        .MatchCase = False
        .MatchWholeWord = False
        .MatchWildcards = False
        .MatchSoundsLike = False
        .MatchAllWordForms = False
    End With
    Selection.Find.Execute Replace:=wdReplaceAll

   ' delete the instructions
    Selection.HomeKey Unit:=wdStory

    Set myrange = Selection.Range
    myrange.Start = Selection.Start

    Selection.HomeKey Unit:=wdStory
    Selection.Find.ClearFormatting
    With Selection.Find
        .Execute findText:="Delete these instructions!", Forward:=True, Wrap:=wdFindStop
        myrange.End = Selection.End + 1
        myrange.Select
        myrange.Delete
    End With

    Selection.HomeKey Unit:=wdStory

End Sub

Sub FormatTex2WordPageHeader(strRunningHead)

    Dim r, E

    ' set the first page to be a different header
    ActiveDocument.PageSetup.DifferentFirstPageHeaderFooter = True

    With ActiveDocument.Sections(1).Headers(wdHeaderFooterFirstPage)
        .Range.ParagraphFormat.LineSpacingRule = wdLineSpaceDouble
        .Range.ParagraphFormat.FirstLineIndent = InchesToPoints(0)
        .Range.ParagraphFormat.TabStops.ClearAll
        .Range.ParagraphFormat.TabStops.Add Position:=InchesToPoints(6.5), _
            Alignment:=wdAlignTabRight, Leader:=wdTabLeaderSpaces
        .Range.Text = "Running head: " & UCase(strRunningHead) & vbTab
        With .Range.Font
            .Name = "Times New Roman"
            .Size = 12
            .Bold = False
            .Italic = False
        End With

        Set r = .Range
        E = .Range.End
        r.Start = E
        .Range.Fields.Add r, wdFieldPage

    End With

    With ActiveDocument.Sections(1).Headers(wdHeaderFooterPrimary)
        .Range.ParagraphFormat.LineSpacingRule = wdLineSpaceDouble
        .Range.ParagraphFormat.FirstLineIndent = InchesToPoints(0)
        .Range.Text = UCase(strRunningHead)
        .PageNumbers.Add PageNumberAlignment:=wdAlignPageNumberRight
        With .Range.Font
            .Name = "Times New Roman"
            .Size = 12
            .Bold = False
            .Italic = False
        End With
    End With

End Sub

Sub FormatAndMoveTex2WordTables()

    Dim i, rngParagraphs As Range

    If ActiveDocument.Tables.Count > 0 Then

        For i = 1 To ActiveDocument.Tables.Count

            Selection.HomeKey Unit:=wdStory

            Set rngParagraphs = ActiveDocument.Range( _
                    Start:=ActiveDocument.Tables(1).Range.Start, _
                    End:=ActiveDocument.Tables(1).Range.End)
            rngParagraphs.Select

            With Selection.Tables(1)
                .Borders(wdBorderLeft).LineStyle = wdLineStyleNone
                .Borders(wdBorderRight).LineStyle = wdLineStyleNone
                With .Borders(wdBorderTop)
                    .LineStyle = wdLineStyleSingle
                    .LineWidth = wdLineWidth050pt
                    .Color = wdColorAutomatic
                End With
                With .Borders(wdBorderBottom)
                    .LineStyle = wdLineStyleSingle
                    .LineWidth = wdLineWidth050pt
                    .Color = wdColorAutomatic
                End With
                .Borders(wdBorderHorizontal).LineStyle = wdLineStyleNone
                .Borders(wdBorderVertical).LineStyle = wdLineStyleNone
                .Borders(wdBorderDiagonalDown).LineStyle = wdLineStyleNone
                .Borders(wdBorderDiagonalUp).LineStyle = wdLineStyleNone
                .Borders.Shadow = False
                '.Rows.Alignment = wdAlignRowLeft
                .PreferredWidthType = wdPreferredWidthPercent
                .PreferredWidth = 100
                .TopPadding = InchesToPoints(0.08)
                .BottomPadding = InchesToPoints(0.08)
                .LeftPadding = InchesToPoints(0.08)
                .RightPadding = InchesToPoints(0.08)
                .Spacing = 0
                .AllowPageBreaks = True
                .AllowAutoFit = False
            End With

            With Selection.ParagraphFormat
                .LineSpacingRule = wdLineSpaceSingle
                .LeftIndent = InchesToPoints(0)
                .RightIndent = InchesToPoints(0)
                .SpaceBefore = 0
                .SpaceBeforeAuto = False
                .SpaceAfter = 0
                .SpaceAfterAuto = False
                .WidowControl = False
                .KeepWithNext = False
                .KeepTogether = False
                .PageBreakBefore = False
                .NoLineNumber = False
                .Hyphenation = True
                .FirstLineIndent = InchesToPoints(0)
                .CharacterUnitLeftIndent = 0
                .CharacterUnitRightIndent = 0
                .CharacterUnitFirstLineIndent = 0
                .LineUnitBefore = 0
                .LineUnitAfter = 0
                .MirrorIndents = False
                .TextboxTightWrap = wdTightNone
            End With

            rngParagraphs.Cut

            Selection.EndKey Unit:=wdStory
            Selection.InsertBreak Type:=wdPageBreak
            Selection.TypeText Text:="Table " & i & vbCrLf
            Selection.Paste

        Next

    End If

End Sub

Sub FormatAndMoveTex2WordFigures()

    Dim i, rngParagraphs As Range

    If ActiveDocument.InlineShapes.Count > 0 Then

        For i = 1 To ActiveDocument.InlineShapes.Count

            Selection.HomeKey Unit:=wdStory

            Set rngParagraphs = ActiveDocument.Range( _
                    Start:=ActiveDocument.InlineShapes(1).Range.Start, _
                    End:=ActiveDocument.InlineShapes(1).Range.End)
            rngParagraphs.Select
            rngParagraphs.Cut

            Selection.EndKey Unit:=wdStory
            Selection.InsertBreak Type:=wdPageBreak
            Selection.Paste
            Selection.TypeText Text:=vbCrLf & "Figure " & i & vbCrLf

        Next

    End If

End Sub
%% 
%% Copyright (C) 2011-2016 by Brian D. Beitzel <brian at beitzel.com>
%% 
%% This work may be distributed and/or modified under the
%% conditions of the LaTeX Project Public License (LPPL), either
%% version 1.3c of this license or (at your option) any later
%% version.  The latest version of this license is in the file:
%% 
%% http://www.latex-project.org/lppl.txt
%% 
%% Users may freely modify these files without permission, as long as the
%% copyright line and this statement are maintained intact.
%% 
%% This work is not endorsed by, affiliated with, or probably even known
%% by, the American Psychological Association.
%% 
%% 
%% This work is "maintained" (as per LPPL maintenance status) by
%% Brian D. Beitzel.
%% 
%% This work consists of the file  apa6.dtx
%% and the derived files           apa6.ins,
%%                                 apa6.cls,
%%                                 apa6.pdf,
%%                                 README,
%%                                 APAamerican.txt,
%%                                 APAbritish.txt,
%%                                 APAdutch.txt,
%%                                 APAenglish.txt,
%%                                 APAgerman.txt,
%%                                 APAngerman.txt,
%%                                 APAgreek.txt,
%%                                 APAczech.txt,
%%                                 APAturkish.txt,
%%                                 APAendfloat.cfg,
%%                                 apa6.ptex,
%%                                 TeX2WordForapa6.bas,
%%                                 Figure1.pdf,
%%                                 shortsample.tex,
%%                                 longsample.tex, and
%%                                 bibliography.bib.
%% 
%%
%% End of file `./pseudoTeX/TeX2WordForapa6.bas'.
