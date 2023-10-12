REM  *****  BASIC  *****  -*- mode: basic-generic; -*-

' Apache OpenOffice BASIC Programming Guide: https://wiki.openoffice.org/wiki/Documentation/BASIC_Guide
' Reference documentation for the UNO IDL API: https://api.libreoffice.org/docs/idl/ref/index.html

Function fnWhichComponent(oDoc) as string
    If HasUnoInterfaces(oDoc, "com.sun.star.lang.XServiceInfo") Then
        If thisComponent.supportsService ("com.sun.star.text.GenericTextDocument") Then
            fnWhichComponent = "Text"
        ElseIf thisComponent.supportsService("com.sun.star.sheet.SpreadsheetDocument") Then
            fnWhichComponent = "Spreadsheet"
        ElseIf thisComponent.supportsService("com.sun.star.presentation.PresentationDocument") Then
            fnWhichComponent = "Presentation"
        ElseIf thisComponent.supportsService("com.sun.star.drawing.GenericDrawingDocument") Then
            fnWhichComponent = "Drawing"
        Else
            fnWhichComponent = "Oops current document something else"
        End If
    Else
        fnWhichComponent = "Not a document"
    End If
End function

Sub UpdateAll(Optional inFileURL, Optional removeFileLinks)
    Dim inDoc                            ' See https://docs.libreoffice.org/sw/html/classSwXTextDocument.html
    Dim interactive

    If IsMissing(inFileURL) Then
        interactive = True
        ' Most likely, the Macro is run interactively. Act on
        ' the current document
        If ThisComponent.HasLocation() Then
            inDoc = ThisComponent
            inFileURL = inDoc.GetLocation()
        End If
    Else
        interactive = False
        Dim oProps(0) as New com.sun.star.beans.PropertyValue
        oProps(0).Name = "Hidden"
        oProps(0).Value = True
        inDoc = StarDesktop.loadComponentFromURL(inFileURL, "_blank", 0, oProps())
    End If

    ' Neither ".uno:UpdateAll" nor ".uno:UpdateAllLinks" seem to update
    ' links.  So use this instead.
    inDoc.UpdateLinks()

    ' Break links to external files, if requested.
    ' See https://lists.freedesktop.org/archives/libreoffice/2011-October/019446.html
    If Not IsMissing(removeFileLinks) Then
        Dim section                       ' See https://docs.libreoffice.org/sw/html/classSwXTextSection.html
        Dim sectionNames
        Dim sectionFileLink as new com.sun.star.text.SectionFileLink

        sectionFileLink.FileURL=""

        sSectionNames = inDoc.getTextSections().getElementNames()

        For i = 0 To UBound(sSectionNames)
            section = inDoc.getTextSections().getByName(sSectionNames(i))
            section.setPropertyValue("FileLink",sectionFileLink)
        Next
    End If

    ' Update All Indices

    ' dispatcher = createUnoService("com.sun.star.frame.DispatchHelper")
    ' dispatcher.executeDispatch(inDoc.CurrentController.Frame, ".uno:UpdateAll", "", 0, Array())

    Dim oIndexes                  ' See https://docs.libreoffice.org/sw/html/classSwXDocumentIndexes.html
    Dim oIndex                    ' See https://docs.libreoffice.org/sw/html/classSwXDocumentIndex.html
    oIndexes = inDoc.getDocumentIndexes()
    For i = 0 To oIndexes.getCount() - 1
        oIndex = oIndexes.getByIndex(i) '
        oIndex.update()
    Next

    If (inDoc.isModified() AND inDoc.hasLocation() AND (Not inDoc.isReadOnly())) Then
        inDoc.store()
    End If

    If Not interactive Then
        inDoc.Close(True)
    End If
End Sub

Sub OptimizeColumnWidthOfSheet(oSheet)
    oCursor = oSheet.createCursor()
    oCursor.gotoEndOfUsedArea(True)
    oColumns = oCursor.getColumns()

    For j = oColumns.getCount() - 1 To 0 Step -1
        oColumn = oColumns.getByIndex(j)
        oColumn.OptimalWidth = True
        REM Prevent columns from getting too wide; clamp their width to MAX_WIDTH
        If oColumn.Width > MAX_WIDTH Then
            oColumn.OptimalWidth = False
            oColumn.Width = MAX_WIDTH
        End If
    Next j
End Sub

Sub OptimizeColumnWidth(Optional inFileURL)
    Dim s As String
    Dim i, j, k As Long
    Dim oTables
    Dim oTable
    Dim oCell
    Dim inDoc
    Dim interactive

    If IsMissing(inFileURL) Then
        interactive = True
        ' Most likely, the Macro is run interactively. Act on
        ' the current document
        If ThisComponent.HasLocation() Then
            inDoc = ThisComponent
            inFileURL = inDoc.GetLocation()
        End If
    Else
        interactive = False
        Dim oProps(0) as New com.sun.star.beans.PropertyValue
        oProps(0).Name = "Hidden"
        oProps(0).Value = True
        inDoc = StarDesktop.loadComponentFromURL(inFileURL, "_blank", 0, oProps())
    End If

    inDocType = fnWhichComponent(inDoc)

    If inDocType = "Text" Then
        oTables = inDoc.getTextTables()
        If oTables.hasElements() Then
            For i = 0 To oTables.getCount() - 1
                oTable = oTables.getByIndex(i)
                inDoc.getCurrentController().select(oTable)
                oFrame = inDoc.CurrentController.Frame
                oDispHelper = createUnoService("com.sun.star.frame.DispatchHelper")
                oDispHelper.executeDispatch(oFrame, ".uno:SelectTable", "", 0, Array())
                oDispHelper.executeDispatch(oFrame, ".uno:SetOptimalColumnWidth", "", 0, Array())
            Next
        End If
    ElseIf inDocType = "Spreadsheet" Then
        Const MAX_WIDTH = 10000 '10 cm
        Dim oCurrentController As Variant
        Dim oActiveSheet As Variant
        Dim oCursor As Variant
        Dim oColumns As Variant
        Dim oColumn As Variant
        Dim oSheets
        Dim oSheet

        oCurrentController = inDoc.getCurrentController()
        If interactive Then
            oSheet = oCurrentController.getActiveSheet()
            OptimizeColumnWidthOfSheet(oSheet)
        Else
            oSheets = inDoc.Sheets
            For i = 0 To oSheets.getCount()-1
                oSheet = oSheets.getByIndex(i)
                OptimizeColumnWidthOfSheet(oSheet)
            Next i
        End If
    End If

    If (inDoc.isModified() AND inDoc.hasLocation() AND (Not inDoc.isReadOnly())) Then
        inDoc.store()
    End If

    If Not interactive Then
        inDoc.Close(True)
    End If
End Sub

Sub Reload(Optional inFileURL)
    Dim inDoc
    Dim interactive

    If IsMissing(inFileURL) Then
        interactive = True
        ' Most likely, the Macro is run interactively. Act on
        ' the current document
        If ThisComponent.HasLocation() Then
            inDoc = ThisComponent
            inFileURL = inDoc.GetLocation()
        End If
    Else
        interactive = False
        Dim oProps(0) as New com.sun.star.beans.PropertyValue
        oProps(0).Name = "Hidden"
        oProps(0).Value = True
        inDoc = StarDesktop.loadComponentFromURL(inFileURL, "_default", 0, oProps())
    End If

    If Not IsNull(inDoc) Then
        dispatcher = createUnoService("com.sun.star.frame.DispatchHelper")
        dispatcher.executeDispatch(inDoc.CurrentController.Frame, ".uno:Reload", "", 0, Array())
    End If
End Sub

Sub CreateIndexMark(oText, oCursor, bodyText, primaryKey, markEntry, alphaIndex)
    oText.insertString( oCursor, bodyText, True )
    ' Create Index Mark
    indexMark = ThisComponent.createInstance("com.sun.star.text.DocumentIndexMark")
    indexMark.PrimaryKey = primaryKey
    ' markEntry = bodyText
    indexMark.setMarkEntry(markEntry)
    ' Insert the Index Mark
    oText.InsertTextContent(oCursor, indexMark, False)
    ' No more text range
    oCursor.collapseToEnd()
End Sub

Function RepeatText(text, times)
    bigText = ""
    For i = 0 To times
        bigText = bigText & text
    Next i
    RepeatText = bigText
End Function

Function AnchoringParaText(i, Optional n)
    text = ""
    Select Case i

    Case 0:
        text = " xxxxxxxx yyyyyyyy"
        If IsMissing(n) Then
            n = 9
        End If
    Case 1:
        text = " aaaaaaaa bbbbbbbb"
        If IsMissing(n) Then
            n = 15
        End If
    Case Else
        text = "Id amet, velit fugiat sunt mollit dolor. Fugiat laborum deserunt cillum aliqua magna voluptate mollit. Nulla in qui ad elit, duis sit sit. Dolor et cupidatat in eiusmod ex. Occaecat culpa dolor et magna sed do culpa. "
        If IsMissing(n) Then
            n = 1
        End If
    End Select
    AnchoringParaText = RepeatText(text, n)
End Function

Sub AddPageNumber
    Doc = ThisComponent

    PageNumber = Doc.createInstance("com.sun.star.text.textfield.PageNumber")
    PageNumber.NumberingType = com.sun.star.style.NumberingType.ARABIC

    PageStyles = Doc.StyleFamilies.getByName("PageStyles")

    StdPage = PageStyles("Default")
    StdPage.FooterIsOn = True

    FooterCursor = StdPage.FooterTextLeft.Text.createTextCursor()
    FooterCursor.ParaAdjust = com.sun.star.style.ParagraphAdjust.CENTER
    StdPage.FooterTextLeft.Text.insertTextContent(FooterCursor, PageNumber, False)
End Sub

Sub ApplyTableTemplate(templateName)
    dim document   as object
    dim dispatcher as object

    document   = ThisComponent.CurrentController.Frame
    dispatcher = createUnoService("com.sun.star.frame.DispatchHelper")

    dim args1(1) as new com.sun.star.beans.PropertyValue
    args1(0).Name = "Template"
    args1(0).Value = templateName
    args1(1).Name = "Family"
    args1(1).Value = 32
    dispatcher.executeDispatch(document, ".uno:StyleApply", "", 0, args1())
End Sub

Sub ApplyTableTemplates
    inDoc = ThisComponent
    oTables = inDoc.getTextTables()
    If oTables.hasElements() Then
        For i = 0 To oTables.getCount() - 1
            oTable = oTables.getByIndex(i)
            inDoc.getCurrentController().select(oTable)
            ApplyTableTemplate(oTable.Name)
        Next
    End If
End Sub

Sub CreateODTFileWithAllFactoryStyles
    Dim inDoc
    Dim interactive
    ' BasicLibraries.loadLibrary("XrayTool")
    ' LoadMriLibrary()
    BasicLibraries.loadLibrary("MRILib")
    Dim oProps(0) as New com.sun.star.beans.PropertyValue
    oProps(0).Name = "Hidden"
    oProps(0).Value = False
    inDoc = StarDesktop.loadComponentFromURL( "private:factory/swriter", "_blank", 0, Array() )
    fileURL = ConvertToURL("/home/kjambunathan/vanilla-styles.odt")

    AddPageNumber()

    StyleFamilies = inDoc.StyleFamilies
    ParagraphStyles = StyleFamilies.getByName("ParagraphStyles")

    ' Create a Paragraph Style for Family Style Names and Insert it in
    ' to the StyleSheet
    myFamilyStyle = inDoc.createInstance("com.sun.star.style.ParagraphStyle")
    myFamilyStyle.Name = "My Family Style"
    myFamilyStyle.CharColor = RGB(255, 0, 0)
    myFamilyStyle.CharWeight = com.sun.star.awt.FontWeight.BOLD
    ParagraphStyles.insertByName("My Family Style", myFamilyStyle)

    ' Create a Paragraph Style for Style Names and Insert it in
    ' to the StyleSheet
    myStyle = inDoc.createInstance("com.sun.star.style.ParagraphStyle")
    myStyle.Name = "My Style"
    myStyle.CharColor = RGB(6, 154, 46)
    myStyle.CharWeight = com.sun.star.awt.FontWeight.BOLD

    ParagraphStyles.insertByName("My Style", myStyle)

    ' Obtain Text and Text Cursor
    oText = inDoc.getText()
    oCursor = oText.createTextCursor()

    ' Create and Configure TOC
    toc = inDoc.createInstance("com.sun.star.text.ContentIndex")
    toc.Title = "Table of Style Families & Styles"
    toc.CreateFromOutline = False
    toc.CreateFromLevelParagraphStyles = True
    oText.insertTextContent(oCursor, toc, False)

    oLevelParagraphStyles = toc.LevelParagraphStyles
    oLevelParagraphStyles.replaceByIndex(0, Array("My Family Style"))
    oLevelParagraphStyles.replaceByIndex(1, Array("My Style"))

    ' Create an Alphbhetical Index of Style Families & Style Names
    ' userIndex =  inDoc.createInstance("com.sun.star.text.UserIndex")
    ' userIndex.CreateFromMarks = True

    alphaIndex = ThisComponent.createInstance("com.sun.star.text.DocumentIndex")
    alphaIndex.UseCombinedEntries = False
    oText.insertTextContent(oCursor, alphaIndex, FALSE)

    ' Create Dummy User Index Marks

    styleFamilyNames = inDoc.StyleFamilies.getElementNames()

    ' For each StyleFamily
    For i = LBound(styleFamilyNames) To UBound(styleFamilyNames)
        styleFamilyName = styleFamilyNames(i)

        styleNames = inDoc.StyleFamilies.getByName(styleFamilyName).getElementNames()

        oText.insertString( oCursor, "STYLE FAMILY: " & styleFamilyName, False )
        oCursor.ParaStyleName = "My Family Style"
        oText.insertControlCharacter( oCursor, com.sun.star.text.ControlCharacter.PARAGRAPH_BREAK, False )
        oCursor.ParaStyleName = "Standard"

        ' For each StyleName in StyleFamily
        For j = LBound(styleNames) To UBound(styleNames)

            styleName = styleNames(j)
            xxx = inDoc.StyleFamilies.getByName(styleFamilyName).getByName(styleName)
            oCursor.CharWeight = com.sun.star.awt.FontWeight.NORMAL
            ' Print styleFamilyName & ": " & styleName
            ' oCursor.CharStyleName = "Standard"

            Select Case styleFamilyName
            Case "PageStyles":
            Case Else
                CreateIndexMark(oText, oCursor, "", styleFamilyName, styleName, alphaIndex)
            End Select

            Select Case styleFamilyName
            Case "CharacterStyles":
                ' Insert Style Name
                oCursor.CharStyleName = styleName
                oCursor.ParaStyleName = "My Style"

                oText.insertString( oCursor, styleName, False )
                oText.insertControlCharacter( oCursor, com.sun.star.text.ControlCharacter.PARAGRAPH_BREAK, False )

                oCursor.CharStyleName = "Standard"
            Case "ParagraphStyles":

                ' Insert Style Name
                oCursor.ParaStyleName = "My Style"

                oText.insertString( oCursor, styleName, False )
                oText.insertControlCharacter( oCursor, com.sun.star.text.ControlCharacter.PARAGRAPH_BREAK, False )

                oCursor.ParaStyleName = "Standard"

                ' Insert a big Paragraph
                oCursor.ParaStyleName = styleName
                oText.insertString( oCursor, AnchoringParaText(2), False )
                oText.insertControlCharacter( oCursor, com.sun.star.text.ControlCharacter.PARAGRAPH_BREAK, False )
                oCursor.ParaStyleName = "Standard"

            Case "NumberingStyles":

                bodyText = styleName

                ' Insert Style Name
                oCursor.ParaStyleName = "My Style"
                oText.insertString( oCursor, bodyText, False )
                oText.insertControlCharacter( oCursor, com.sun.star.text.ControlCharacter.PARAGRAPH_BREAK, False )
                oCursor.ParaStyleName = "Standard"

                nCount = xxx.NumberingRules.getCount()
                For k = 0 To nCount - 1
                    oCursor.NumberingLevel = k
                    oCursor.NumberingStyleName = styleName

                    oCursor.CharColor = RGB(0,0,255)
                    bodyText = styleName & " [Level " & k & "]"
                    oText.insertString( oCursor, bodyText, True )
                    ' oCursor.CharStyleName = "Standard"

                    oCursor.collapseToEnd()

                    oCursor.CharStyleName = "Standard"

                    bodyText = " " & AnchoringParaText(2)
                    oText.insertString( oCursor, bodyText, False )

                    oText.insertControlCharacter( oCursor, com.sun.star.text.ControlCharacter.PARAGRAPH_BREAK, False )
                Next k
                oCursor.NumberingLevel = 0
                oCursor.NumberingStyleName = ""
                oCursor.ParaStyleName = "Standard"

            Case "CellStyles1":
                oTable = inDoc.createInstance( "com.sun.star.text.TextTable" )
                oTable.initialize(1, 1) 'Single Cell
                oTable.TableName = styleName
                oInsertPoint = ThisComponent.Text.getEnd()
                oInsertPoint.getText().insertTextContent(oInsertPoint , oTable, False)
                oCursor.ParaStyleName = "My Style"
                oText.insertControlCharacter( oCursor, com.sun.star.text.ControlCharacter.PARAGRAPH_BREAK, False )
                oTableCell = oTable.getCellByPosition(0,0)
                oTableCell.setString(styleName & " {" & styleFamilyName & "}")
                ' TODO: Don't know how to associate oTableCell with the corresponding xxx
            Case "TableStyles":
                oCursor.ParaStyleName = "My Style"
                oText.insertString( oCursor, styleName, False )
                oText.insertControlCharacter( oCursor, com.sun.star.text.ControlCharacter.PARAGRAPH_BREAK, False )
                oCursor.ParaStyleName = "Standard"

                oTable = inDoc.createInstance( "com.sun.star.text.TextTable" )
                oTable.initialize(6, 6) 'Two rows, three columns
                oTable.TableName = styleName
                ' Setting "TableTemplateName" doesn't give fancy tables
                ' oTable.setPropertyValue("TableTemplateName", styleName)
                oInsertPoint = ThisComponent.Text.getEnd()
                oInsertPoint.getText().insertTextContent(oInsertPoint , oTable, False)
                ' Invoking `autoformat' gives a fancy table, but
                ' the Table Template definition doesn't land in
                ' the `styles.xml'.
                '  oTable.autoformat(styleName)
                oText.insertControlCharacter( oCursor, com.sun.star.text.ControlCharacter.PARAGRAPH_BREAK, False )
                oTableCell = oTable.getCellByPosition(0,0)
                oTableCell.setString(styleName & " {" & styleFamilyName & "}")
            Case "FrameStyles":

                ' Push the style
                oCursor.ParaStyleName = "My Style"

                ' Caption the Frame, and style it with 'My Style'
                ' Emit LineBreaks around the caption to create some breathing space.
                oText.insertControlCharacter( oCursor, com.sun.star.text.ControlCharacter.LINE_BREAK, False )
                oText.insertString( oCursor,   styleName & " {" & styleFamilyName & "}", False )
                oText.insertControlCharacter( oCursor, com.sun.star.text.ControlCharacter.LINE_BREAK, False )

                oText.insertControlCharacter( oCursor, com.sun.star.text.ControlCharacter.PARAGRAPH_BREAK, False )

                ' Pop the style
                oCursor.ParaStyleName = "Standard"

                ' Insert some text before the Frame
                bodyText = "^^^PARA BEGIN: " & styleName & AnchoringParaText(0) & RepeatText("+", 8)
                oText.insertString( oCursor, bodyText , False )

                ' Create and Insert the Text Frame
                oFrame = ThisComponent.createInstance( "com.sun.star.text.TextFrame" )
                oText.insertTextContent( oCursor, oFrame, false )

                ' Insert some text after the Frame
                bodyText = RepeatText("-", 8) + AnchoringParaText(1) & styleName & " :PARA END$$$$"
                oText.insertString(oCursor, bodyText, False)

                ' Insert Paragraph Break
                oText.insertControlCharacter( oCursor, com.sun.star.text.ControlCharacter.PARAGRAPH_BREAK, False )

                ' Configure the Frame
                oFrame.FrameStyleName = styleName
                oFrame.Name = styleName
                If j = -1 Then
                    xray oFrame
                End If
                oFrame.Width = 4000

                ' Fill the Frame
                annotation = ""
                If oFrame.AnchorType = com.sun.star.text.TextContentAnchorType.AT_PARAGRAPH Then
                    annotation = annotation & " [" &  styleName & "@at-paragraph" &  "]"
                ElseIf oFrame.AnchorType = com.sun.star.text.TextContentAnchorType.AS_CHARACTER Then
                    annotation = annotation & " [" &  styleName & "@as-character" &  "]"
                ElseIf oFrame.AnchorType = com.sun.star.text.TextContentAnchorType.AT_PAGE Then
                    annotation = annotation & " [" &  styleName & "@at-page" &  "]"
                ElseIf oFrame.AnchorType = com.sun.star.text.TextContentAnchorType.AT_FRAME Then
                    annotation = annotation & " [" &  styleName & "@at-frame" &  "]"
                ElseIf oFrame.AnchorType = com.sun.star.text.TextContentAnchorType.AT_CHARACTER Then
                    annotation = annotation & " [" &  styleName & "@at-character" &  "]"
                Else
                    MsgBox "This shouldn't happen"
                End If

                oFrame.BackColor = RGB(255, 255, 215)
                oFrame.Text.String = RepeatText(annotation, 2)
            Case "PageStyles":
                oText.insertString(oCursor, "" & styleName & " {" & styleFamilyName & "}", False)
                oCursor.PageDescName = styleName
                oCursor.ParaStyleName = "My Style"
                oCursor.breakType = com.sun.star.style.BreakType.PAGE_BEFORE
                oText.insertControlCharacter( oCursor, com.sun.star.text.ControlCharacter.PARAGRAPH_BREAK, False )
                oCursor.ParaStyleName = "Standard"
                oCursor.PageDescName = "Standard"

                CreateIndexMark(oText, oCursor, "", styleFamilyName, styleName, alphaIndex)
                ' oText.insertString( oCursor, styleName & " {" & styleFamilyName & "}", False )
                ' oCursor.ParaStyleName = "My Style"
            Case Else
                ' MsgBox "You haven't handled " & styleName & "[" & styleFamilyName & "]"
                oCursor.ParaStyleName = "My Style"
                oText.insertString( oCursor, styleName & " {" & styleFamilyName & "}", False )
                oText.insertControlCharacter( oCursor, com.sun.star.text.ControlCharacter.PARAGRAPH_BREAK, False )
            End Select

        Next j
        oText.insertString(oCursor, "--- END STYLE FAMILY `" & styleFamilyName & "' ---", False)
        oCursor.breakType = com.sun.star.style.BreakType.PAGE_AFTER
        oText.insertControlCharacter( oCursor, com.sun.star.text.ControlCharacter.PARAGRAPH_BREAK, False )
    Next i
    ApplyTableTemplates()
    If (inDoc.isModified() AND inDoc.hasLocation() AND (Not inDoc.isReadOnly())) Then
        inDoc.store()
    End If
    Dim xProps(0) as New com.sun.star.beans.PropertyValue
    xProps(0).Name = "Overwrite"
    xProps(0).Value = True
    ' inDoc.storeAsUrl(fileURL, xProps)
    toc.update()

    alphaIndex.update()

End Sub

Sub ODSDoExportToTSV(Optional inFileURL, Optional format)
    Dim inDoc
    Dim interactive
    Dim oCurrentController As Variant
    Dim oUserSheet As Variant

    If IsMissing(inFileURL) Then
        interactive = True
        ' Most likely, the Macro is run interactively. Act on
        ' the current document
        If ThisComponent.HasLocation() Then
            inDoc = ThisComponent
            inFileURL = inDoc.GetLocation()
        End If
    Else
        interactive = False
        Dim oProps(0) as New com.sun.star.beans.PropertyValue
        oProps(0).Name = "Hidden"
        ' oProps(0).Value = True

        ' When the sheet is Hidden, the switching of sheet with
        ' setActiveSheet doesn't happen.  As a result all the exported
        ' CSV file end up with same content.

        ' According to
        ' https://bz.apache.org/ooo/show_bug.cgi?id=127395 using
        ' `setActiveSheet' on a non-Hidden document should switch
        ' sheets without any issue.  May be there is a regression in
        ' the recent version of LO.
        oProps(0).Value = False

        inDoc = StarDesktop.loadComponentFromURL(inFileURL, "_blank", 0, oProps())
    End If

    inFileURL = inDoc.GetLocation()

    Dim oFilterProps(1) as new com.sun.star.beans.PropertyValue
    oFilterProps(0).Name = "FilterName"
    oFilterProps(0).Value = "Text - txt - csv (StarCalc)"
    oFilterProps(1).Name = "FilterOptions"

    If IsMissing(format) Then
        format = "formulatsv"
    End If

    Dim outFileExtension
    Select Case format
    Case "formulatsv"
        ' See https://github.com/kjambunathan/org-mode-ox-odt/issues/94#issuecomment-881846106
        ' Character Set                                    - Unicode (UTF-8)
        ' Field Delimiter                                  - Tab
        ' String Delimiter                                 - Double Quote (= `"' character)
        ' Save Cell Content As Shown                       - OFF
        ' Save Cell Formulas instead of calculated values  - ON
        ' Quote All Text Cells                             - OFF
        ' Fixed Column Width                               - OFF
        oFilterProps(1).Value = "9,34,76,1,,0,false,true,false,true,false,0,true,false"
        outFileExtension = ".formula.tsv"
    Case "csv":
        ' Character Set                                    - Unicode (UTF-8)
        ' Field Delimiter                                  - Comma (= `,' character)
        ' String Delimiter                                 - Double Quote (= `"' character)
        ' Save Cell Content As Shown                       - ON
        ' Save Cell Formulas instead of calculated values  - OFF
        ' Quote All Text Cells                             - ON
        ' Fixed Column Width                               - OFF
        oFilterProps(1).Value = "44,34,76,1,,0,true,true,true,false,false,0,true,false"
        outFileExtension = ".csv"
    Case "tsv":
        ' Character Set                                    - Unicode (UTF-8)
        ' Field Delimiter                                  - Tab
        ' String Delimiter                                 - Double Quote (= `"' character)
        ' Save Cell Content As Shown                       - OFF
        ' Save Cell Formulas instead of calculated values  - OFF
        ' Quote All Text Cells                             - OFF
        ' Fixed Column Width                               - OFF
        oFilterProps(1).Value = "9,34,76,1,,0,false,true,false,false,false,0,true,false"
        outFileExtension = ".tsv"
    Case Else
        ' By default export to TSV
        ' Character Set                                    - Unicode (UTF-8)
        ' Field Delimiter                                  - Tab
        ' String Delimiter                                 - Double Quote (= `"' character)
        ' Save Cell Content As Shown                       - OFF
        ' Save Cell Formulas instead of calculated values  - OFF
        ' Quote All Text Cells                             - OFF
        ' Fixed Column Width                               - OFF
        oFilterProps(1).Value = "9,34,76,1,,0,false,true,false,false,false,0,true,false"
        outFileExtension = ".formula.tsv"
    End Select

    oCurrentController = inDoc.getCurrentController()
    If interactive Then
        oUserSheet = oCurrentController.getActiveSheet()
    End If

    oSheets = inDoc.Sheets
    For i = 0 To oSheets.getCount()-1
        oSheet = oSheets.getByIndex(i)
        oCurrentController.setActiveSheet(oSheet)

        outFileUrl = inFileUrl + "--" + oSheet.Name + outFileExtension
        inDoc.StoreToURL(outFileUrl, oFilterProps())
    Next i

    If interactive Then
        oCurrentController.setActiveSheet(oUserSheet)
    End If

    If (inDoc.isModified() AND inDoc.hasLocation() AND (Not inDoc.isReadOnly())) Then
        inDoc.store()
    End If

    If Not interactive Then
        inDoc.Close(True)
    End If
End Sub

Sub ODSExportToFormulaTSV(Optional inFileURL)
    ODSDoExportToTSV( inFileURL, "formulatsv")
End Sub

Sub ODSExportToTSV(Optional inFileURL)
    ODSDoExportToTSV( inFileURL, "tsv")
End Sub

Sub ODSExportToCSV(Optional inFileURL)
    ODSDoExportToTSV( inFileURL, "csv")
End Sub

' Local Variables:
' indent-tabs-mode: nil
' basic-indent-offset: 4
' End:
