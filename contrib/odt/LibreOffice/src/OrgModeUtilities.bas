REM  *****  BASIC  *****

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

   ' Update All Indices
   dispatcher = createUnoService("com.sun.star.frame.DispatchHelper")

   dispatcher.executeDispatch(inDoc.CurrentController.Frame, ".uno:UpdateAll", "", 0, Array())

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

   If (inDoc.isModified() AND inDoc.hasLocation() AND (Not inDoc.isReadOnly())) Then
      inDoc.store()
   End If

   If Not interactive Then
      inDoc.Close(True)
   End If
End Sub

Sub OptimizeColumnWidth(Optional inFileURL)
   Dim s As String
   Dim i As Long
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
