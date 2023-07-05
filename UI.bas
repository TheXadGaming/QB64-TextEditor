Type Coordinate
    X As Integer
    Y As Integer
End Type

Type Scale
    Width As Integer
    Height As Integer
End Type

Type Color
    Foreground As Integer
    Background As Integer
End Type

Type Element
    ID As Integer '                 'The Identifing Number for the Element.
    ParentID As Integer '           'This Element's Parent's Identifer.
    Type As String '                'The type of Element this is.
    Location As Coordinate '        'The .X and .Y position of the Top-Left corner.
    TextLocation As Coordinate '    'The .X and .Y position of the Text.
    Scale As Scale '                'The .Width and .Height of the Element.
    Text As String '                'The Text on the Element.
    Signal As Integer '             'The Echoed Value on interact.
    MainColor As Color '            'The Normal .Foreground and .Background colors.
    InteractedColor As Color '      'The .Foreground and .Background colors once interacted with,
    InteractionKey As Integer '     'The Key to be pressed to interact with.
End Type

Const UI_True = -1
Const UI_False = 0

Sub ProcessElements (ElementsArray() As Element, SignalVar As Integer, KeyboardCode As Integer, SelectedElement As Integer)
    Elements = 0
    Do
        Elements = Elements + 1
    Loop While (ElementsArray(Elements).ID <> 0)
    Elements = Elements - 1
    If (Elements >= 1) Then
        For DisplayNum = 1 To Elements
            If ((DisplayNum = SelectedElement) And (ElementsArray(DisplayNum).InteractionKey = KeyboardCode)) Then: SignalVar = ElementsArray(DisplayNum).Signal
            ShouldDisplay = UI_True
            If (ElementsArray(DisplayNum).ParentID = 0) Then
                ParentType$ = ElementsArray(DisplayNum).Type
                ParentID = ElementsArray(DisplayNum).ID
                DisplayingParent = True
            End If
            If (ParentType$ = "dropdown") Then
                If (ElementsArray(DisplayNum).ParentID = 0) Then
                ElseIf (ElementsArray(DisplayNum).ParentID <> 0) Then
                    If (ElementsArray(SelectedElement).ParentID = ParentID) Then
                    Else
                        ShouldDisplay = UI_False
                    End If
                End If
            End If
            If (ShouldDisplay = UI_True) Then
                DisplayElement ElementsArray(DisplayNum), UI_False
            End If
            DisplayingParent = UI_False
        Next DisplayNum
        ID = SelectedElement
        Do
            DisplayElement ElementsArray(ID), UI_True
            ID = ElementsArray(ID).ParentID
            If (ID = 0) Then: Exit Do
        Loop
    End If
End Sub

Sub LoadUIFile (Path As String, ElementsArray() As Element)
    Dim FileLine As String
    File = FreeFile
    LineNum = 0
    CurrentElement = 0
    Open Path For Input As #File
    Do Until EOF(File)
        CurrentElement = (Int(LineNum / 16) + 1)
        Attribute = ((LineNum - ((CurrentElement - 1) * 16)) + 1)
        'Divided/Multipled By Value is amount of Data Points.
        LineNum = LineNum + 1
        Input #File, FileLine
        Select Case Attribute
            Case 1: ElementsArray(CurrentElement).ID = (Val(FileLine))
            Case 2: ElementsArray(CurrentElement).ParentID = (Val(FileLine))
            Case 3: ElementsArray(CurrentElement).Type = (FileLine)
            Case 4: ElementsArray(CurrentElement).Location.X = (Val(FileLine))
            Case 5: ElementsArray(CurrentElement).Location.Y = (Val(FileLine))
            Case 6: ElementsArray(CurrentElement).TextLocation.X = (Val(FileLine))
            Case 7: ElementsArray(CurrentElement).TextLocation.Y = (Val(FileLine))
            Case 8: ElementsArray(CurrentElement).Scale.Width = (Val(FileLine))
            Case 9: ElementsArray(CurrentElement).Scale.Height = (Val(FileLine))
            Case 10: ElementsArray(CurrentElement).Text = (FileLine)
            Case 11: ElementsArray(CurrentElement).Signal = (Val(FileLine))
            Case 12: ElementsArray(CurrentElement).MainColor.Foreground = (Val(FileLine))
            Case 13: ElementsArray(CurrentElement).MainColor.Background = (Val(FileLine))
            Case 14: ElementsArray(CurrentElement).InteractedColor.Foreground = (Val(FileLine))
            Case 15: ElementsArray(CurrentElement).InteractedColor.Background = (Val(FileLine))
            Case 16: ElementsArray(CurrentElement).InteractionKey = (Val(FileLine))
        End Select
    Loop
    Close #File
End Sub

Sub DisplayElement (Element As Element, Selected As Integer)
    For Row = Element.Location.Y To (Element.Scale.Height + Element.Location.Y) - 1
        For Column = Element.Location.X To (Element.Scale.Width + Element.Location.X) - 1
            Locate Row, Column
            Color Element.MainColor.Background, Element.MainColor.Background
            If (Selected = UI_True) Then
                Color Element.InteractedColor.Background, Element.InteractedColor.Background
            End If
            Print "Û";
        Next Column
    Next Row
    Locate Element.TextLocation.Y, Element.TextLocation.X
    Color Element.MainColor.Foreground, Element.MainColor.Background
    If (Selected = UI_True) Then
        Color Element.InteractedColor.Foreground, Element.InteractedColor.Background
    End If
    Print Element.Text;
End Sub

Sub DisplayContents (Element As Element)
    Print Element.ID
    Print Element.ParentID
    Print Element.Type
    Print Element.Location.X
    Print Element.Location.Y
    Print Element.TextLocation.X
    Print Element.TextLocation.Y
    Print Element.Scale.Width
    Print Element.Scale.Height
    Print Element.Text
    Print Element.Signal
    Print Element.MainColor.Foreground
    Print Element.MainColor.Background
    Print Element.InteractedColor.Foreground
    Print Element.InteractedColor.Background
    Print Element.InteractionKey
End Sub
