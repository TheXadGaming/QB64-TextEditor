'Add CTRL+F To find a phrase in the lines
'Add CTRL+G to type in a line to go to
'ADD CTRL+H to find and replace
'Add ALT+S for settings
'Add ALT+(1-255) for adding special characters

'Menu Bar:
'This is accessed with the ESCAPE key.
'The menu bar which will take up the top row, and move all of the current text down one, will give you right now 1 option on it:
'The File Button, the user can move around these buttons by using the arrow keys left/right and once you press for example right at the end
'the selected button will wrap-around to the left-most, in this file button there will be 4 additional buttons in a dropdown menu:
'New - deletes everything in the current arrays and starts a new file. if your in an unsaved file then we prompt the user: do you wanna create a new file without saving?
'Save - saves the file at the given directory with the given name.
'Save As - lets you save a file by overwriting an existing .txt file.
'Open - Opens a .txt file.

'Fix some wierd bugs in PositionLimits.
'FIx some selection bugs in TextSelection.
'Fix some wierd stuff with copy and paste and key functions.


Screen 13
_FullScreen
_Title "Text Editor: v0w15"
_MouseHide
_MouseMove 1, 1

'$include: 'KeyConst.bas'

Const True = -1
Const False = 0

Dim Shared FirstLoop As Integer: FirstLoop = False
Dim Shared Insert As Integer: Insert = False
Dim Shared Selected As Integer: Selected = False
Dim Shared Menu As Integer: Menu = False

Dim Shared KeyAscii As Integer
Dim Shared KeyPress As String

Dim Shared TextRowOffset As Integer
Dim Shared NewLine As String
Dim Shared SelectedText As String
Dim Shared Lines(32767) As String
Dim Shared UsedLines As Integer: UsedLines = 1
Dim Shared CurrentPosition As Coordinates
Dim Shared SelectedPositions(2) As Coordinates
Dim Shared PSelectedPositions(2) As Coordinates
CurrentPosition.X = 1: CurrentPosition.Y = 1
SelectedPosition.X = 0: SelectedPosition.Y = 0
NewLine = Chr$(13) + Chr$(10)

Type Coordinates
    X As Integer
    Y As Integer
End Type

Color 15, 0
Do: _Limit 1000

    If (Menu = True) Then: TextRowOffset = 1
    For Row = 1 To 25
        If (Row + TextRowOffset <= 25) Then
            For Column = 1 To 40
                RowOffset = PositionOffset(CurrentPosition, 1)
                ColumnOffset = PositionOffset(CurrentPosition, 0)
                If (TextRowOffset < 1) Then
                    If (Selected = True) Then
                        If IsSelected(Row + RowOffset, Column + ColumnOffset) Then: Color 0, 14
                    Else
                        Color 15, 0
                    End If
                Else
                    Color 20, 0
                End If
                Locate Row + TextRowOffset, Column
                Print Mid$(Lines(Row + RowOffset), Column + ColumnOffset, 1);
                If (Selected = True) Then
                    Color 15, 0
                End If
            Next Column
        End If
    Next Row
    Call DrawCursor(TextRowOffset)

    If (Menu = True) Then
        Call MenuBar
        Menu = False
        For Row = 1 To 25
            For Column = 1 To 40
                Locate Row, Column
                Color 15, 0
                Print " ";
            Next Column
        Next Row
        TextRowOffset = 0
        FirstLoop = False
    End If

    KeyPress = InKey$
    If Len(KeyPress) > 0 Then
        KeyAscii = ASCII_16(KeyPress)
    Else
        KeyAscii = 0
    End If
    If ((KeyAscii <> 0) Or (FirstLoop = False)) Then
        FirstLoop = True
        Call KeyboardInput(KeyAscii)

        PositionLimits CurrentPosition, UsedLines, Lines(), Selected

        TextSelection CurrentPosition, PSelectedPositions(), SelectedPositions(), Selected, KeyAscii

        If (KeyAscii = KeyCTRLA) Then
            Selected = True
            SelectedPositions(1).X = 1
            SelectedPositions(1).Y = 1
            SelectedPositions(2).Y = UsedLines
            SelectedPositions(2).X = Len(Lines(SelectedPositions(2).Y)) + 1
            CurrentPosition.X = SelectedPositions(2).X
            CurrentPosition.Y = SelectedPositions(2).Y
        End If
        Cls
    End If

    _Display
Loop

'$include: 'UI.bas'

Function LastPointFunc (CurrentPosition() As Coordinates, PreviousPosition() As Coordinates)
    If (CurrentPosition(1).X <> PreviousPosition(1).X) Or (CurrentPosition(1).Y <> PreviousPosition(1).Y) Then: LastPointFunc = 1
    If (CurrentPosition(2).X <> PreviousPosition(2).X) Or (CurrentPosition(2).Y <> PreviousPosition(2).Y) Then: LastPointFunc = 2
End Function

Function IsSelected (Row, Column)
    FuncReturn = False
    If (((Column) >= SelectedPositions(1).X) Or ((SelectedPositions(1).Y <> SelectedPositions(2).Y) And ((Row) > SelectedPositions(1).Y))) Then
        If (((Column) <= SelectedPositions(2).X) Or ((SelectedPositions(1).Y <> SelectedPositions(2).Y) And ((Row) <> SelectedPositions(2).Y))) Then
            If ((Row) >= SelectedPositions(1).Y And ((Row) <= SelectedPositions(2).Y)) Then
                FuncReturn = True
            End If
        End If
    End If
    IsSelected = FuncReturn
End Function

Function ASCII_16 (K As String)
    Dim A As Long
    A = Asc(Mid$(K, 1, 1))
    If Len(K) = 2 Then
        A = A + (Asc(Mid$(K, 2, 1)) * 256)
    End If
    ASCII_16 = A
End Function

Function AddCharacter$ (KeyPress As String)
    If (Insert = False) Then
        RightTemp$ = Right$(Lines(CurrentPosition.Y), (Len(Lines(CurrentPosition.Y)) - (CurrentPosition.X - 1)))
        LeftTemp$ = Left$(Lines(CurrentPosition.Y), CurrentPosition.X - 1)
        CurrentPosition.X = CurrentPosition.X + 1
        AddCharacter$ = (LeftTemp$ + KeyPress + RightTemp$)
    Else
        Temp$ = Lines(CurrentPosition.Y)
        Mid$(Temp$, CurrentPosition.X, 1) = KeyPress
        If Mid$(Temp$, CurrentPosition.X, 1) <> "" Then
            CurrentPosition.X = CurrentPosition.X + 1
        End If
        AddCharacter$ = (Temp$)
    End If
End Function

Function CursorPosition (Position As Coordinates, ReturnType As Integer)
    If ReturnType = 1 Then
        CursorRow = Position.Y
        If CursorRow > 25 Then: CursorRow = 25
        CursorPosition = CursorRow
    Else
        CursorColumn = Position.X
        If CursorColumn > 40 Then: CursorColumn = 40
        CursorPosition = CursorColumn
    End If
End Function

Function PositionOffset (Position As Coordinates, ReturnType As Integer)
    If ReturnType = 1 Then
        RowOffset = Position.Y - 25
        If RowOffset < 0 Then: RowOffset = 0
        PositionOffset = RowOffset
    Else
        ColumnOffset = Position.X - 40
        If ColumnOffset < 0 Then: ColumnOffset = 0
        PositionOffset = ColumnOffset
    End If
End Function

Function NotVal (Value As Integer)
    If (Value = True) Then
        NotVal = False
    End If
    If (Value = False) Then
        NotVal = True
    End If
End Function


Sub DrawCursor (TextRowOffset As Integer)
    CursorColumn = CursorPosition(CurrentPosition, 0)
    CursorRow = CursorPosition(CurrentPosition, 1) + TextRowOffset
    CursorColor = 15
    If IsSelected(CursorRow, CursorColumn) Then: CursorColor = 0
    If (Insert = False) Then
        Line ((CursorColumn - 1) * 8, (CursorRow * 8) - 1)-(((CursorColumn - 1) * 8) + 6, (CursorRow * 8) - 1), CursorColor
    Else
        For Row = 1 To 8
            Line (((CursorColumn - 1) * 8), (((CursorRow - 1) * 8) + (Row - 1)))-((((CursorColumn - 1) * 8) + 6), (((CursorRow - 1) * 8) + (Row - 1))), CursorColor
        Next Row
    End If
End Sub

Sub PositionLimits (Position As Coordinates, AmountOfLines As Integer, TextLines() As String, Selected As Integer)
    If (Position.Y < 1) Then: Position.Y = 1
    If (Position.Y > AmountOfLines) Then: Position.Y = AmountOfLines
    If (Position.X < 1) Then
        If (Position.Y > 1) Then
            Position.Y = Position.Y - 1
            If (Selected = True) Then
                Position.X = Len(TextLines(Position.Y))
            ElseIf (Selected = False) Then
                Position.X = Len(TextLines(Position.Y)) + 1
            End If
        Else
            Position.X = 1
        End If
    End If
    If (Selected = True) Then
        If (Position.X > Len(TextLines(Position.Y))) Then
            If (Position.Y + 1 <= AmountOfLines) Then
                Position.Y = Position.Y + 1
                Position.X = 1
            Else
                Position.X = Len(TextLines(Position.Y))
            End If
        End If
    ElseIf (Selected = False) Then
        If (Position.X > Len(TextLines(Position.Y)) + 1) Then
            If (Position.Y + 1 <= AmountOfLines) Then
                Position.Y = Position.Y + 1
                Position.X = 1
            Else
                Position.X = Len(TextLines(Position.Y)) + 1
            End If
        End If
    End If
End Sub

Sub TextSelection (CharacterPosition As Coordinates, PreviousPositions( 2) As Coordinates, CurrentPositions( 2) As Coordinates, Selected As Integer, KeyAscii As Integer)
    Static ExtraLastPoint As Integer
    Static LastPoint As Integer
    Static LastKey As Integer
    Static FirstSelect As Integer

    If (_KeyDown(KeyRShift)) Or (_KeyDown(KeyLShift)) Then
        PreviousPositions(1).X = CurrentPositions(1).X
        PreviousPositions(1).Y = CurrentPositions(1).Y
        PreviousPositions(2).X = CurrentPositions(2).X
        PreviousPositions(2).Y = CurrentPositions(2).Y
        If (KeyAscii = KeyLeft) Then
            If (Selected = False) Then
                Selected = True
                FirstSelect = True
                CurrentPositions(1).X = CharacterPosition.X
                CurrentPositions(1).Y = CurrentPosition.Y
                CurrentPositions(2).X = CharacterPosition.X + 1
                CurrentPositions(2).Y = CharacterPosition.Y
            Else
                FirstSelect = False
                If (LastKey = KeyUp) Then
                    CurrentPositions(1).X = CharacterPosition.X
                    CurrentPositions(1).Y = CharacterPosition.Y
                ElseIf (LastKey = KeyDown) Then
                    CurrentPositions(2).X = CharacterPosition.X
                    CurrentPositions(2).Y = CharacterPosition.Y
                ElseIf (LastKey = KeyRight) Then
                    If (LastPoint = 1) Then
                        CurrentPositions(1).X = CharacterPosition.X
                        CurrentPositions(1).Y = CharacterPosition.Y
                    ElseIf (LastPoint = 2) Then
                        CurrentPositions(2).X = CharacterPosition.X
                        CurrentPositions(2).Y = CharacterPosition.Y
                    End If
                ElseIf (LastKey = KeyLeft) Then
                    If (CurrentPositions(1).X <> CurrentPositions(2).X) Then
                        If (LastPoint = 1) Then
                            CurrentPositions(1).X = CharacterPosition.X
                            CurrentPositions(1).Y = CharacterPosition.Y
                        ElseIf (LastPoint = 2) Then
                            CurrentPositions(2).X = CharacterPosition.X
                            CurrentPositions(2).Y = CharacterPosition.Y
                        End If
                    ElseIf (CurrentPositions(1).X = CurrentPositions(2).X) And (CurrentPositions(1).Y = CurrentPositions(2).Y) Then
                        CurrentPositions(1).X = CharacterPosition.X
                        CurrentPositions(1).Y = CharacterPosition.Y
                    Else
                        If (LastPoint = 1) Then
                            CurrentPositions(1).X = CharacterPosition.X
                            CurrentPositions(1).Y = CharacterPosition.Y
                        ElseIf (LastPoint = 2) Then
                            CurrentPositions(2).X = CharacterPosition.X
                            CurrentPositions(2).Y = CharacterPosition.Y
                        End If
                    End If
                Else
                End If
            End If
        ElseIf (KeyAscii = KeyRight) Then
            If (Selected = False) Then
                Selected = True
                FirstSelect = True
                CurrentPositions(1).X = CharacterPosition.X - 1
                CurrentPositions(1).Y = CharacterPosition.Y
                CurrentPositions(2).X = CharacterPosition.X
                CurrentPositions(2).Y = CharacterPosition.Y
            Else
                FirstSelect = False
                If (LastKey = KeyUp) Then
                    CurrentPositions(1).X = CharacterPosition.X
                    CurrentPositions(1).Y = CharacterPosition.Y
                ElseIf (LastKey = KeyDown) Then
                    CurrentPositions(2).X = CharacterPosition.X
                    CurrentPositions(2).Y = CharacterPosition.Y
                ElseIf (LastKey = KeyRight) Then
                    If (CurrentPositions(1).X <> CurrentPositions(2).X) Then
                        If (LastPoint = 1) Then
                            CurrentPositions(1).X = CharacterPosition.X
                            CurrentPositions(1).Y = CharacterPosition.Y
                        ElseIf (LastPoint = 2) Then
                            CurrentPositions(2).X = CharacterPosition.X
                            CurrentPositions(2).Y = CharacterPosition.Y
                        End If
                    ElseIf (CurrentPositions(1).X = CurrentPositions(2).X) And (CurrentPositions(1).Y = CurrentPositions(2).Y) Then
                        CurrentPositions(2).X = CharacterPosition.X
                        CurrentPositions(2).Y = CharacterPosition.Y
                    Else
                        If (LastPoint = 1) Then
                            CurrentPositions(1).X = CharacterPosition.X
                            CurrentPositions(1).Y = CharacterPosition.Y
                        ElseIf (LastPoint = 2) Then
                            CurrentPositions(2).X = CharacterPosition.X
                            CurrentPositions(2).Y = CharacterPosition.Y
                        End If
                    End If
                ElseIf (LastKey = KeyLeft) Then
                    If (LastPoint = 1) Then
                        CurrentPositions(1).X = CharacterPosition.X
                        CurrentPositions(1).Y = CharacterPosition.Y
                    ElseIf (LastPoint = 2) Then
                        CurrentPositions(2).X = CharacterPosition.X
                        CurrentPositions(2).Y = CharacterPosition.Y
                    End If
                Else
                End If
            End If
        ElseIf (KeyAscii = KeyUp) Then
            If (Selected = False) Then
                Selected = True
                FirstSelect = True
                CurrentPositions(1).X = CharacterPosition.X
                CurrentPositions(1).Y = CharacterPosition.Y
                CurrentPositions(2).X = CharacterPosition.X
                CurrentPositions(2).Y = CharacterPosition.Y + 1
            Else
                FirstSelect = False
                If (LastKey = KeyUp) Then
                    If (LastPoint = 1) Then
                        CurrentPositions(1).X = CharacterPosition.X
                        CurrentPositions(1).Y = CharacterPosition.Y
                    ElseIf (LastPoint = 2) Then
                        CurrentPositions(1).X = CharacterPosition.X
                        CurrentPositions(1).Y = CharacterPosition.Y
                    End If
                ElseIf (LastKey = KeyDown) Then
                    CurrentPositions(2).X = CharacterPosition.X
                    CurrentPositions(2).Y = CharacterPosition.Y
                ElseIf (LastKey = KeyRight) Then
                    If (LastPoint = 1) Then
                        CurrentPositions(1).X = CharacterPosition.X
                        CurrentPositions(1).Y = CharacterPosition.Y
                    ElseIf (LastPoint = 2) Then
                        CurrentPositions(2).X = CharacterPosition.X
                        CurrentPositions(2).Y = CharacterPosition.Y
                    End If
                ElseIf (LastKey = KeyLeft) Then
                    If (LastPoint = 1) Then
                        CurrentPositions(1).X = CharacterPosition.X
                        CurrentPositions(1).Y = CharacterPosition.Y
                    ElseIf (LastPoint = 2) Then
                        CurrentPositions(1).X = CharacterPosition.X
                        CurrentPositions(1).Y = CharacterPosition.Y
                    End If
                Else
                End If
            End If
        ElseIf (KeyAscii = KeyDown) Then
            If (Selected = False) Then
                Selected = True
                FirstSelect = True
                CurrentPositions(1).X = CharacterPosition.X
                CurrentPositions(1).Y = CharacterPosition.Y - 1
                CurrentPositions(2).X = CharacterPosition.X
                CurrentPositions(2).Y = CharacterPosition.Y
            Else
                FirstSelect = False
                If (LastKey = KeyUp) Then
                    If (LastPoint = 1) Then
                        CurrentPositions(1).X = CharacterPosition.X
                        CurrentPositions(1).Y = CharacterPosition.Y
                    ElseIf (LastPoint = 2) Then
                        CurrentPositions(2).X = CharacterPosition.X
                        CurrentPositions(2).Y = CharacterPosition.Y
                    End If
                ElseIf (LastKey = KeyDown) Then
                    If (LastPoint = 1) Then
                        CurrentPositions(1).X = CharacterPosition.X
                        CurrentPositions(1).Y = CharacterPosition.Y
                    ElseIf (LastPoint = 2) Then
                        CurrentPositions(2).X = CharacterPosition.X
                        CurrentPositions(2).Y = CharacterPosition.Y
                    End If
                ElseIf (LastKey = KeyRight) Then
                    If (LastPoint = 1) Then
                        CurrentPositions(1).X = CharacterPosition.X
                        CurrentPositions(1).Y = CharacterPosition.Y
                    ElseIf (LastPoint = 2) Then
                        CurrentPositions(2).X = CharacterPosition.X
                        CurrentPositions(2).Y = CharacterPosition.Y
                    End If
                ElseIf (LastKey = KeyLeft) Then
                    If (LastPoint = 1) Then
                        CurrentPositions(1).X = CharacterPosition.X
                        CurrentPositions(1).Y = CharacterPosition.Y
                    ElseIf (LastPoint = 2) Then
                        CurrentPositions(2).X = CharacterPosition.X
                        CurrentPositions(2).Y = CharacterPosition.Y
                    End If
                Else
                End If
            End If
        Else
            Selected = False
            CurrentPositions(1).X = 0
            CurrentPositions(1).Y = 0
            CurrentPositions(2).X = 0
            CurrentPositions(2).Y = 0
            PreviousPositions(1).X = 0
            PreviousPositions(1).Y = 0
            PreviousPositions(2).X = 0
            PreviousPositions(2).Y = 0
            LastKey = 0
            LastPoint = 0
            FirstSelect = False
        End If
        LastKey = KeyAscii
        If (FirstSelect = True) Then
            If (KeyAscii = KeyLeft) Then
                LastPoint = 1
            ElseIf (KeyAscii = KeyRight) Then
                LastPoint = 2
            ElseIf (KeyAscii = KeyUp) Then
                LastPoint = 1
            ElseIf (KeyAscii = KeyDown) Then
                LastPoint = 2
            End If
        ElseIf (FirstSelect = False) Then
            LastPoint = LastPointFunc(CurrentPositions(), PreviousPositions())
        End If
        If (LastPoint <> 0) Then
            ExtraLastPoint = LastPoint
        ElseIf (LastPoint = 0) Then
            LastPoint = ExtraLastPoint
        End If
    Else
        Selected = False
        CurrentPositions(1).X = 0
        CurrentPositions(1).Y = 0
        CurrentPositions(2).X = 0
        CurrentPositions(2).Y = 0
        PreviousPositions(1).X = 0
        PreviousPositions(1).Y = 0
        PreviousPositions(2).X = 0
        PreviousPositions(2).Y = 0
        LastKey = 0
        LastPoint = 0
        FirstSelect = False
    End If
End Sub

Sub KeyboardInput (KeyAscii As Integer)
    If (KeyAscii = KeyCTRLC) Then
        SelectedText = ""
        If (Selected = True) Then
            For Row = SelectedPositions(1).Y To SelectedPositions(2).Y
                TempLine$ = ""
                For Column = 1 To Len(Lines(CurrentPosition.Y))
                    If IsSelected(Row, Column) Then
                        TempLine$ = TempLine$ + (Mid$(Lines(Row), Column, 1))
                    End If
                Next Column
                SelectedText = SelectedText + TempLine$
                If (SelectedPositions(1).Y <> SelectedPositions(2).Y) Then: SelectedText = SelectedText + NewLine
            Next Row
            _Clipboard$ = SelectedText
        End If
    End If
    If (KeyAscii = KeyCTRLV) Then
        If (Selected = True) Then: DeleteSelected (False)
        If (_Clipboard$ = "") Then: Exit Sub
        TempClip$ = _Clipboard$
        TempRight$ = ""
        Temp$ = ""
        LineNum = 0
        Char = 0
        Do
            LineNum = LineNum + 1
            Char = 0
            Do
                Char = Char + 1
            Loop Until ((Mid$(TempClip$, Char, 1) = Chr$(13)) And (Mid$(TempClip$, Char + 1, 1) = Chr$(10))) Or (Char = Len(TempClip$))
            Temp$ = Left$(TempClip$, Char + 1)
            If (Asc(Left$(Temp$, 1)) = 10) Then: Temp$ = Right$(Temp$, Len(Temp$) - 1)
            If LineNum = 1 Then
                TempRight$ = Right$(Lines((CurrentPosition.Y - 1) + LineNum), ((Len(Lines((CurrentPosition.Y - 1) + LineNum))) - (CurrentPosition.X - 1)))
                Lines((CurrentPosition.Y - 1) + LineNum) = Left$(Lines((CurrentPosition.Y - 1) + LineNum), (CurrentPosition.X - 1)) + Temp$
            Else
                Lines((CurrentPosition.Y - 1) + LineNum) = Temp$
            End If
            If (Char = Len(TempClip$)) Then: Exit Do
            For A = (UsedLines + 2) To ((CurrentPosition.Y + LineNum) + 1) Step -1
                Lines(A) = Lines(A - 1)
                Lines(A - 1) = ""
            Next A
            TempClip$ = Right$(TempClip$, Len(TempClip$) - Char)
        Loop
        UsedLines = UsedLines + LineNum
        CurrentPosition.X = Len(Lines(CurrentPosition.Y)) + 1
        Lines((CurrentPosition.Y - 1) + LineNum) = Lines((CurrentPosition.Y - 1) + LineNum) + TempRight$
    End If
    If (KeyAscii = KeyCTRLX) Then
        If (Selected = True) Then
            KeyboardInput (KeyCTRLC)
            DeleteSelected (True)
        End If
    End If
    If (KeyAscii = KeyBackspace) Then
        If (Selected = False) Then
            If (CurrentPosition.X > 1) Then
                Lines(CurrentPosition.Y) = Left$(Lines(CurrentPosition.Y), (CurrentPosition.X - 2)) + Right$(Lines(CurrentPosition.Y), (Len(Lines(CurrentPosition.Y)) - (CurrentPosition.X - 1)))
                CurrentPosition.X = CurrentPosition.X - 1
            Else
                If CurrentPosition.Y > 1 Then
                    CurrentPosition.Y = CurrentPosition.Y - 1
                    CurrentPosition.X = Len(Lines(CurrentPosition.Y)) + 1
                    Lines(CurrentPosition.Y) = Lines(CurrentPosition.Y) + Lines(CurrentPosition.Y + 1)
                    Lines(CurrentPosition.Y + 1) = ""
                    For A = CurrentPosition.Y + 1 To UsedLines
                        Lines(A) = Lines(A + 1)
                        Lines(A + 1) = ""
                    Next A
                    UsedLines = UsedLines - 1
                End If
            End If
        Else
            DeleteSelected (True)
        End If
    End If
    If (KeyAscii = KeyTab) Then
        If (Selected = True) Then: DeleteSelected (True)
        Lines(CurrentPosition.Y) = AddCharacter$("    ")
        CurrentPosition.X = CurrentPosition.X + 3
    End If
    If (KeyAscii = KeyEnter) Then
        TempColumn = CurrentPosition.X
        TempRow = CurrentPosition.Y '
        CurrentPosition.X = 1
        CurrentPosition.Y = CurrentPosition.Y + 1
        UsedLines = UsedLines + 1
        For A = (UsedLines + 1) To (CurrentPosition.Y + 1) Step -1
            Lines(A) = Lines(A - 1)
            Lines(A - 1) = ""
        Next A
        If Right$(Lines(TempRow), (Len(Lines(TempRow)) - (TempColumn - 1))) <> "" Then
            Lines(TempRow + 1) = Right$(Lines(TempRow), (Len(Lines(TempRow)) - (TempColumn - 1)))
            Lines(TempRow) = Left$(Lines(TempRow), TempColumn - 1)
        End If
    End If
    If (KeyAscii = KeyPageUp) Then: CurrentPosition.Y = CurrentPosition.Y - 25
    If (KeyAscii = KeyPageDown) Then: CurrentPosition.Y = CurrentPosition.Y + 25
    If ((KeyAscii >= 32) And (KeyAscii <= 126)) Then
        If (Selected = True) Then: DeleteSelected (True)
        Lines(CurrentPosition.Y) = AddCharacter$(KeyPress)
    End If
    If (KeyAscii = KeyHome) Then: CurrentPosition.X = 1
    If (KeyAscii = KeyLeft) Then
        CurrentPosition.X = CurrentPosition.X - 1
    ElseIf (KeyAscii = KeyRight) Then
        CurrentPosition.X = CurrentPosition.X + 1
    ElseIf (KeyAscii = KeyUp) Then
        CurrentPosition.Y = CurrentPosition.Y - 1
    ElseIf (KeyAscii = KeyDown) Then
        CurrentPosition.Y = CurrentPosition.Y + 1
    End If
    If (KeyAscii = KeyEnd) Then: CurrentPosition.X = Len(Lines(CurrentPosition.Y)) + 1
    If (KeyAscii = KeyDelete) Then
        If (Selected = False) Then
            If (CurrentPosition.X = (Len(Lines(CurrentPosition.Y)) + 1)) Then
                Lines(CurrentPosition.Y) = Lines(CurrentPosition.Y) + Lines(CurrentPosition.Y + 1)
                For A = CurrentPosition.Y + 1 To UsedLines
                    Lines(A) = Lines(A + 1)
                    Lines(A + 1) = ""
                Next A
            End If
            Lines(CurrentPosition.Y) = Left$(Lines(CurrentPosition.Y), CurrentPosition.X - 1) + Right$(Lines(CurrentPosition.Y), Len(Lines(CurrentPosition.Y)) - CurrentPosition.X)
        Else
            DeleteSelected (True)
        End If
    End If
    If (KeyAscii = KeyInsert) Then
        Insert = NotVal(Insert)
    End If
    If (KeyAscii = KeyEscape) Then
        Menu = NotVal(Menu)
    End If
End Sub

Sub DeleteSelected (MoveCursor As Integer)
    For Row = SelectedPositions(1).Y To SelectedPositions(2).Y
        TempLine$ = ""
        For Column = 1 To Len(Lines(Row))
            Add = True
            If IsSelected(Row, Column) Then
                Add = False
            End If
            If (Add = True) Then: TempLine$ = TempLine$ + (Mid$(Lines(Row), Column, 1))
        Next Column
        Lines(Row) = TempLine$
    Next Row
    If (MoveCursor = True) Then
        CurentPosition.Y = SelectedPositions(1).Y
        CurrentPosition.X = (Len(Lines(SelectedPositions(1).Y)) + 1)
    End If
End Sub

Sub MenuBar

End Sub


