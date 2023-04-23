'Add Pasting over selected text.

'Add Blinking Cursor
'Add insert mode.

Screen 13
_FullScreen
_Title "Text Editor v0w8"
_MouseHide
_MouseMove 1, 1

Dim Shared Lines(32767) As String
Dim Shared UsedLines As Integer: UsedLines = 1

Dim Shared SelectedText As String
Dim Shared SelectedPoints(2) As Coordinates

Dim Shared CurrentRow As Integer: CurrentRow = 1
Dim Shared CurrentColumn As Integer: CurrentColumn = 1
Dim Shared ColumnOffset As Integer
Dim Shared RowOffset As Integer

Dim Shared CursorRow As Integer
Dim Shared CursorColumn As Integer

Dim Shared BackgroundColor As Integer: BackgroundColor = 199
Dim Shared TextColor As Integer: TextColor = 15

Dim Shared KeyAscii As Integer
Dim Shared KeyPress As String

Const True = -1
Const False = 0

Dim Shared NewLine As String: NewLine = (Chr$(13) + Chr$(10))

Dim Shared Selected As Integer: Selected = False

Dim Shared FirstLoop As Integer: FirstLoop = False

Type Coordinates
    X As Integer
    Y As Integer
End Type

Color 15, 0
Do: _Limit 1000
    For Row = 1 To 25
        For Column = 1 To 40
            If (((Column + ColumnOffset) >= SelectedPoints(1).X) Or ((SelectedPoints(1).Y <> SelectedPoints(2).Y) And ((Row + RowOffset) > SelectedPoints(1).Y))) Then
                If (((Column + ColumnOffset) <= SelectedPoints(2).X) Or ((SelectedPoints(1).Y <> SelectedPoints(2).Y) And ((Row + RowOffset) <> SelectedPoints(2).Y))) Then
                    If ((Row + RowOffset) >= SelectedPoints(1).Y And ((Row + RowOffset) <= SelectedPoints(2).Y)) Then
                        Color 0, 14
                    End If
                End If
            End If
            Locate Row, Column: Print Mid$(Lines(Row + RowOffset), Column + ColumnOffset, 1);
            Color TextColor, BackgroundColor
        Next Column
    Next Row

    CursorRow = CurrentRow
    CursorColumn = CurrentColumn
    If CursorRow > 25 Then: CursorRow = 25
    If CursorColumn > 40 Then: CursorColumn = 40
    Line ((CursorColumn - 1) * 8, (CursorRow * 8) - 1)-(((CursorColumn - 1) * 8) + 6, (CursorRow * 8) - 1), 15

    KeyPress = InKey$
    If Len(KeyPress) > 0 Then
        KeyAscii = ASCII_16(KeyPress)
    Else
        KeyAscii = 0
    End If
    If ((KeyAscii <> 0) Or (FirstLoop = False)) Then
        FirstLoop = True
        If (KeyAscii = 3) Then: KeyInput ("CTRL+C") 'CTRL+C
        If (KeyAscii = 8) Then: KeyInput ("BackSpace") 'BackSpace
        If (KeyAscii = 9) Then: KeyInput ("Tab") 'Tab
        If (KeyAscii = 13) Then: KeyInput ("Enter") 'Enter
        If (KeyAscii = 24) Then: KeyInput ("CTRL+X") 'CTRL+X
        If ((KeyAscii >= 33) And (KeyAscii <= 64)) Then: KeyInput ("C3") ' ': ; < = > ? @ 0 - 9 ! " # $ % & ' ( ) * + , - . /
        If ((KeyAscii) >= 65 And (KeyAscii <= 96)) Then: KeyInput ("C2") 'A-Z [ \ ] `
        If ((KeyAscii) >= 97 And (KeyAscii <= 126)) Then: KeyInput ("C1") 'a-z { | } ~
        If (KeyAscii = 32) Then: KeyInput ("Space") 'Space
        If (KeyAscii = 18176) Then: KeyInput ("Home") 'Home
        If ((KeyAscii = 18432) And (CurrentRow > 1)) Then: KeyInput ("Up") 'Up
        If (KeyAscii = 18688) Then: KeyInput ("Page Up") 'Page Up
        If (KeyAscii = 19200) Then: KeyInput ("Left") 'Left
        If (KeyAscii = 19712) Then: KeyInput ("Right") 'Right
        If (KeyAscii = 20224) Then: KeyInput ("End") 'End
        If ((KeyAscii = 20480) And ((CurrentRow + 1) <= UsedLines)) Then: KeyInput ("Down") 'Down
        If (KeyAscii = 20736) Then: KeyInput ("Page Down") 'Page Down
        If (KeyAscii = 21248) Then: KeyInput ("Delete") 'Delete

        SelectionKeyCombinations

        If (KeyAscii = 1) Then: KeyInput ("CTRL+A")
        If (KeyAscii = 22) Then: KeyInput ("CTRL+V")

        ColumnOffset = CurrentColumn - 40
        RowOffset = CurrentRow - 25
        If ColumnOffset < 0 Then: ColumnOffset = 0
        If RowOffset < 0 Then: RowOffset = 0
        Cls
    End If
    _Display
Loop

Function ASCII_16 (K As String)
    Dim A As Long
    A = Asc(Mid$(K, 1, 1))
    If Len(K) = 2 Then
        A = A + (Asc(Mid$(K, 2, 1)) * 256)
    End If
    ASCII_16 = A
End Function

Function AddCharacter$ (KeyPress As String)
    RightTemp$ = Right$(Lines(CurrentRow), (Len(Lines(CurrentRow)) - (CurrentColumn - 1)))
    LeftTemp$ = Left$(Lines(CurrentRow), CurrentColumn - 1)
    CurrentColumn = CurrentColumn + 1
    AddCharacter$ = (LeftTemp$ + KeyPress + RightTemp$)
End Function

Sub SelectionKeyCombinations
    If ((_KeyDown(100304)) Or (_KeyDown(100303))) Then 'L/R Shift
        If (KeyAscii = 19200) Then 'Left
            If (Selected = False) Then
                Selected = True
                SelectedPoints(1).X = CurrentColumn
                SelectedPoints(1).Y = CurrentRow
                SelectedPoints(2).X = CurrentColumn + 1
                SelectedPoints(2).Y = CurrentRow
            Else
                If (PastPoint = 1) Then
                    SelectedPoints(1).X = CurrentColumn
                    SelectedPoints(1).Y = CurrentRow
                ElseIf (PastPoint = 2) Then
                    SelectedPoints(2).X = CurrentColumn
                    SelectedPoints(2).Y = CurrentRow
                End If
            End If
        End If
        If (KeyAscii = 19712) Then 'Right
            If (Selected = False) Then
                Selected = True
                SelectedPoints(1).X = CurrentColumn - 1
                SelectedPoints(1).Y = CurrentRow
                SelectedPoints(2).X = CurrentColumn
                SelectedPoints(2).Y = CurrentRow
            Else
                If (PastPoint = 1) Then
                    SelectedPoints(1).X = CurrentColumn
                    SelectedPoints(1).Y = CurrentRow
                ElseIf (PastPoint = 2) Then
                    SelectedPoints(2).X = CurrentColumn
                    SelectedPoints(2).Y = CurrentRow
                End If
            End If
        End If
        If (KeyAscii = 18432) Then 'Up
            If (Selected = False) Then
                Selected = True
                SelectedPoints(1).X = CurrentColumn
                SelectedPoints(1).Y = CurrentRow
                SelectedPoints(2).X = CurrentColumn
                SelectedPoints(2).Y = CurrentRow + 1
            Else
                If (PastPoint = 1) Then
                    SelectedPoints(1).X = CurrentColumn
                    SelectedPoints(1).Y = CurrentRow
                ElseIf (PastPoint = 2) Then
                    SelectedPoints(2).X = CurrentColumn
                    SelectedPoints(2).Y = CurrentRow
                End If
            End If
        End If
        If (KeyAscii = 20480) Then 'Down
            If (Selected = False) Then
                Selected = True
                SelectedPoints(1).X = CurrentColumn
                SelectedPoints(1).Y = CurrentRow - 1
                SelectedPoints(2).X = CurrentColumn
                SelectedPoints(2).Y = CurrentRow
            Else
                If (PastPoint = 1) Then
                    SelectedPoints(1).X = CurrentColumn
                    SelectedPoints(1).Y = CurrentRow
                ElseIf (PastPoint = 2) Then
                    SelectedPoints(2).X = CurrentColumn
                    SelectedPoints(2).Y = CurrentRow
                End If
            End If
        End If
    Else
        If (Selected = True) Then
            SelectedPoints(1).X = 0
            SelectedPoints(1).Y = 0
            SelectedPoints(2).X = 0
            SelectedPoints(2).Y = 0
            Selected = False
        End If
    End If
End Sub

Sub DeleteSelected
    TempLine$ = ""
    For Row = SelectedPoints(1).Y To SelectedPoints(2).Y
        For Column = 1 To Len(Lines(Row))
            Add = True
            If (((Column) >= SelectedPoints(1).X) Or ((SelectedPoints(1).Y <> SelectedPoints(2).Y) And ((Row) > SelectedPoints(1).Y))) Then
                If (((Column) <= SelectedPoints(2).X) Or ((SelectedPoints(1).Y <> SelectedPoints(2).Y) And ((Row) <> SelectedPoints(2).Y))) Then
                    If ((Row) >= SelectedPoints(1).Y And ((Row) <= SelectedPoints(2).Y)) Then
                        Add = False
                    End If
                End If
            End If
            If Add = True Then: TempLine$ = TempLine$ + (Mid$(Lines(Row), Column, 1))
        Next Column
        Lines(Row) = TempLine$
        TempLine$ = ""
    Next Row
    CurrentRow = SelectedPoints(1).Y
    CurrentColumn = (Len(Lines(SelectedPoints(1).Y)) + 1)
End Sub

Sub KeyInput (IKey As String)
    If (IKey = "Home") Then: CurrentColumn = 1
    If (IKey = "End") Then: CurrentColumn = (Len(Lines(CurrentRow)) + 1)
    If (IKey = "Delete") Then
        If (Selected = False) Then
            If (CurrentColumn = (Len(Lines(CurrentRow)) + 1)) Then
                Lines(CurrentRow) = Lines(CurrentRow) + Lines(CurrentRow + 1)
                For A = CurrentRow + 1 To UsedLines
                    Lines(A) = Lines(A + 1)
                    Lines(A + 1) = ""
                Next A
            End If
            Lines(CurrentRow) = Left$(Lines(CurrentRow), CurrentColumn - 1) + Right$(Lines(CurrentRow), Len(Lines(CurrentRow)) - CurrentColumn)
        Else
            DeleteSelected
        End If
    End If
    If (IKey = "Page Up") Then 'Page Up
        CurrentRow = CurrentRow - 25
        If CurrentRow < 1 Then: CurrentRow = 1
    End If
    If (IKey = "Page Down") Then 'Page Down
        CurrentRow = CurrentRow + 25
        If CurrentRow > UsedLines Then: UsedLines = CurrentRow
    End If
    If ((IKey = "Up") And (CurrentRow > 1)) Then 'Up
        CurrentRow = CurrentRow - 1
        If ((CurrentColumn + 1) > Len(Lines(CurrentRow + RowOffset))) Then
            CurrentColumn = Len(Lines(CurrentRow + RowOffset)) + 1
        End If
    End If
    If ((IKey = "Down") And ((CurrentRow + 1) <= UsedLines)) Then 'Down
        CurrentRow = CurrentRow + 1
        If ((CurrentColumn + 1) > Len(Lines(CurrentRow + RowOffset))) Then
            CurrentColumn = Len(Lines(CurrentRow + RowOffset)) + 1
        End If
    End If
    If (IKey = "Left") Then 'Left
        If (CurrentColumn > 1) Then
            CurrentColumn = CurrentColumn - 1
        Else
            If (CurrentRow > 1) Then
                CurrentRow = CurrentRow - 1
                CurrentColumn = Len(Lines(CurrentRow)) + 1
            End If
        End If
    End If
    If (IKey = "Right") Then 'Right
        If Mid$(Lines(CurrentRow), (CurrentColumn), 1) <> "" Then
            CurrentColumn = CurrentColumn + 1
        Else
            If ((CurrentRow + 1) <= UsedLines) Then
                CurrentRow = CurrentRow + 1
                CurrentColumn = 1
            End If
        End If
    End If
    If (IKey = "C1") Then 'a-z { | } ~
        If Selected = True Then: DeleteSelected
        Lines(CurrentRow) = AddCharacter$(KeyPress)
    End If
    If (IKey = "C2") Then 'A-Z [ \ ] `
        If Selected = True Then: DeleteSelected
        Lines(CurrentRow) = AddCharacter$(KeyPress)
    End If
    If (IKey = "C3") Then ' ': ; < = > ? @ 0 - 9 ! " # $ % & ' ( ) * + , - . /
        If Selected = True Then: DeleteSelected
        Lines(CurrentRow) = AddCharacter$(KeyPress)
    End If
    If (IKey = "Space") Then 'Space
        If Selected = True Then: DeleteSelected
        Lines(CurrentRow) = AddCharacter$(KeyPress)
    End If
    If (IKey = "Enter") Then 'Enter
        TempColumn = CurrentColumn
        TempRow = CurrentRow
        CurrentColumn = 1
        CurrentRow = CurrentRow + 1
        UsedLines = UsedLines + 1
        For A = (UsedLines + 1) To (CurrentRow + 1) Step -1
            Lines(A) = Lines(A - 1)
            Lines(A - 1) = ""
        Next A
        If Right$(Lines(TempRow), (Len(Lines(TempRow)) - (TempColumn - 1))) <> "" Then
            Lines(TempRow + 1) = Right$(Lines(TempRow), (Len(Lines(TempRow)) - (TempColumn - 1)))
            Lines(TempRow) = Left$(Lines(TempRow), TempColumn - 1)
        End If
    End If
    If (IKey = "Tab") Then: Lines(CurrentRow) = AddCharacter$("    "): CurrentColumn = CurrentColumn + 3 'Tab
    If (IKey = "BackSpace") Then 'BackSpace
        If Selected = False Then
            If (CurrentColumn > 1) Then
                Lines(CurrentRow) = Left$(Lines(CurrentRow), (CurrentColumn - 2)) + Right$(Lines(CurrentRow), (Len(Lines(CurrentRow)) - (CurrentColumn - 1)))
                CurrentColumn = CurrentColumn - 1
            Else
                If CurrentRow > 1 Then
                    CurrentRow = CurrentRow - 1
                    CurrentColumn = Len(Lines(CurrentRow)) + 1
                    Lines(CurrentRow) = Lines(CurrentRow) + Lines(CurrentRow + 1)
                    Lines(CurrentRow + 1) = ""
                    For A = CurrentRow + 1 To UsedLines
                        Lines(A) = Lines(A + 1)
                        Lines(A + 1) = ""
                    Next A
                    UsedLines = UsedLines - 1
                End If
            End If
        Else
            DeleteSelected
        End If
    End If
    If (IKey = "CTRL+A") Then
        Selected = True
        SelectedPoints(1).X = 1
        SelectedPoints(1).Y = 1
        SelectedPoints(2).X = Len(Lines(UsedLines))
        SelectedPoints(2).Y = UsedLines
        CurrentColumn = Len(Lines(UsedLines)) + 1
        CurrentRow = UsedLines
    End If
    If (IKey = "CTRL+C") Then
        If Selected = True Then
            For Row = SelectedPoints(1).Y To SelectedPoints(2).Y
                TempLine$ = ""
                For Column = 1 To Len(Lines(Row))
                    If (((Column + ColumnOffset) >= SelectedPoints(1).X) Or ((SelectedPoints(1).Y <> SelectedPoints(2).Y) And ((Row + RowOffset) > SelectedPoints(1).Y))) Then
                        If (((Column + ColumnOffset) <= SelectedPoints(2).X) Or ((SelectedPoints(1).Y <> SelectedPoints(2).Y) And ((Row + RowOffset) <> SelectedPoints(2).Y))) Then
                            If ((Row + RowOffset) >= SelectedPoints(1).Y And ((Row + RowOffset) <= SelectedPoints(2).Y)) Then
                                TempLine$ = TempLine$ + (Mid$(Lines(Row), Column, 1))
                            End If
                        End If
                    End If
                Next Column
                SelectedText = SelectedText + TempLine$
                If (SelectedPoints(1).Y <> SelectedPoints(2).Y) Then: SelectedText = SelectedText + NewLine
            Next Row
            _Clipboard$ = ""
            _Clipboard$ = SelectedText
            SelectedText = ""
        End If
    End If
    If (IKey = "CTRL+X") Then
        If Selected = True Then
            KeyInput ("CTRL+C")
            DeleteSelected
        End If
    End If
    If (IKey = "CTRL+V") Then
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
                TempRight$ = Right$(Lines((CurrentRow - 1) + LineNum), ((Len(Lines((CurrentRow - 1) + LineNum))) - (CurrentColumn - 1)))
                Lines((CurrentRow - 1) + LineNum) = Left$(Lines((CurrentRow - 1) + LineNum), (CurrentColumn - 1)) + Temp$
            Else
                Lines((CurrentRow - 1) + LineNum) = Temp$
            End If
            If (Char = Len(TempClip$)) Then: Exit Do
            For A = (UsedLines + 2) To ((CurrentRow + LineNum) + 1) Step -1
                Lines(A) = Lines(A - 1)
                Lines(A - 1) = ""
            Next A
            TempClip$ = Right$(TempClip$, Len(TempClip$) - Char)
        Loop
        UsedLines = UsedLines + LineNum
        CurrentColumn = Len(Lines(CurrentRow))
        Lines((CurrentRow - 1) + LineNum) = Lines((CurrentRow - 1) + LineNum) + TempRight$
    End If
End Sub

Function PastPoint
    CC = CurrentColumn
    CR = CurrentRow
    FuncReturn = 0
    If ((CC <= SelectedPoints(2).X) And (CR <= SelectedPoints(2).Y)) Then
        FuncReturn = 1
    ElseIf ((CC >= SelectedPoints(2).X) And (CR >= SelectedPoints(2).Y)) Then
        FuncReturn = 2
    End If
    PastPoint = FuncReturn
End Function
