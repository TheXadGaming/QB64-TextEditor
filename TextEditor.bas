'Add Deleteing on selected text
'Add that for Backspace
'Add replace selected text with character pressed
'Add CTRL+V with multi line
'Add CTRL+C copy with multi line

Screen 13
_FullScreen
_Title "Text Editor v0w5"
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

Dim Shared Clipboard As String

Dim Shared KeyAscii As Integer
Dim Shared KeyPress As String

Const True = -1
Const False = 0

Dim Shared SyntaxHighlighting As Integer: SyntaxHighlighting = True
Dim Shared Selected As Integer: Selected = False
Dim Shared SquareSelected As Integer: SquareSelected = False

Dim Shared FirstLoop As Integer: FirstLoop = False

Type Coordinates
    X As Integer
    Y As Integer
End Type

Color 15, 0
Do: _Limit 1000
    For Row = 1 To 25
        For Column = 1 To 40
            If SyntaxHighlighting = True Then
                If Mid$(Lines(Row + RowOffset), Column + ColumnOffset, 1) <> "" Then
                    Color SyntaxTextColor(Mid$(Lines(Row + RowOffset), Column + ColumnOffset, 1)), BackgroundColor
                    Locate Row, Column: Print Mid$(Lines(Row + RowOffset), Column + ColumnOffset, 1);
                End If
            End If
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

    Clipboard = _Clipboard$
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
        If (KeyAscii = 18176) Then: CurrentColumn = 1 'Home
        If (KeyAscii = 20224) Then: CurrentColumn = (Len(Lines(CurrentRow)) + 1) 'End
        If (KeyAscii = 21248) Then 'Delete
            If (CurrentColumn = (Len(Lines(CurrentRow)) + 1)) Then
                Lines(CurrentRow) = Lines(CurrentRow) + Lines(CurrentRow + 1)
                For A = CurrentRow + 1 To UsedLines
                    Lines(A) = Lines(A + 1)
                    Lines(A + 1) = ""
                Next A
            End If
            Lines(CurrentRow) = Left$(Lines(CurrentRow), CurrentColumn - 1) + Right$(Lines(CurrentRow), Len(Lines(CurrentRow)) - CurrentColumn)
        End If
        If (KeyAscii = 18688) Then 'Page Up
            CurrentRow = CurrentRow - 25
            If CurrentRow < 1 Then: CurrentRow = 1
        End If
        If (KeyAscii = 20736) Then 'Page Down
            CurrentRow = CurrentRow + 25
            If CurrentRow > UsedLines Then: UsedLines = CurrentRow
        End If
        If (KeyAscii = 3) Then: _Clipboard$ = SelectedText 'CTRL+V
        If (KeyAscii = 22) Then: Lines(CurrentRow) = AddCharacter$(Clipboard): CurrentColumn = (Len(Lines(CurrentRow)) + 1) 'CTRL+C
        If ((KeyAscii = 18432) And (CurrentRow > 1)) Then 'Up
            CurrentRow = CurrentRow - 1
            If ((CurrentColumn + 1) > Len(Lines(CurrentRow + RowOffset))) Then
                CurrentColumn = Len(Lines(CurrentRow + RowOffset)) + 1
            End If
        End If
        If ((KeyAscii = 20480) And ((CurrentRow + 1) <= UsedLines)) Then 'Down
            CurrentRow = CurrentRow + 1
            If ((CurrentColumn + 1) > Len(Lines(CurrentRow + RowOffset))) Then
                CurrentColumn = Len(Lines(CurrentRow + RowOffset)) + 1
            End If
        End If
        If (KeyAscii = 19200) Then 'Left
            If (CurrentColumn > 1) Then
                CurrentColumn = CurrentColumn - 1
            Else
                If (CurrentRow > 1) Then
                    CurrentRow = CurrentRow - 1
                    CurrentColumn = Len(Lines(CurrentRow)) + 1
                End If
            End If
        End If
        If (KeyAscii = 19712) Then 'Right
            If Mid$(Lines(CurrentRow), (CurrentColumn), 1) <> "" Then
                CurrentColumn = CurrentColumn + 1
            Else
                If ((CurrentRow + 1) <= UsedLines) Then
                    CurrentRow = CurrentRow + 1
                    CurrentColumn = 1
                End If
            End If
        End If
        If ((KeyAscii) >= 97 And (KeyAscii <= 126)) Then: Lines(CurrentRow) = AddCharacter$(KeyPress) 'a-z { | } ~
        If ((KeyAscii) >= 65 And (KeyAscii <= 96)) Then: Lines(CurrentRow) = AddCharacter$(KeyPress) 'A-Z [ \ ] `
        If ((KeyAscii >= 33) And (KeyAscii <= 64)) Then: Lines(CurrentRow) = AddCharacter$(KeyPress) ' ': ; < = > ? @ 0 - 9 ! " # $ % & ' ( ) * + , - . /
        If (KeyAscii = 32) Then: Lines(CurrentRow) = AddCharacter$(KeyPress) 'Space
        If (KeyAscii = 13) Then 'Enter
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
        If (KeyAscii = 9) Then: Lines(CurrentRow) = AddCharacter$("    "): CurrentColumn = CurrentColumn + 3 'Tab
        If (KeyAscii = 8) Then 'BackSpace
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
                End If
            End If
        End If

        SelectionKeyCombinations
        KeyCombinations

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

Sub KeyCombinations
    If (KeyAscii = 8960) Then 'ALT + H
        If SyntaxHighlighting = True Then
            SyntaxHighlighting = False
        Else
            SyntaxHighlighting = True
        End If
    End If
End Sub

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
Function SyntaxTextColor (Text As String)
    FuncReturn = 15
    If ((Asc(Text) >= 48) And (Asc(Text) <= 57)) Then
        FuncReturn = 40
    End If
    SyntaxTextColor = FuncReturn
End Function
