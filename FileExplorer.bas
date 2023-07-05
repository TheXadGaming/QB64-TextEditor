Rem $Dynamic

Type File
    FileName As String
    FileExtension As String
    FileType As String
End Type

Const MaxFileAmount = 500
Const True = -1
Const False = 0

Sub FileExplorer (ReturnExtenstion As String, ReturnDirectory As String, ReturnFile As String, StartDirectory As String, PWidth As Integer, PHeight As Integer)
    If Right$(StartDirectory, 1) <> "\" Then: StartDirectory = StartDirectory + "\"
    If StartDirectory = "" Then: StartDirectory = "C:\"
    If ReturnExtenstion = "" Then: ReturnExtenstion = ".*"
    If PWidth = 0 Then: PWidth = 40
    If PHeight = 0 Then: PHeight = 25
    ReturnExtenstion = UCase$(ReturnExtenstion)
    PFileOffset = FileOffset
    FileOffset = SelectedFile - 21
    If FileOffset < 0 Then: FileOffset = 0
    If PFileOffset <> FileOffset Then: Cls
    Screen 13
    Color 2, 0
    Width 80, 25
    Dim CurrentDirectoryFiles(MaxFileAmount) As File
    Dim CurrentDirectory As String: CurrentDirectory = StartDirectory
    Dim CurrentDrive As String: CurrentDrive = Left$(CurrentDirectory, 3)
    Dim FilesList(MaxFileAmount) As String
    Dim FilesListLength As Integer
    Dim KeyAscii As Integer
    Dim KeyPress As String
    Dim SelectedFile As Integer: SelectedFile = 1
    Dim FileOffset As Integer: FileOffset = 0
    Dim DrawScreen As Integer: DrawScreen = True
    Dim ShouldGetFiles As Integer: ShouldGetFiles = True
    Dim ExError As Integer: ExError = False
    Do
        KeyPress = InKey$
        If Len(KeyPress) > 0 Then
            KeyAscii = ASCII_16(KeyPress)
        Else
            KeyAscii = 0
        End If
        If (KeyAscii <> 0) Then
            If (KeyAscii = 13) Then
                If (CurrentDirectoryFiles(SelectedFile).FileType = "Folder") Then
                    CurrentDirectory = CurrentDirectory + CurrentDirectoryFiles(SelectedFile).FileName + CurrentDirectoryFiles(SelectedFile).FileExtension + "\"
                    SelectedFile = 1
                    ShouldGetFiles = True
                    Cls
                ElseIf (CurrentDirectoryFiles(SelectedFile).FileType = "Back") And Not (CurrentDirectory = CurrentDrive) Then
                    CurrentDirectory = PDirectory$(CurrentDirectory)
                    SelectedFile = 1
                    ShouldGetFiles = True
                    Cls
                End If
            End If
            If (KeyAscii = 8) Then
                If Not (CurrentDirectory = CurrentDrive) Then
                    CurrentDirectory = PDirectory$(CurrentDirectory)
                    SelectedFile = 1
                    ShouldGetFiles = True
                    Cls
                End If
            End If
            ExError = False
            If (KeyAscii = 32) Then
                If ReturnExtenstion = "DIR" Then
                    If CurrentDirectoryFiles(SelectedFile).FileType = "Folder" Then
                        ReturnDirectory = CurrentDirectory
                        ReturnFile = CurrentDirectoryFiles(SelectedFile).FileName + CurrentDirectoryFiles(SelectedFile).FileExtension + "\"
                        ShouldExit = True
                    Else
                        ExError = True
                    End If
                Else
                    If CurrentDirectoryFiles(SelectedFile).FileType = "File" Then
                        If UCase$(CurrentDirectoryFiles(SelectedFile).FileExtension) = ReturnExtenstion Or ReturnExtenstion = ".*" Then
                            ReturnDirectory = CurrentDirectory
                            ReturnFile = CurrentDirectoryFiles(SelectedFile).FileName + CurrentDirectoryFiles(SelectedFile).FileExtension
                            ShouldExit = True
                        Else
                            ExError = True
                        End If
                    Else
                        ExError = True
                    End If
                End If
            End If
            If (KeyAscii = 18176) Then
                CurrentDirectory = CurrentDrive
                SelectedFile = 1
                ShouldGetFiles = True
                DrawScreen = True
            End If
            If (KeyAscii = 18688) Then
                SelectedFile = SelectedFile - 22
                If SelectedFile < 1 Then: SelectedFile = 1
            End If
            If (KeyAscii = 20736) Then
                SelectedFile = SelectedFile + 22
                If SelectedFile > FilesListLength Then: SelectedFile = FilesListLength
            End If
            If (KeyAscii = 18432) Then: SelectedFile = SelectedFile - 1
            If (KeyAscii = 20480) Then: SelectedFile = SelectedFile + 1
            If SelectedFile < 1 Then: SelectedFile = 1
            If SelectedFile > FilesListLength Then: SelectedFile = FilesListLength
            DrawScreen = True
        End If
        PFileOffset = FileOffset
        FileOffset = SelectedFile - 21
        If FileOffset < 0 Then: FileOffset = 0
        If PFileOffset <> FileOffset Then: Cls
        If ShouldGetFiles = True Then
            ReDim CurrentDirectoryFiles(MaxFileAmount) As File
            ReDim FilesList(MaxFileAmount) As String
            GetFiles CurrentDirectory, FilesList(), FilesListLength, CurrentDirectoryFiles(), CurrentDrive
        End If
        If DrawScreen = True Then: DisplayExplorer FileOffset, SelectedFile, CurrentDirectoryFiles(), CurrentDirectory, CurrentDrive, FilesListLength, ExError, ReturnExtenstion
        ShouldGetFiles = False
        DrawScreen = False
        _Display
    Loop Until ShouldExit = True
    Screen 13
    Width PWidth, PHeight
End Sub

Function ASCII_16 (K As String)
    Dim A As Long
    A = Asc(Mid$(K, 1, 1))
    If Len(K) = 2 Then
        A = A + (Asc(Mid$(K, 2, 1)) * 256)
    End If
    ASCII_16 = A
End Function

Sub DisplayExplorer (FileOffset As Integer, SelectedFile As Integer, CurrentDirectoryFiles() As File, CurrentDirectory As String, CurrentDrive As String, FilesListLength As Integer, ExError As Integer, ReturnEx As String)
    Cls
    Color 2, 0
    Locate 1, 1: Print "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿";
    For A = 1 To 22
        Color 2, 0
        Locate A + 1, 1: Print "³";
        If (A + FileOffset) <= FilesListLength Then
            If (A + FileOffset) = SelectedFile Then: Color 2, 15
            Locate A + 1, 2: Print Left$(CurrentDirectoryFiles(A + FileOffset).FileName + CurrentDirectoryFiles(A + FileOffset).FileExtension, 78);
            If CurrentDirectoryFiles(A + FileOffset).FileType = "Folder" Then
                Locate A + 1, 74: Print "<DIR>";
            End If
            Color 2, 0
        Else
            Locate A + 1, 2: Print "                                      ";
        End If
        Color 2, 0
        Locate A + 1, 80: Print "³";
    Next A
    Locate 23, 1: Print "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´";

    If ExError = False Then
        If Len(CurrentDirectory) < 78 Then
            Locate 24, 1: Print "³";: Locate 24, 2: Print Right$(CurrentDirectory, 78);: Locate 24, 80: Print "³";
        ElseIf Len(CurrentDirectory) > 78 Then
            Locate 24, 1: Print "³";: Locate 24, 2: Print CurrentDrive; "...\"; Right$(CurrentDirectory, 71);: Locate 24, 80: Print "³";
        End If
        Locate 25, 1: Print "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ";
    Else
        If ReturnEx = "DIR" Then
            TmpVar$ = "Folder"
        ElseIf ReturnEx = ".*" Then
            TmpVar$ = "File"
        Else
            TmpVar$ = ReturnEx
        End If
        Locate 24, 1: Print "³";
        Color 15, 4
        Locate 24, 2: Print "          s                                                                    ";
        Locate 24, 2: Print "Expected: "; TmpVar$;
        Locate 24, 35: Print "WRONG TYPE";
        Color 2, 0
        Locate 24, 80: Print "³";
        Locate 25, 1: Print "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ";
    End If
End Sub

Sub GetFiles (CurrentDirectory As String, FilesList() As String, FilesListLength As Integer, CurrentDirectoryFiles() As File, CurrentDrive As String)
    ListFile$ = (_StartDir$ + "\FilesList.list")
    File = FreeFile
    Index = 1
    FilesAmount = 0
    FoldersAmount = 0
    If CurrentDirectory <> "" Then
        For A = 1 To 2
            If A = 1 Then
                Shell _Hide "DIR " + Chr$(34) + CurrentDirectory + Chr$(34) + " /A:D /B > " + Chr$(34) + ListFile$ + Chr$(34)
            ElseIf A = 2 Then
                Shell _Hide "DIR " + Chr$(34) + CurrentDirectory + Chr$(34) + " /A:-D /B > " + Chr$(34) + ListFile$ + Chr$(34)
            End If
            Open ListFile$ For Input As #File
            Do While Not EOF(File) And Index < MaxFileAmount
                Line Input #File, FilesList(Index)
                Index = Index + 1
            Loop
            Close #File
            Kill ListFile$
            If A = 1 Then
                FoldersAmount = Index - 1
            ElseIf A = 2 Then
                FilesAmount = Index - FoldersAmount
            End If
        Next A
    End If
    FilesListLength = FoldersAmount + FilesAmount - 1
    For B = 1 To FilesListLength
        If B <= FoldersAmount Then
            TmpVar$ = "Folder"
        Else
            TmpVar$ = "File"
        End If
        CurrentDirectoryFiles(B).FileType = TmpVar$
        If CurrentDirectoryFiles(B).FileType = "Folder" Then
            CurrentDirectoryFiles(B).FileName = FilesList(B)
        ElseIf CurrentDirectoryFiles(B).FileType = "File" Then
            CurrentDirectoryFiles(B).FileExtension = GetFileExtension$(FilesList(B))
            CurrentDirectoryFiles(B).FileName = Left$(FilesList(B), Len(FilesList(B)) - Len(CurrentDirectoryFiles(B).FileExtension))
        End If
    Next B
    If CurrentDirectory <> CurrentDrive Then
        FilesListLength = FilesListLength + 1
        CurrentDirectoryFiles(FilesListLength).FileName = ".."
        CurrentDirectoryFiles(FilesListLength).FileType = "Back"
    End If
End Sub

Function GetFileExtension$ (FileName$)
    FuncResult$ = ""
    A = 0
    Do
        If Mid$(FileName$, Len(FileName$) - A, 1) = "." Then: Exit Do
        If A = Len(FileName$) Then: Exit Function
        A = A + 1
    Loop
    FuncResult$ = Right$(FileName$, A + 1)
    If Left$(FuncResult$, 1) <> "." Then: FuncResult$ = ("." + FuncResult$)
    GetFileExtension$ = FuncResult$
End Function

Function PDirectory$ (Directory$)
    FuncReturn$ = ""
    A = 1
    Do
        If Mid$(Directory$, Len(Directory$) - A, 1) = "\" Or A = Len(Directory$) Then: Exit Do
        A = A + 1
    Loop
    If A = Len(Directory) Then: PDirectory$ = Directory$
    FuncReturn$ = Left$(Directory$, Len(Directory$) - A)

    PDirectory$ = FuncReturn$
End Function
