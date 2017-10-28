Imports NATUPNPLib
Public Class frmMain
    Dim tacoz As New Button
    Dim mytaunt As String
    Dim SQL1 As New MySQLClient.MySQLControl()
    Dim SQL2 As New MySQLClient.MySQLControl()
    Dim matchmaker As New XbandMatchMakingClient.XbandMatchClient
    Dim overmind As New XbandMatchTracker.MatchTracker
    Dim chatcolor As New Color
    Dim chatconnected As Integer
    Dim Users(3) As PictureBox
    Dim maildat As New TextBox
    Dim maildata(2) As Label
    Dim chatcontrol As New ChatControl.ChatControl
    Dim mailinput(2) As TextBox
    Dim Title(3) As Label
    Dim dg As DataGridView
    Dim CurrentSelected As Integer
    Dim CurrentState As Integer
    Dim Flashing As Integer
    Dim YesNo As Integer
    Dim lblYes As Label
    Dim wrongattempts As Integer
    Dim lblNo As Label
    Dim backbutton As PictureBox
    Dim UserID As String
    Dim UzerName As TextBox
    Dim storeusername As String
    Dim Browzer As New WebBrowser
    Dim storepassword As String
    Dim storeicon As String
    Dim TheBox As Panel
    Dim TheBox2 As Panel
    Dim info1 As Label
    Dim info2 As Label
    Dim UserIcon As PictureBox
    Dim txtPassword As TextBox
    Dim CurrentICN As Integer
    Dim choosecolor As New ColorDialog
    Dim gameId As Integer = -1
    Dim opponent As String
    Dim gameName As String
    Dim formCommands As New Stack(Of Integer)
    Dim tmr As New Timer()

    'Dim tc As TCMPortMapper.PortMapper


    Private Sub TheBox_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs)
        Controls.Remove(sender)
        'Controls.Remove(TheBox2)
        tmrFlash.Enabled = False
        Me.Select()
    End Sub



    Private Sub TheBox2_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs)
        'Controls.Remove(sender)
        Select Case e.KeyData
            Case Keys.Right
                If YesNo = 1 Then
                    Flashing = 1
                    lblYes.ForeColor = Color.Black
                    YesNo = 0
                    Exit Sub
                End If
            Case Keys.Left
                If YesNo = 0 Then
                    Flashing = 0
                    lblNo.ForeColor = Color.Black
                    YesNo = 1
                    Exit Sub
                End If
            Case Keys.Enter
                If YesNo = 0 Then
                    tmrFlash.Enabled = False
                    Controls.Remove(sender)
                    Exit Sub
                End If
                If YesNo = 1 Then
                    Controls.Remove(sender)
                    Paint_New_User()
                    Exit Sub

                End If
        End Select
        Me.Select()
    End Sub
    Private Sub frmMain_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles Me.KeyDown
        Select Case e.KeyData
            Case Keys.Right
                Menu_Move(2)


            Case Keys.Left
                Menu_Move(4)

            Case Keys.Up
                Menu_Move(1)

            Case Keys.Down
                Menu_Move(3)

            Case Keys.Enter

                Menu_Move(5)
            Case Keys.F5
                If CurrentState = 0 Then
                    Dim blahdeblah As String
                    blahdeblah = InputBox("Enter the XBAND 2 username you wish to recover", "XBAND Username Recovery")
                    blahdeblah = blahdeblah.Replace("'", "`")

                    If blahdeblah = "" Then
                        Exit Sub
                    End If
                    SQL1.ExecuteQuery("SELECT * FROM player WHERE Username = '" & blahdeblah & "';")
                    SQL1.Next()
                    If SQL1.FieldData(2).data = Nothing Then
                        XbMsgbox("Invalid username! Please check your spelling and try again, if that isnt working you might be banned.")
                        Exit Sub
                    End If
                    storeicon = SQL1.FieldData(5).data.ToString
                    Dim fileContents() As String
                    fileContents = New String() {}
                    Array.Resize(fileContents, 8)
                    fileContents = System.IO.File.ReadAllLines(Application.StartupPath + "//settings.ini")
                    fileContents(CurrentSelected) = storeicon
                    fileContents(CurrentSelected + 4) = SQL1.FieldData(1).data.ToString
                    System.IO.File.WriteAllLines(Application.StartupPath + "//settings.ini", fileContents)
                    Unload_User_Menu()
                    Draw_Main_Menu()
                End If
            Case Keys.Delete
                If CurrentState = 0 Then
                    If MsgBox("Are you sure you want to delete this user?", MsgBoxStyle.YesNo, "Really Delete This User?") = MsgBoxResult.Yes Then
                        Dim fileContents() As String
                        fileContents = New String() {}
                        Array.Resize(fileContents, 8)
                        fileContents = System.IO.File.ReadAllLines(Application.StartupPath + "//settings.ini")
                        fileContents(CurrentSelected) = "0"
                        fileContents(CurrentSelected + 4) = "New User"
                        System.IO.File.WriteAllLines(Application.StartupPath + "//settings.ini", fileContents)
                        Unload_User_Menu()
                        Draw_Main_Menu()
                    End If
                End If
        End Select
    End Sub

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Me.Location = New Point(0, 0)
        Dim muzic As Integer
        chatcolor = Color.Yellow
        chatconnected = 0
        Randomize()
        SQL1.Connect("127.0.0.1", "xband", "root", "")
        SQL2.Connect("127.0.0.1", "xband", "root", "")
        muzic = Int(Rnd() * 7)
        PlayLoopingBackgroundSoundFile()
        'wmp.URL = Application.StartupPath & "\mp3\main-menu.mp3"
        Dim fileContents() As String ' = System.IO.File.ReadAllLines(Application.StartupPath + "//settings.ini")

        If System.IO.File.Exists(Application.StartupPath + "//settings.ini") Then
            fileContents = System.IO.File.ReadAllLines(Application.StartupPath + "//settings.ini")
        Else
            System.IO.File.WriteAllText(Application.StartupPath + "//settings.ini", "")
            fileContents = New String() {}
            Array.Resize(fileContents, 8)
            For i = 0 To 3
                fileContents(i) = "0"
            Next
            For i = 4 To 7
                fileContents(i) = "New Player"
            Next
        End If

        System.IO.File.WriteAllLines(Application.StartupPath + "//settings.ini", fileContents)
        SQL1.ExecuteQuery("SELECT * FROM CONFIG")
        SQL1.Next()
        If SQL1.FieldData(1).data = "1" Then
            XbMsgbox("It's not time yet! Please be patient until we open the gates.")
            Exit Sub
        End If
        If SQL1.FieldData(1).data = "2" Then
            XbMsgbox("Thank you for participating in the XBAND 2 Weekend Warrior program. We hope you pwned some newbs!")
            Exit Sub
        End If
        Draw_Main_Menu()

        AddHandler tmr.Tick, AddressOf TimerTickSandwich
        tmr.Interval = 200
        tmr.Enabled = True

        'Dim u As New NATUPNPLib.UPnPNATClass()
        'Dim t As NATUPNPLib.IStaticPortMappingCollection = u.StaticPortMappingCollection
        't.Add(7845, "TCP", 7845, "192.168.2.5", True, "NETPLAY")
        'AddPort(t, 7845, "192.168.2.5", "NETPLAY")
        'Dim h As System.Net.IPHostEntry = System.Net.Dns.GetHostEntry(System.Net.Dns.GetHostName)
        'Console.WriteLine(h.AddressList(0).ToString())
        't.Remove(7845, "TCP")

    End Sub
    Sub PlayLoopingBackgroundSoundFile()
        My.Computer.Audio.Play(Application.StartupPath & "\mp3\main-menu.wav",
        AudioPlayMode.BackgroundLoop)
    End Sub
    Public Sub PlayRandomTrack()

        Dim trackNum As Integer = CInt(Rnd() * 3 + 0.5)

        Select Case trackNum

            Case 1
                My.Computer.Audio.Play(Application.StartupPath & "\mp3\waiting-music1.wav", AudioPlayMode.Background)

            Case 2
                My.Computer.Audio.Play(Application.StartupPath & "\mp3\waiting-music2.wav", AudioPlayMode.Background)

            Case Else
                My.Computer.Audio.Play(Application.StartupPath & "\mp3\waiting-music3.wav", AudioPlayMode.Background)

        End Select

    End Sub

    Private Sub TimerTickSandwich(ByVal sender As Object, ByVal e As System.EventArgs)

        If formCommands.Count > 0 Then
            Dim c As Integer = formCommands.Pop()
            Select Case c
                Case 0
                    Me.Hide()
                Case 1
                    Me.Show()
                Case 20
                    XbMsgbox("You are Victorious!")
                Case 21
                    XbMsgbox("Better luck next time :(")
                Case 22
                    XbMsgbox("It was a tie!")
                Case 25
                    TauntScreenStart()
                Case 50
                    Me.Focus()
                    Me.BringToFront()
                    unload_taunt()
                    DRAW_NAV_MENU()
                Case 99

                Case 100


            End Select
        End If

    End Sub

    Private Sub XbMsgbox(ByVal Info As String)
        Dim TheInfo As Label
        TheBox2 = New Panel
        TheBox2.Size = New Point(500, 200)
        TheBox2.Location = New Point(50, 50)
        AddHandler TheBox2.KeyDown, AddressOf TheBox_KeyDown
        Controls.Add(TheBox2)
        TheInfo = New Label
        TheInfo.Text = Info
        TheInfo.Size = New Point(465, 100)
        TheInfo.TextAlign = ContentAlignment.TopLeft
        TheInfo.BackColor = Color.Transparent
        TheInfo.Font = New Font("Terminal", 14, FontStyle.Bold)
        'TheBox.BorderStyle = BorderStyle.Fixed3D
        TheBox2.Controls.Add(TheInfo)
        TheInfo.Location = New Point(30, 20)
        TheBox2.BringToFront()
        TheBox2.BackgroundImage = Image.FromFile(Application.StartupPath & "\img\xbmsgbox.png")
        TheBox2.BackgroundImageLayout = ImageLayout.Stretch
        TheBox2.TabIndex = 0
        lblNo = New Label
        lblNo.Text = "OK"
        lblNo.BackColor = Color.Transparent
        lblNo.ForeColor = Color.Black
        lblNo.Font = New Font(TheInfo.Font.FontFamily, 14, FontStyle.Bold)
        lblNo.AutoSize = True
        lblNo.Location = New Point(225, 150)
        TheBox2.Controls.Add(lblNo)
        Flashing = 1
        tmrFlash.Enabled = True
        TheBox2.Select()
        TheBox2.Focus()
    End Sub
    Private Sub Draw_Main_Menu()
        CurrentSelected = 0
        CurrentState = 0
        Dim filecontents() As String
        filecontents = System.IO.File.ReadAllLines(Application.StartupPath + "//settings.ini")
        Me.SuspendLayout()
        Dim b As Integer
        Dim c As Integer
        b = 120
        c = 120
        For i = 0 To 3
            Users(i) = New PictureBox()
            Users(i).Size = New Point(70, 100)
            Dim x As Integer = (i Mod 2) * 325 + 90
            Dim y As Integer = 25 + (Math.Floor(i / 2) * 250)
            Users(i).Location = New Point(x, y)
            Users(i).SizeMode = PictureBoxSizeMode.StretchImage
            Users(i).Tag = filecontents(i)
            Users(i).Image = Image.FromFile(Application.StartupPath & "\img\" & filecontents(i) & "w.gif")
            'Users(i).Image.PixelFormat(System.Drawing.ImagiFormat16bppGrayScale)
            Controls.Add(Users(i))
        Next
        b = 120
        c = 120
        For i = 0 To 3
            Title(i) = New Label()
            Dim x As Integer = (i Mod 2) * 325 + 55
            Dim y As Integer = 122 + (Math.Floor(i / 2) * 250)
            Title(i).Width = 150
            Title(i).Height = 40
            Title(i).TextAlign = ContentAlignment.TopCenter
            Title(i).Location = New Point(x, y)
            Title(i).BackColor = Color.Transparent
            Title(i).ForeColor = Color.Gray
            Title(i).Font = New Font("Terminal", 18, FontStyle.Bold)
            Title(i).BorderStyle = BorderStyle.None
            Title(i).Text = filecontents(i + 4)
            Controls.Add(Title(i))
        Next
        info1 = New Label
        info2 = New Label
        info1.Text = "Press F5 over a userbox if you already have an account!"
        info2.Text = "Press DEL over a userbox if you wish to delete that user"
        info1.BackColor = Color.Transparent
        info2.BackColor = Color.Transparent
        info1.ForeColor = Color.Yellow
        info2.ForeColor = Color.Yellow
        info1.Font = New Font("System", 14, FontStyle.Bold, GraphicsUnit.Pixel)
        info2.Font = New Font("System", 14, FontStyle.Bold, GraphicsUnit.Pixel)
        info1.AutoSize = True
        info2.AutoSize = True
        info1.Location = New Point(90, 450)
        info2.Location = New Point(90, 470)
        Controls.Add(info1)
        Controls.Add(info2)
        Me.ResumeLayout()
        Load_User(0)
        Me.Select()
    End Sub
    Private Sub XbMsgboxYesNo(ByVal info As String)
        Dim TheInfo As Label
        TheBox2 = New Panel
        TheBox2.Size = New Point(500, 200)
        TheBox2.Location = New Point(50, 50)
        AddHandler TheBox2.KeyDown, AddressOf TheBox2_KeyDown
        Controls.Add(TheBox2)
        TheInfo = New Label
        TheInfo.Text = info
        TheInfo.Size = New Point(465, 90)
        TheInfo.TextAlign = ContentAlignment.TopLeft
        TheInfo.BackColor = Color.Transparent
        TheInfo.Font = New Font(TheInfo.Font.FontFamily, 14, FontStyle.Bold)
        'TheBox.BorderStyle = BorderStyle.Fixed3D
        TheBox2.Controls.Add(TheInfo)
        TheInfo.Location = New Point(30, 20)
        TheBox2.BringToFront()
        TheBox2.BackgroundImage = Image.FromFile(Application.StartupPath & "\img\xbmsgbox.png")
        TheBox2.BackgroundImageLayout = ImageLayout.Stretch
        TheBox2.TabIndex = 0
        lblYes = New Label
        lblNo = New Label
        lblNo.Text = "No"
        lblYes.Text = "Yes"
        lblNo.BackColor = Color.Transparent
        lblYes.BackColor = Color.Transparent
        lblNo.ForeColor = Color.Black
        lblYes.ForeColor = Color.Black
        lblYes.Font = New Font(TheInfo.Font.FontFamily, 14, FontStyle.Bold)
        lblNo.Font = New Font(TheInfo.Font.FontFamily, 14, FontStyle.Bold)
        lblNo.AutoSize = True
        lblNo.Location = New Point(300, 150)
        lblYes.Location = New Point(150, 150)
        lblYes.AutoSize = True
        YesNo = 0
        Flashing = 1
        tmrFlash.Enabled = True
        TheBox2.Controls.Add(lblYes)
        TheBox2.Controls.Add(lblNo)
        TheBox2.Select()
        TheBox2.Focus()

    End Sub
    Private Sub Unload_User(ByVal userid As Integer)
        Users(userid).Image = Image.FromFile(Application.StartupPath & "\img\" & Users(userid).Tag & "w.gif")
        Title(userid).ForeColor = Color.Gray
        Me.Select()
    End Sub
    Private Sub Load_User(ByVal userid As Integer)
        Users(userid).Image = Image.FromFile(Application.StartupPath & "\img\" & Users(userid).Tag & ".gif")
        Title(userid).ForeColor = Color.Yellow
        Me.Select()
    End Sub

    Private Sub Menu_Move(ByVal direction As Integer)
        '1 up - 2 right - 3 down - 4 left - 5 enter
        Select Case CurrentState
            '0 = Main Menu
            Case 0
                Select Case direction
                    Case 1
                        If CurrentSelected = 2 Then
                            Unload_User(CurrentSelected)
                            Load_User(0)
                            CurrentSelected = 0
                            Exit Sub
                        End If
                        If CurrentSelected = 3 Then
                            Unload_User(CurrentSelected)
                            Load_User(1)
                            CurrentSelected = 1
                            Exit Sub
                        End If
                    Case 2
                        If CurrentSelected = 0 Then
                            Unload_User(CurrentSelected)
                            Load_User(1)
                            CurrentSelected = 1
                            Exit Sub
                        End If
                        If CurrentSelected = 2 Then
                            Unload_User(CurrentSelected)
                            Load_User(3)
                            CurrentSelected = 3
                            Exit Sub
                        End If
                    Case 3
                        If CurrentSelected = 0 Then
                            Unload_User(CurrentSelected)
                            Load_User(2)
                            CurrentSelected = 2
                            Exit Sub
                        End If
                        If CurrentSelected = 1 Then
                            Unload_User(CurrentSelected)
                            Load_User(3)
                            CurrentSelected = 3
                            Exit Sub
                        End If
                    Case 4
                        If CurrentSelected = 1 Then
                            Unload_User(CurrentSelected)
                            Load_User(0)
                            CurrentSelected = 0
                            Exit Sub
                        End If
                        If CurrentSelected = 3 Then
                            Unload_User(CurrentSelected)
                            Load_User(2)
                            CurrentSelected = 2
                            Exit Sub
                        End If
                    Case 5
                        If Users(CurrentSelected).Tag = 0 Then
                            XbMsgboxYesNo("Are you sure you want to enter a new player ID? You'll create a Code Name and choose a Character.")
                        Else
                            BringPasswordUp()
                        End If
                End Select
            Case 2
                Select Case direction
                    Case 1
                        If CurrentSelected = 4 Then
                            CurrentSelected = 1
                            Users(3).Image = Image.FromFile(Application.StartupPath & "\img\01\3.gif")
                            Users(0).Image = Image.FromFile(Application.StartupPath & "\img\01\6.gif")
                            Exit Sub
                        End If
                        If CurrentSelected = 5 Then
                            CurrentSelected = 2
                            Users(4).Image = Image.FromFile(Application.StartupPath & "\img\01\4.gif")
                            Users(1).Image = Image.FromFile(Application.StartupPath & "\img\01\7.gif")
                            Exit Sub
                        End If
                        If CurrentSelected = 6 Then
                            CurrentSelected = 3
                            Users(5).Image = Image.FromFile(Application.StartupPath & "\img\01\5.gif")
                            Users(2).Image = Image.FromFile(Application.StartupPath & "\img\01\8.gif")
                            Exit Sub
                        End If
                    Case 2
                        If CurrentSelected = 1 Then
                            CurrentSelected = 2
                            Users(0).Image = Image.FromFile(Application.StartupPath & "\img\01\0.gif")
                            Users(1).Image = Image.FromFile(Application.StartupPath & "\img\01\7.gif")
                            Exit Sub
                        End If
                        If CurrentSelected = 2 Then
                            CurrentSelected = 3
                            Users(1).Image = Image.FromFile(Application.StartupPath & "\img\01\1.gif")
                            Users(2).Image = Image.FromFile(Application.StartupPath & "\img\01\8.gif")
                            Exit Sub
                        End If
                        If CurrentSelected = 4 Then
                            CurrentSelected = 5
                            Users(3).Image = Image.FromFile(Application.StartupPath & "\img\01\3.gif")
                            Users(4).Image = Image.FromFile(Application.StartupPath & "\img\01\10.gif")
                            Exit Sub
                        End If
                        If CurrentSelected = 5 Then
                            CurrentSelected = 6
                            Users(4).Image = Image.FromFile(Application.StartupPath & "\img\01\4.gif")
                            Users(5).Image = Image.FromFile(Application.StartupPath & "\img\01\11.gif")
                            Exit Sub
                        End If
                    Case 3
                        If CurrentSelected = 1 Then
                            CurrentSelected = 4
                            Users(0).Image = Image.FromFile(Application.StartupPath & "\img\01\0.gif")
                            Users(3).Image = Image.FromFile(Application.StartupPath & "\img\01\9.gif")
                            Exit Sub
                        End If
                        If CurrentSelected = 2 Then
                            CurrentSelected = 5
                            Users(1).Image = Image.FromFile(Application.StartupPath & "\img\01\1.gif")
                            Users(4).Image = Image.FromFile(Application.StartupPath & "\img\01\10.gif")
                            Exit Sub
                        End If
                        If CurrentSelected = 3 Then
                            CurrentSelected = 6
                            Users(2).Image = Image.FromFile(Application.StartupPath & "\img\01\2.gif")
                            Users(5).Image = Image.FromFile(Application.StartupPath & "\img\01\11.gif")
                            Exit Sub
                        End If
                    Case 4
                        If CurrentSelected = 2 Then
                            CurrentSelected = 1
                            Users(1).Image = Image.FromFile(Application.StartupPath & "\img\01\1.gif")
                            Users(0).Image = Image.FromFile(Application.StartupPath & "\img\01\6.gif")
                            Exit Sub
                        End If
                        If CurrentSelected = 3 Then
                            CurrentSelected = 2
                            Users(2).Image = Image.FromFile(Application.StartupPath & "\img\01\2.gif")
                            Users(1).Image = Image.FromFile(Application.StartupPath & "\img\01\7.gif")
                            Exit Sub
                        End If
                        If CurrentSelected = 5 Then
                            CurrentSelected = 4
                            Users(4).Image = Image.FromFile(Application.StartupPath & "\img\01\4.gif")
                            Users(3).Image = Image.FromFile(Application.StartupPath & "\img\01\9.gif")
                            Exit Sub
                        End If
                        If CurrentSelected = 6 Then
                            CurrentSelected = 5
                            Users(5).Image = Image.FromFile(Application.StartupPath & "\img\01\5.gif")
                            Users(4).Image = Image.FromFile(Application.StartupPath & "\img\01\10.gif")
                            Exit Sub
                        End If
                    Case 5
                        If CurrentSelected = 1 Then
                            Unload_Main_Menu()
                            Paint_Challenge_Menu()
                        End If
                        If CurrentSelected = 2 Then
                            Unload_Main_Menu()
                            Paint_Buddy_List()
                        End If
                        If CurrentSelected = 3 Then
                            Unload_Main_Menu()
                            Paint_Mail_Menu()
                        End If
                        If CurrentSelected = 4 Then
                            Unload_Main_Menu()
                            Paint_Statistics()
                        End If

                        If CurrentSelected = 5 Then
                            Unload_Main_Menu()
                            Paint_Options()
                        End If

                        If CurrentSelected = 6 Then
                            Unload_Main_Menu()
                            Draw_Main_Menu()
                        End If
                End Select
            Case 3
                Select Case direction
                    Case 1
                        If CurrentSelected = 3 Then
                            Users(2).Image = Image.FromFile(Application.StartupPath & "\img\02\3.gif")
                            Users(0).Image = Image.FromFile(Application.StartupPath & "\img\02\5.gif")
                            CurrentSelected = 1
                            Exit Sub
                        End If
                        If CurrentSelected = 4 Then
                            Users(3).Image = Image.FromFile(Application.StartupPath & "\img\02\4.gif")
                            Users(1).Image = Image.FromFile(Application.StartupPath & "\img\02\6.gif")
                            CurrentSelected = 2
                            Exit Sub
                        End If
                    Case 2
                        If CurrentSelected = 1 Then
                            Users(0).Image = Image.FromFile(Application.StartupPath & "\img\02\1.gif")
                            Users(1).Image = Image.FromFile(Application.StartupPath & "\img\02\6.gif")
                            CurrentSelected = 2
                            Exit Sub
                        End If
                        If CurrentSelected = 3 Then
                            Users(2).Image = Image.FromFile(Application.StartupPath & "\img\02\3.gif")
                            Users(3).Image = Image.FromFile(Application.StartupPath & "\img\02\8.gif")
                            CurrentSelected = 4
                            Exit Sub
                        End If
                    Case 3
                        If CurrentSelected = 1 Then
                            Users(0).Image = Image.FromFile(Application.StartupPath & "\img\02\1.gif")
                            Users(2).Image = Image.FromFile(Application.StartupPath & "\img\02\7.gif")
                            CurrentSelected = 3
                            Exit Sub
                        End If
                        If CurrentSelected = 2 Then
                            Users(1).Image = Image.FromFile(Application.StartupPath & "\img\02\2.gif")
                            Users(3).Image = Image.FromFile(Application.StartupPath & "\img\02\8.gif")
                            CurrentSelected = 4
                            Exit Sub
                        End If
                    Case 4
                        If CurrentSelected = 2 Then
                            Users(1).Image = Image.FromFile(Application.StartupPath & "\img\02\2.gif")
                            Users(0).Image = Image.FromFile(Application.StartupPath & "\img\02\5.gif")
                            CurrentSelected = 1
                            Exit Sub
                        End If
                        If CurrentSelected = 4 Then
                            Users(3).Image = Image.FromFile(Application.StartupPath & "\img\02\4.gif")
                            Users(2).Image = Image.FromFile(Application.StartupPath & "\img\02\7.gif")
                            CurrentSelected = 3
                            Exit Sub
                        End If
                    Case 5
                        If CurrentSelected = 1 Then
                            Unload_Mailbox()
                            Paint_Mailbox()
                        End If
                        If CurrentSelected = 2 Then
                            Unload_Mailbox()
                            Bandwidth()
                            Exit Sub
                        End If
                        If CurrentSelected = 3 Then
                            Unload_Mailbox()
                            Xband_News()
                            Exit Sub
                        End If
                        If CurrentSelected = 4 Then
                            Unload_Mailbox()
                            DRAW_NAV_MENU()
                            Exit Sub
                        End If
                End Select
            Case 4
                Select Case direction
                    Case 1
                        If CurrentSelected = 2 Then
                            Title(1).BackColor = Color.Transparent
                            Title(1).ForeColor = Color.Yellow
                            Title(0).BackColor = Color.Black
                            Title(0).ForeColor = Color.AliceBlue
                            CurrentSelected = 1
                            Exit Sub
                        End If
                        If CurrentSelected = 3 Then
                            Title(2).BackColor = Color.Transparent
                            Title(2).ForeColor = Color.Yellow
                            Title(1).BackColor = Color.Black
                            Title(1).ForeColor = Color.AliceBlue
                            CurrentSelected = 2
                            Exit Sub
                        End If
                        If CurrentSelected = 4 Then
                            Title(3).BackColor = Color.Transparent
                            Title(3).ForeColor = Color.Yellow
                            Title(2).BackColor = Color.Black
                            Title(2).ForeColor = Color.AliceBlue
                            CurrentSelected = 3
                            Exit Sub
                        End If
                        If CurrentSelected = 5 Then
                            Title(4).BackColor = Color.Transparent
                            Title(4).ForeColor = Color.Yellow
                            Title(3).BackColor = Color.Black
                            Title(3).ForeColor = Color.AliceBlue
                            CurrentSelected = 4
                            Exit Sub
                        End If
                        If CurrentSelected = 6 Then
                            Title(5).BackColor = Color.Transparent
                            Title(5).ForeColor = Color.Yellow
                            Title(4).BackColor = Color.Black
                            Title(4).ForeColor = Color.AliceBlue
                            CurrentSelected = 5
                            Exit Sub
                        End If
                        If CurrentSelected = 7 Then
                            Title(6).BackColor = Color.Transparent
                            Title(6).ForeColor = Color.Yellow
                            Title(5).BackColor = Color.Black
                            Title(5).ForeColor = Color.AliceBlue
                            CurrentSelected = 6
                            Exit Sub
                        End If

                    Case 3
                        If CurrentSelected = 1 Then
                            Title(0).BackColor = Color.Transparent
                            Title(0).ForeColor = Color.Yellow
                            Title(1).BackColor = Color.Black
                            Title(1).ForeColor = Color.AliceBlue
                            CurrentSelected = 2
                            Exit Sub
                        End If
                        If CurrentSelected = 2 Then
                            Title(1).BackColor = Color.Transparent
                            Title(1).ForeColor = Color.Yellow
                            Title(2).BackColor = Color.Black
                            Title(2).ForeColor = Color.AliceBlue
                            CurrentSelected = 3
                            Exit Sub
                        End If
                        If CurrentSelected = 3 Then
                            Title(2).BackColor = Color.Transparent
                            Title(2).ForeColor = Color.Yellow
                            Title(3).BackColor = Color.Black
                            Title(3).ForeColor = Color.AliceBlue
                            CurrentSelected = 4
                            Exit Sub
                        End If
                        If CurrentSelected = 4 Then
                            Title(3).BackColor = Color.Transparent
                            Title(3).ForeColor = Color.Yellow
                            Title(4).BackColor = Color.Black
                            Title(4).ForeColor = Color.AliceBlue
                            CurrentSelected = 5
                            Exit Sub
                        End If
                        If CurrentSelected = 5 Then
                            Title(4).BackColor = Color.Transparent
                            Title(4).ForeColor = Color.Yellow
                            Title(5).BackColor = Color.Black
                            Title(5).ForeColor = Color.AliceBlue
                            CurrentSelected = 6
                            Exit Sub
                        End If
                        If CurrentSelected = 6 Then
                            Title(5).BackColor = Color.Transparent
                            Title(5).ForeColor = Color.Yellow
                            Title(6).BackColor = Color.Black
                            Title(6).ForeColor = Color.AliceBlue
                            CurrentSelected = 7
                            Exit Sub
                        End If
                    Case 5
                        If CurrentSelected = 6 Then
                            Unload_Challenge()
                            Paint_Chat()
                            Exit Sub
                        End If
                        If CurrentSelected = 7 Then
                            Unload_Challenge()
                            DRAW_NAV_MENU()
                            Exit Sub
                        End If
                        If CurrentSelected = 1 Then
                            Unload_Challenge()
                            Wait_Screen(1)
                            Exit Sub
                        End If
                        If CurrentSelected = 2 Then
                            Unload_Challenge()
                            Wait_Screen(2)
                        End If
                        If CurrentSelected = 3 Then
                            Unload_Challenge()
                            Wait_Screen(3)
                        End If
                        If CurrentSelected = 4 Then
                            Unload_Challenge()
                            Wait_Screen(4)
                        End If
                        If CurrentSelected = 5 Then
                            Unload_Challenge()
                            Wait_Screen(5)
                        End If
                End Select
            Case 5
                Select Case direction
                    Case 1
                        If CurrentSelected = 2 Then
                            Title(1).BackColor = Color.Transparent
                            Title(1).ForeColor = Color.Yellow
                            Title(0).BackColor = Color.Black
                            Title(0).ForeColor = Color.AliceBlue
                            CurrentSelected = 1
                            Exit Sub
                        End If
                        If CurrentSelected = 3 Then
                            Title(2).BackColor = Color.Transparent
                            Title(2).ForeColor = Color.Yellow
                            Title(1).BackColor = Color.Black
                            Title(1).ForeColor = Color.AliceBlue
                            CurrentSelected = 2
                            Exit Sub
                        End If
                    Case 3
                        If CurrentSelected = 1 Then
                            Title(0).BackColor = Color.Transparent
                            Title(0).ForeColor = Color.Yellow
                            Title(1).BackColor = Color.Black
                            Title(1).ForeColor = Color.AliceBlue
                            CurrentSelected = 2
                            Exit Sub
                        End If
                        If CurrentSelected = 2 Then
                            Title(1).BackColor = Color.Transparent
                            Title(1).ForeColor = Color.Yellow
                            Title(2).BackColor = Color.Black
                            Title(2).ForeColor = Color.AliceBlue
                            CurrentSelected = 3
                            Exit Sub
                        End If
                        If CurrentSelected = 3 Then
                            Title(2).BackColor = Color.Transparent
                            Title(2).ForeColor = Color.Yellow
                            Title(3).BackColor = Color.Black
                            Title(3).ForeColor = Color.AliceBlue
                            CurrentSelected = 4
                            Exit Sub
                        End If
                    Case 5
                        If CurrentSelected = 1 Then
                            Dim blah As String
                            blah = InputBox("Enter your profile information here", "Temporary Profile Placeholder")
                            blah = blah.Replace("'", "`")
                            If blah = "" Then
                                Exit Sub
                            End If
                            Dim txtblah As New TextBox
                            txtblah = New TextBox
                            SQL1.ExecuteQuery("UPDATE player SET profile = '" & blah & "' WHERE id = '" & UserID & "';")
                            SQL1.Next()
                        End If
                        If CurrentSelected = 2 Then
                            Dim blah As String
                            blah = InputBox("Enter your Taunt here", "Temporary Taunt Placeholder")
                            blah = blah.Replace("'", "`")
                            If blah = "" Then
                                Exit Sub
                            End If
                            Dim txtblah As New TextBox
                            txtblah = New TextBox
                            SQL1.ExecuteQuery("UPDATE player SET Taunt = '" & blah & "' WHERE id = '" & UserID & "';")
                            SQL1.Next()
                        End If
                        If CurrentSelected = 3 Then
                            Unload_Options()
                            DRAW_NAV_MENU()
                        End If
                End Select
            Case 6
                Select Case direction
                    Case 2
                        If CurrentSelected = 5 Then
                            Exit Sub
                        End If
                        CurrentSelected = CurrentSelected + 1
                        SQL1.ExecuteQuery("SELECT * FROM games WHERE id=" & CurrentSelected)
                        SQL1.Next()
                        info1.Text = SQL1.FieldData(1).data.ToString
                        SQL2.ExecuteQuery("SELECT IFNULL(Wins,0), IFNULL(Losses,0), IFNULL(Draws,0) FROM " &
"( SELECT COUNT(DidWin) AS Wins FROM playerwinloss WHERE GameId = " & CurrentSelected & " AND PlayerId = " & UserID & " AND DidWin = 1) As WinTable, " &
"( SELECT COUNT(DidWin) AS Losses FROM playerwinloss WHERE GameId = " & CurrentSelected & " AND PlayerId = " & UserID & " AND DidWin = 0) As LossTable, " &
"( SELECT COUNT(DidWin) AS Draws FROM playerwinloss WHERE GameId = " & CurrentSelected & " AND PlayerId = " & UserID & " AND DidWin = 2) As DrawTable ")
                        SQL2.Next()
                        Title(0).Text = "Wins : " & SQL2.FieldData(0).data.ToString
                        Title(1).Text = "Losses : " & SQL2.FieldData(1).data.ToString
                        Title(2).Text = "Ties : " & SQL2.FieldData(2).data.ToString

                    Case 4
                        If CurrentSelected = 1 Then
                            Exit Sub
                        End If
                        CurrentSelected = CurrentSelected - 1
                        SQL1.ExecuteQuery("SELECT * FROM games WHERE id=" & CurrentSelected)
                        SQL1.Next()
                        info1.Text = SQL1.FieldData(1).data.ToString
                        SQL2.ExecuteQuery("SELECT IFNULL(Wins,0), IFNULL(Losses,0), IFNULL(Draws,0) FROM " &
"( SELECT COUNT(DidWin) AS Wins FROM playerwinloss WHERE GameId = " & CurrentSelected & " AND PlayerId = " & UserID & " AND DidWin = 1) As WinTable, " &
"( SELECT COUNT(DidWin) AS Losses FROM playerwinloss WHERE GameId = " & CurrentSelected & " AND PlayerId = " & UserID & " AND DidWin = 0) As LossTable, " &
"( SELECT COUNT(DidWin) AS Draws FROM playerwinloss WHERE GameId = " & CurrentSelected & " AND PlayerId = " & UserID & " AND DidWin = 2) As DrawTable ")
                        SQL2.Next()
                        Title(0).Text = "Wins : " & SQL2.FieldData(0).data.ToString
                        Title(1).Text = "Losses : " & SQL2.FieldData(1).data.ToString
                        Title(2).Text = "Ties : " & SQL2.FieldData(2).data.ToString
                    Case 5
                        unload_statistics()
                        DRAW_NAV_MENU()
                End Select
        End Select
    End Sub
    Private Sub Unload_Options()
        For i = 0 To 2
            Controls.Remove(Title(i))
        Next
    End Sub
    Private Sub Unload_Main_Menu()
        For i = 0 To 5
            Controls.Remove(Users(i))
        Next

    End Sub

    Private Sub tmrFlash_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tmrFlash.Tick
        Select Case Flashing
            Case 0
                If lblYes.ForeColor = Color.Black Then
                    lblYes.ForeColor = Color.Red
                    Exit Sub
                End If
                If lblYes.ForeColor = Color.Red Then
                    lblYes.ForeColor = Color.Black
                    Exit Sub
                End If
            Case 1
                If lblNo.ForeColor = Color.Black Then
                    lblNo.ForeColor = Color.Red
                    Exit Sub
                End If
                If lblNo.ForeColor = Color.Red Then
                    lblNo.ForeColor = Color.Black
                    Exit Sub
                End If
        End Select
    End Sub

    Private Sub Paint_New_User()
        Unload_User_Menu()
        UzerName = New TextBox
        info1 = New Label
        info2 = New Label
        info1.AutoSize = True
        'info2.AutoSize = True
        info2.Size = New Point(500, 150)
        UzerName.Size = New Point(180, 0)
        info1.Font = New Font("System", 18, FontStyle.Bold)
        info2.Font = New Font("System", 14, FontStyle.Regular)
        UzerName.Font = New Font("System", 14, FontStyle.Regular)
        UzerName.Location = New Point(165, 200)
        info1.Location = New Point(110, 0)
        info2.Location = New Point(5, 50)
        info1.Text = "Enter Your Code Name"
        info2.Text = "A Code Name is part of your Player ID. It's a name you choose for yourself so that other XBAND players can find you. You don't have to use your real name -- make up something like ''Thrasher'' or ''Electric Ninja''."
        info1.ForeColor = Color.MediumBlue
        info2.ForeColor = Color.Lime
        info2.BackColor = Color.Black
        info1.BackColor = Color.Black
        info1.TabIndex = 23
        info2.TabIndex = 24
        UzerName.BorderStyle = BorderStyle.Fixed3D
        UzerName.MaxLength = 13
        UzerName.BackColor = Color.Black
        UzerName.ForeColor = Color.Yellow
        TheBox = New Panel
        TheBox.Size = New Point(500, 250)
        TheBox.BackColor = Color.Black
        TheBox.Location = New Point(40, 50)
        TheBox.Controls.Add(info1)
        TheBox.Controls.Add(info2)
        TheBox.Controls.Add(UzerName)
        Controls.Add(TheBox)
        AddHandler UzerName.KeyDown, AddressOf Uzername_Keydown
        UzerName.Select()
        UzerName.Focus()
    End Sub

    Private Sub Unload_User_Menu()
        SuspendLayout()
        For i = 0 To 3
            Controls.Remove(Users(i))
            Controls.Remove(Title(i))
        Next
        Controls.Remove(info1)
        Controls.Remove(info2)
        ResumeLayout()
    End Sub

    Private Sub wmp_GotFocus(ByVal sender As Object, ByVal e As System.EventArgs)
        Me.Select()
    End Sub

    Private Sub Uzername_Keydown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs)
        CurrentState = 1
        Select Case e.KeyData
            Case Keys.Enter
                UzerName.Text = UzerName.Text.Replace("'", "`")
                UzerName.Text = UzerName.Text.Replace(" ", "_")
                If LCase(UzerName.Text) = "admin" Then
                    UzerName.Enabled = False
                    Dim pineapple As New WebBrowser
                    pineapple = New WebBrowser
                    pineapple.Url = New Uri("http://xband.byethost33.com")
                    pineapple.Size = New Point(600, 600)
                    pineapple.IsWebBrowserContextMenuEnabled = False
                    pineapple.WebBrowserShortcutsEnabled = False
                    Controls.Add(pineapple)
                    pineapple.BringToFront()
                    pineapple.Focus()
                    Exit Sub
                End If
                If Len(UzerName.Text) < 2 Then
                    XbMsgbox("Your username must be at least 2 characters long!")
                    Exit Sub
                End If
                Dim sqldata1 As TextBox
                sqldata1 = New TextBox
                UzerName.Enabled = False
                SQL1.AddTextBoxReference(sqldata1, "Username")
                SQL1.ExecuteQuery("SELECT * FROM player WHERE Username='" & UzerName.Text & "';")
                SQL1.Next()
                If sqldata1.Text.ToLower = UzerName.Text.ToLower Then
                    XbMsgbox("That username is already in use by another player! Please choose a different one!")
                    UzerName.Enabled = True
                    UzerName.Text = ""
                    Exit Sub
                Else
                    storeusername = UzerName.Text
                    UzerName.Text = ""
                    UzerName.Enabled = True
                    RemoveHandler UzerName.KeyDown, AddressOf Uzername_Keydown
                    AddHandler UzerName.KeyDown, AddressOf Pazzword_Keydown
                    UzerName.Select()
                    UzerName.Focus()
                    Password_Registration()
                End If
        End Select
    End Sub

    Private Sub Password_Registration()
        info1.Text = "  Choose a Password"
        info2.Text = "Xband 2.0 requires you choose a password for account recovery purposes. We recommend that you not give your password out to anyone as the staff of Xband 2.0 will be able to do nothing for you aside delete your account. This unfortunately means you will lose your stats :("
        'Pazzword = New TextBox
        'Pazzword.Size = New Point(180, 0)
        'Pazzword.Font = New Font("System", 14, FontStyle.Regular)
        'Pazzword.Location = New Point(165, 200)
        'Pazzword.BorderStyle = BorderStyle.Fixed3D
        'Pazzword.MaxLength = 13
        'Pazzword.BackColor = Color.Black
        'Pazzword.ForeColor = Color.Yellow
        AddHandler UzerName.KeyDown, AddressOf Pazzword_Keydown
        UzerName.PasswordChar = "*"
    End Sub
    Private Sub Pazzword_Keydown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs)
        Select Case e.KeyData
            Case Keys.Enter
                If Len(UzerName.Text) < 5 Then
                    'UzerName.TabIndex = 59
                    'UzerName.TabStop = False
                    UzerName.Text = ""
                    info2.Text = "Your Password must be at least 5 characters long!"
                    Exit Sub
                End If
                storepassword = UzerName.Text
                Controls.Remove(TheBox)
                Character_Selection()
        End Select
    End Sub

    Private Sub Character_Selection()
        CurrentICN = 1
        UserIcon = New PictureBox
        info1 = New Label
        info2 = New Label
        info1.AutoSize = True
        'info2.AutoSize = True
        info2.Size = New Point(500, 150)
        UzerName.Size = New Point(180, 0)
        info1.Font = New Font("System", 18, FontStyle.Bold)
        info2.Font = New Font("System", 14, FontStyle.Regular)
        UzerName.Font = New Font("System", 14, FontStyle.Regular)
        UzerName.Location = New Point(165, 200)
        info1.Location = New Point(110, 0)
        info2.Location = New Point(5, 50)
        info1.Text = "    Choose A Character"
        info2.Text = "Your Character appears with your Code Name when you challenge other XBAND players -- pick one that reflects who you want to be. Hit Left and Right to see the Characters; use Up and Down to change colors."
        info1.ForeColor = Color.MediumBlue
        info2.ForeColor = Color.Lime
        info2.BackColor = Color.Black
        info1.BackColor = Color.Black
        info1.TabIndex = 23
        info2.TabIndex = 24
        TheBox = New Panel
        TheBox.Size = New Point(500, 350)
        TheBox.BackColor = Color.Black
        TheBox.Location = New Point(40, 50)
        TheBox.Controls.Add(info1)
        TheBox.Controls.Add(info2)
        UserIcon.Image = Image.FromFile(Application.StartupPath & "\img\1.gif")
        UserIcon.Size = New Point(70, 100)
        UserIcon.SizeMode = PictureBoxSizeMode.StretchImage
        UserIcon.Location = New Point(210, 200)
        Controls.Add(TheBox)
        TheBox.Controls.Add(UserIcon)
        AddHandler TheBox.KeyDown, AddressOf IconChooser_Keydown
        TheBox.Select()
        TheBox.Focus()
    End Sub
    Private Sub IconChooser_Keydown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs)
        Select Case e.KeyCode
            Case Keys.Left
                If CurrentICN <= 1 Then
                    CurrentICN = 40
                    UserIcon.Image = Image.FromFile(Application.StartupPath & "\img\40.gif")
                    Exit Sub
                End If
                CurrentICN = CurrentICN - 1
                UserIcon.Image = Image.FromFile(Application.StartupPath & "\img\" & CurrentICN & ".gif")

            Case Keys.Right
                If CurrentICN >= 40 Then
                    CurrentICN = 1
                    UserIcon.Image = Image.FromFile(Application.StartupPath & "\img\1.gif")
                    Exit Sub
                End If
                CurrentICN = CurrentICN + 1
                UserIcon.Image = Image.FromFile(Application.StartupPath & "\img\" & CurrentICN & ".gif")
            Case Keys.Enter
                storeicon = CurrentICN
                Controls.Remove(TheBox)
                Dim cmd As String
                cmd = "INSERT INTO player(Username,Password,IconID) VALUES('" & storeusername & "','" & storepassword & "','" + storeicon + "');"
                Dim tmp As Integer
                tmp = SQL1.ExecuteNonQuery(cmd)
                If tmp = "-1" Then
                    MsgBox("EPIC FAIL! There was a problem submitting data to the XBAND 2 Servers!! Try again later :(", MsgBoxStyle.Critical)
                    Exit Sub
                End If
                Dim fileContents() As String
                fileContents = New String() {}
                Array.Resize(fileContents, 8)
                fileContents = System.IO.File.ReadAllLines(Application.StartupPath + "//settings.ini")
                fileContents(CurrentSelected) = storeicon
                fileContents(CurrentSelected + 4) = storeusername
                System.IO.File.WriteAllLines(Application.StartupPath + "//settings.ini", fileContents)
                SQL1.ExecuteQuery("SELECT * FROM player WHERE Username='" & storeusername & "';")
                SQL1.Next()
                UserID = SQL1.FieldData(0).data
                cmd = "INSERT INTO xmail(PlayerId,RecipientId,Subject,Message,DateTime) VALUES('" & "0" & "','" & UserID & "','" + "Welcome to XBAND 2!" + "','" + "Hey " & storeusername & "!" & vbNewLine & "Welcome back to XBAND! If you have any questions, complaints, or find any bugs, feel free to message us here at username XBAND!" + vbNewLine + "- The XBAND 2 Dev Team" + "',NOW());"
                tmp = SQL1.ExecuteNonQuery(cmd)
                If tmp = "-1" Then
                    MsgBox("EPIC FAIL! There was a problem submitting data to the XBAND 2 Servers!! Try again later :(", MsgBoxStyle.Critical)
                    Exit Sub
                End If
                DRAW_NAV_MENU()

        End Select
    End Sub
    Private Sub BringPasswordUp()
        Dim TheInfo As Label
        TheBox2 = New Panel
        TheBox2.Size = New Point(500, 200)
        TheBox2.Location = New Point(50, 50)
        Controls.Add(TheBox2)
        TheInfo = New Label
        TheInfo.Size = New Point(465, 100)
        TheInfo.TextAlign = ContentAlignment.TopLeft
        TheInfo.BackColor = Color.Black
        TheInfo.ForeColor = Color.LimeGreen
        TheInfo.Font = New Font("Terminal", 14, FontStyle.Bold)
        'TheBox.BorderStyle = BorderStyle.Fixed3D
        TheBox2.Controls.Add(TheInfo)
        TheInfo.Location = New Point(30, 20)
        txtPassword = New TextBox
        txtPassword.BackColor = Color.Black
        txtPassword.ForeColor = Color.Yellow
        txtPassword.Location = New Point(150, 150)
        txtPassword.PasswordChar = "*"
        AddHandler txtPassword.KeyDown, AddressOf txtpassword_Keydown
        TheBox2.Controls.Add(txtPassword)
        TheBox2.BringToFront()
        TheBox2.BackColor = Color.Black
        TheBox2.TabIndex = 0
        TheInfo.Text = "Please enter your password to login to the Xband 2 Network."
        lblNo = New Label
        lblNo.Text = "OK"
        lblNo.BackColor = Color.Transparent
        lblNo.ForeColor = Color.Black
        lblNo.Font = New Font(TheInfo.Font.FontFamily, 14, FontStyle.Bold)
        lblNo.AutoSize = True
        lblNo.Location = New Point(200, 150)
        txtPassword.Size = New Point(180, 0)
        TheBox2.Controls.Add(lblNo)
        Flashing = 1
        tmrFlash.Enabled = True
        txtPassword.Select()
        txtPassword.Focus()
    End Sub

    Private Sub txtpassword_Keydown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs)
        Select Case e.KeyCode
            Case Keys.Enter

                Dim sqldata1 As New TextBox
                sqldata1 = New TextBox
                SQL1.AddTextBoxReference(sqldata1, "Password")
                SQL1.ExecuteQuery("SELECT * FROM player WHERE Username='" & Title(CurrentSelected).Text & "';")
                SQL1.Next()
                If txtPassword.Text = sqldata1.Text Then
                    storeusername = Title(CurrentSelected).Text
                    storeicon = SQL1.FieldData(5).data
                    Controls.Remove(TheBox2)
                    Unload_User_Menu()
                    DRAW_NAV_MENU()

                Else
                    Controls.Remove(TheBox2)
                    XbMsgbox("The password you entered was incorrect :(")
                    wrongattempts = wrongattempts + 1
                    If wrongattempts = 12 Then
                        Dim pineapple As New WebBrowser
                        pineapple = New WebBrowser
                        pineapple.Url = New Uri("http://xband.byethost33.com/showthread.php?tid=3")
                        pineapple.Size = New Point(600, 600)
                        pineapple.IsWebBrowserContextMenuEnabled = False
                        pineapple.WebBrowserShortcutsEnabled = False
                        Controls.Add(pineapple)
                        pineapple.BringToFront()
                        pineapple.Focus()
                    End If

                End If

        End Select
    End Sub
    Private Sub DRAW_NAV_MENU()
        Dim sqldata As TextBox
        sqldata = New TextBox
        SQL1.ExecuteQuery("SELECT * FROM player WHERE Username='" & storeusername & "';")
        SQL1.Next()
        UserID = SQL1.FieldData(0).data
        storeicon = SQL1.FieldData(5).data
        CurrentState = 2
        CurrentSelected = 1
        Dim b As Integer
        Dim c As Integer
        b = 120
        c = 120
        Array.Resize(Users, 6)
        For i = 0 To 5
            Users(i) = New PictureBox()
            Users(i).Size = New Point(100, 100)
            Dim x As Integer = (i Mod 3) * 200 + 50
            '325:
            '+ 90
            Dim y As Integer = Math.Floor(i / 3) * 200 + 50
            Users(i).Location = New Point(x, y)
            Users(i).BackColor = Color.Transparent
            Users(i).BorderStyle = BorderStyle.None
            'AddHandler Users(i).Click, AddressOf Users_Click
            Users(i).SizeMode = PictureBoxSizeMode.StretchImage
            'Users(i).Tag = filecontents(i)
            Users(i).Image = Image.FromFile(Application.StartupPath & "\img\01\" & i & ".gif")
            'Users(i).Image.PixelFormat(System.Drawing.ImagiFormat16bppGrayScale)
            Controls.Add(Users(i))
        Next
        Users(0).Image = Image.FromFile(Application.StartupPath & "\img\01\" & 6 & ".gif")
        Me.ResumeLayout()
        Me.Select()
        Me.Focus()
    End Sub

    Private Sub Bandwidth()
        ' CurrentState = 50
        Browzer = New WebBrowser
        Browzer.Navigate("http://xband.freeiz.com/bandwith.html")
        Browzer.ScrollBarsEnabled = False
        Browzer.IsWebBrowserContextMenuEnabled = False
        Browzer.WebBrowserShortcutsEnabled = False
        Browzer.Size = New Point(530, 400)
        AddHandler Browzer.PreviewKeyDown, AddressOf Newspaper_PreviewKeydown
        Browzer.Location = New Point(20, 20)
        backbutton = New PictureBox
        backbutton.Image = Image.FromFile(Application.StartupPath & "\img\01\11.gif")
        backbutton.Location = New Point(250, 414)
        backbutton.BackColor = Color.Transparent
        backbutton.Size = New Point(100, 100)
        Controls.Add(Browzer)
        Controls.Add(backbutton)
        Browzer.Focus()
        ' Browzer.Select()
    End Sub
    Private Sub Xband_News()
        'CurrentState = 50
        Browzer = New WebBrowser
        Browzer.Navigate("http://xband.byethost33.com/showthread.php?tid=4")
        Browzer.ScrollBarsEnabled = False
        Browzer.IsWebBrowserContextMenuEnabled = False
        Browzer.WebBrowserShortcutsEnabled = False
        Browzer.Size = New Point(530, 400)
        Browzer.Location = New Point(20, 20)
        backbutton = New PictureBox
        backbutton.Image = Image.FromFile(Application.StartupPath & "\img\01\11.gif")
        backbutton.Location = New Point(250, 414)
        backbutton.BackColor = Color.Transparent
        backbutton.Size = New Point(100, 100)
        AddHandler Browzer.PreviewKeyDown, AddressOf Newspaper_PreviewKeydown

        Controls.Add(Browzer)
        Controls.Add(backbutton)
        Browzer.Focus()
        'Browzer.Select()
    End Sub
    Private Sub Paint_Challenge_Menu()
        Array.Resize(Title, 7)
        Dim x As Integer
        Dim y As Integer
        x = 150
        y = 100
        SQL1.ExecuteQuery("SELECT * FROM games;")
        For i = 0 To 4
            SQL1.Next()
            Title(i) = New Label
            Title(i).BackColor = Color.Transparent
            Title(i).AutoSize = False
            Title(i).Font = New Font("System", 20, FontStyle.Bold, GraphicsUnit.Pixel)
            Title(i).Size = New Point(300, 50)
            Title(i).ForeColor = Color.Yellow
            Title(i).Location = New Point(x, y)
            Title(i).Text = SQL1.FieldData(1).data.ToString
            'x = x + 100
            y = y + 50
            Controls.Add(Title(i))
        Next
        Title(5) = New Label
        Title(5).BackColor = Color.Transparent
        Title(5).AutoSize = False
        Title(5).Font = New Font("System", 20, FontStyle.Bold, GraphicsUnit.Pixel)
        Title(5).Size = New Point(300, 50)
        Title(5).ForeColor = Color.Yellow
        Title(5).Location = New Point(x, y)
        Title(5).Text = "Midnight Lounge"
        Title(0).ForeColor = Color.AliceBlue
        Title(0).BackColor = Color.Black
        Controls.Add(Title(5))
        y = y + 50
        Title(6) = New Label
        Title(6).BackColor = Color.Transparent
        Title(6).AutoSize = False
        Title(6).Font = New Font("System", 20, FontStyle.Bold, GraphicsUnit.Pixel)
        Title(6).Size = New Point(300, 50)
        Title(6).ForeColor = Color.Yellow
        Title(6).Location = New Point(x, y)
        Title(6).Text = "Back"
        Controls.Add(Title(6))
        CurrentSelected = 1
        CurrentState = 4
        Me.Focus()
        Me.Select()
    End Sub
    Private Sub Tacoz_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs)
        chatcontrol.Disconnect()
        chatcontrol.DisposeControl()
        Controls.Remove(chatcontrol)
        Controls.Remove(tacoz)
        DRAW_NAV_MENU()
    End Sub

    Private Sub FilterColors(ByRef Colors As List(Of Color))
        Dim lst As New List(Of Color)

        For Each c As Color In Colors
            Dim avg As Integer = 0
            avg += c.R
            avg += c.G
            avg += c.B
            avg /= 3
            If avg < 100 Then
                lst.Add(c)
            End If
        Next
        For Each c As Color In lst
            Colors.Remove(c)
        Next
    End Sub
    Private Sub Paint_Chat()


        Dim cols As New List(Of Color)
        Dim typ As Type = chatcolor.GetType()
        Dim pi() As System.Reflection.PropertyInfo = typ.GetProperties()
        For Each p As System.Reflection.PropertyInfo In pi
            If p.PropertyType.Name <> "Color" Then Continue For
            cols.Add(p.GetValue(chatcolor, Nothing))
        Next

        FilterColors(cols)
        Dim awesome As Integer
        awesome = Int(Rnd() * cols.Count)
        chatcolor = cols(awesome)






        chatcontrol = New ChatControl.ChatControl
        chatcontrol.Location = New Point(0, 0)
        'chatcontrol.Dock = DockStyle.Fill
        chatcontrol.Hostname = "127.0.0.1"
        chatcontrol.Port = "7945"
        chatcontrol.UserName = storeusername
        chatcontrol.Connect()
        chatcontrol.RichTextBox.BackColor = Color.Black


        chatcontrol.SendStartupInfo(storeusername, chatcolor.ToArgb)
        chatconnected = 1
        Me.SuspendLayout()
        Me.Controls.Add(chatcontrol)
        Me.ResumeLayout()
        tacoz = New Button
        tacoz.Text = "LEAVE"
        tacoz.Location = New Point(490, 450)
        Controls.Add(tacoz)
        AddHandler tacoz.Click, AddressOf Tacoz_MouseDown
    End Sub

    Private Sub Paint_Mail_Menu()
        CurrentState = 3
        Dim b As Integer
        Dim c As Integer
        b = 120
        c = 120
        For i = 0 To 3
            Users(i) = New PictureBox()
            Users(i).Size = New Point(100, 100)
            Dim x As Integer = (i Mod 2) * 325 + 90
            Dim y As Integer = 25 + (Math.Floor(i / 2) * 250)
            Users(i).Location = New Point(x, y)
            'Users(i).BorderStyle = BorderStyle.None
            'AddHandler Users(i).Click, AddressOf Users_Click
            Users(i).IsAccessible = False
            Users(i).BackColor = Color.Transparent
            Users(i).SizeMode = PictureBoxSizeMode.StretchImage
            Users(i).Image = Image.FromFile(Application.StartupPath & "\img\02\" & i + 1 & ".gif")
            'Users(i).Image.PixelFormat(System.Drawing.ImagiFormat16bppGrayScale)
            Controls.Add(Users(i))
        Next
        Users(0).Image = Image.FromFile(Application.StartupPath & "\img\02\5.gif")
        CurrentSelected = 1
        Me.Focus()
        Me.Select()
    End Sub

    Private Sub Unload_Mailbox()
        For i = 0 To 3
            Controls.Remove(Users(i))
        Next
    End Sub

    Private Sub Newspaper_PreviewKeydown(ByVal sender As Object, ByVal e As System.Windows.Forms.PreviewKeyDownEventArgs)
        Select Case e.KeyCode
            Case Keys.Enter
                Controls.Remove(backbutton)
                Browzer.Dispose()
                Paint_Mail_Menu()
        End Select
    End Sub

    Private Sub Paint_Mailbox()
        SQL1.ExecuteQuery("SELECT * FROM xmail WHERE RecipientId = '" & UserID & "';")
        CurrentState = 9
        CurrentSelected = 1
        dg = New DataGridView
        dg.Location = New System.Drawing.Point(15, 25)
        dg.Size = New System.Drawing.Point(540, 350)
        dg.HorizontalScrollingOffset = 1000
        dg.ColumnCount = 6
        dg.EnableHeadersVisualStyles = False
        dg.ColumnHeadersBorderStyle = DataGridViewHeaderBorderStyle.None
        dg.AllowUserToAddRows = False
        dg.Columns(0).Name = "ID"
        dg.Columns(1).Name = "From"
        dg.Columns(2).Name = "Title"
        dg.Columns(3).Name = "Message"
        dg.Columns(4).Name = "IconID"
        dg.Columns(5).Name = "DateTime"
        dg.Columns(0).Visible = False
        dg.Columns(3).Visible = False
        dg.Columns(4).Visible = False
        dg.Columns(5).Visible = False
        dg.AutoSizeRowsMode = DataGridViewAutoSizeRowsMode.AllCells
        dg.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.AllCells
        'dg.AutoSize = True
        dg.EditMode = DataGridViewEditMode.EditProgrammatically
        dg.DefaultCellStyle.Padding = New System.Windows.Forms.Padding(0, 0, 120, 0)
        dg.SelectionMode = DataGridViewSelectionMode.FullRowSelect
        dg.BackgroundColor = Color.Black
        AddHandler dg.KeyDown, AddressOf dg_KeyDown
        dg.CellBorderStyle = DataGridViewCellBorderStyle.None
        dg.ForeColor = Color.CornflowerBlue
        dg.RowHeadersDefaultCellStyle.BackColor = Color.CornflowerBlue
        dg.ColumnHeadersDefaultCellStyle.Padding = New Windows.Forms.Padding(0, 0, 0, 10)
        dg.ColumnHeadersHeightSizeMode = DataGridViewColumnHeadersHeightSizeMode.AutoSize
        dg.RowsDefaultCellStyle.BackColor = Color.Black
        dg.ColumnHeadersDefaultCellStyle.BackColor = Color.Black
        dg.ColumnHeadersDefaultCellStyle.ForeColor = Color.CornflowerBlue
        dg.ColumnHeadersDefaultCellStyle.Font = New System.Drawing.Font("system", 14, FontStyle.Bold, GraphicsUnit.Pixel)
        dg.RowsDefaultCellStyle.Font = New System.Drawing.Font("system", 14, FontStyle.Bold, GraphicsUnit.Pixel)
        dg.RowHeadersVisible = False
        dg.ColumnHeadersDefaultCellStyle.Font = New System.Drawing.Font("system", 16, FontStyle.Bold, GraphicsUnit.Pixel)
        dg.ColumnHeadersDefaultCellStyle.BackColor = Color.Black
        dg.CellBorderStyle = DataGridViewCellBorderStyle.None
        Controls.Add(dg)
        Dim j As Integer
        j = 20
        For i = 0 To 3
            Users(i) = New PictureBox
            Users(i).Image = Image.FromFile(Application.StartupPath & "\img\03\" & i + 1 & ".gif")
            Users(i).Size = New Point(100, 100)
            Users(i).Location = New Point(j, 375)
            Users(i).BackColor = Color.Transparent
            Controls.Add(Users(i))
            j = j + 150
        Next
        Users(0).Image = Image.FromFile(Application.StartupPath & "\img\03\5.gif")
        dg.Select()
        dg.Focus()
        'CurrentSelected = 4

        SQL1.Next()
        If SQL1.FieldData(0).data = Nothing Then
            Exit Sub
        End If
        SQL2.ExecuteQuery("SELECT * FROM player WHERE Id = '" & SQL1.FieldData(1).data & "';")
        SQL2.Next()
        dg.Rows.Add(SQL1.FieldData(0).data, SQL2.FieldData(1).data, SQL1.FieldData(3).data, SQL1.FieldData(4).data, SQL2.FieldData(5).data, SQL1.FieldData(5).data)
        Do Until SQL1.Next = False
            SQL2.ExecuteQuery("SELECT * FROM player WHERE Id = '" & SQL1.FieldData(1).data & "';")
            SQL2.Next()
            dg.Rows.Add(SQL1.FieldData(0).data, SQL2.FieldData(1).data, SQL1.FieldData(3).data, SQL1.FieldData(4).data, SQL2.FieldData(5).data, SQL1.FieldData(5).data)
        Loop
    End Sub
    Private Sub dg_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs)
        Select Case e.KeyCode
            Case Keys.Right
                Dim music As String = Application.StartupPath & "\mp3\sound.wav" ' *.wav file location
                Dim media As New Media.SoundPlayer(music)
                media.Play() ' Async, creates a new thread
                If CurrentSelected = 1 Then
                    CurrentSelected = 2
                    Users(0).Image = Image.FromFile(Application.StartupPath & "\img\03\1.gif")
                    Users(1).Image = Image.FromFile(Application.StartupPath & "\img\03\6.gif")
                    Exit Sub
                End If
                If CurrentSelected = 2 Then
                    CurrentSelected = 3
                    Users(1).Image = Image.FromFile(Application.StartupPath & "\img\03\2.gif")
                    Users(2).Image = Image.FromFile(Application.StartupPath & "\img\03\7.gif")
                    Exit Sub
                End If
                If CurrentSelected = 3 Then
                    CurrentSelected = 4
                    Users(2).Image = Image.FromFile(Application.StartupPath & "\img\03\3.gif")
                    Users(3).Image = Image.FromFile(Application.StartupPath & "\img\03\8.gif")
                    Exit Sub
                End If
            Case Keys.Left
                Dim music As String = Application.StartupPath & "\mp3\sound.wav" ' *.wav file location
                Dim media As New Media.SoundPlayer(music)
                media.Play() ' Async, creates a new thread
                If CurrentSelected = 2 Then
                    CurrentSelected = 1
                    Users(1).Image = Image.FromFile(Application.StartupPath & "\img\03\2.gif")
                    Users(0).Image = Image.FromFile(Application.StartupPath & "\img\03\5.gif")
                    Exit Sub
                End If
                If CurrentSelected = 3 Then
                    CurrentSelected = 2
                    Users(2).Image = Image.FromFile(Application.StartupPath & "\img\03\3.gif")
                    Users(1).Image = Image.FromFile(Application.StartupPath & "\img\03\6.gif")
                    Exit Sub
                End If
                If CurrentSelected = 4 Then
                    CurrentSelected = 3
                    Users(3).Image = Image.FromFile(Application.StartupPath & "\img\03\4.gif")
                    Users(2).Image = Image.FromFile(Application.StartupPath & "\img\03\7.gif")
                    Exit Sub
                End If
            Case Keys.Enter
                Dim music As String = Application.StartupPath & "\mp3\sound.wav" ' *.wav file location
                Dim media As New Media.SoundPlayer(music)
                media.Play() ' Async, creates a new thread
                Select Case CurrentSelected
                    Case 1

                        If dg.RowCount = 0 Then
                            Exit Sub
                        End If
                        Dim i As Integer
                        i = dg.CurrentRow.Index
                        Dim tempo(6) As String
                        tempo(0) = dg.Item(4, i).Value.ToString
                        tempo(1) = dg.Item(1, i).Value.ToString
                        tempo(2) = dg.Item(2, i).Value.ToString
                        tempo(3) = dg.Item(3, i).Value.ToString
                        tempo(4) = dg.Item(5, i).Value.ToString
                        tempo(5) = dg.Item(0, i).Value.ToString
                        Unload_Mail_Screen()
                        READ_MAIL(tempo(0), tempo(1), tempo(2), tempo(3), tempo(4), tempo(5))
                    Case 2
                        If dg.RowCount = 0 Then
                            Exit Sub
                        End If
                        SQL1.ExecuteNonQuery("DELETE FROM xmail WHERE Id='" & dg.Item(0, dg.CurrentRow.Index).Value & "';")
                        SQL1.Next()
                        Unload_Mail_Screen()
                        Paint_Mailbox()

                    Case 3
                        Unload_Mail_Screen()
                        Write_Mail("")

                    Case 4
                        Unload_Mail_Screen()
                        Paint_Mail_Menu()
                End Select
        End Select
    End Sub
    Private Sub Unload_Mail_Screen()
        dg.Dispose()
        For i = 0 To 3
            Controls.Remove(Users(i))
        Next
    End Sub
    Private Sub READ_MAIL(ByVal PlayerPic As String, ByVal FromUsername As String, ByVal Subject As String, ByVal Message As String, ByVal DateTime As String, ByVal msgid As String)
        Users(0) = New PictureBox
        Users(0).Image = Image.FromFile(Application.StartupPath & "\img\" & PlayerPic & ".gif")
        Users(0).Size = New Point(70, 100)
        Users(0).Location = New Point(50, 50)
        Users(0).SizeMode = PictureBoxSizeMode.StretchImage
        maildata(0) = New Label
        maildata(1) = New Label
        maildata(0).BackColor = Color.Transparent
        maildata(0).ForeColor = Color.Crimson
        maildata(0).Font = New Font("system", 24, FontStyle.Bold, GraphicsUnit.Pixel)
        maildata(0).AutoSize = True
        maildata(0).Location = New Point(120, 50)
        maildata(0).Text = FromUsername
        maildata(1).BackColor = Color.Transparent
        maildata(1).ForeColor = Color.CornflowerBlue
        maildata(1).AutoSize = True
        maildata(1).Font = New Font("system", 20, FontStyle.Bold, GraphicsUnit.Pixel)
        maildata(1).Text = "Date Sent : " & DateTime
        maildata(1).Location = New Point(120, 85)
        maildata(2) = New Label
        maildata(2).AutoSize = True
        maildata(2).BackColor = Color.Transparent
        maildata(2).ForeColor = Color.CornflowerBlue
        maildata(2).Text = "Title : " & Subject
        maildata(2).Location = New Point(120, 123)
        maildata(2).Font = New Font("system", 20, FontStyle.Bold, GraphicsUnit.Pixel)
        maildat = New TextBox
        maildat.Multiline = True
        maildat.ReadOnly = True
        maildat.Size = New Point(500, 240)
        maildat.BackColor = Color.Black
        maildat.Font = New Font("system", 16, FontStyle.Bold, GraphicsUnit.Pixel)
        maildat.ForeColor = Color.CornflowerBlue
        maildat.Location = New Point(50, 160)
        maildat.Text = Message
        Controls.Add(Users(0))
        Controls.Add(maildata(0))
        Controls.Add(maildata(1))
        Controls.Add(maildata(2))
        Controls.Add(maildat)
        Users(1) = New PictureBox
        Users(2) = New PictureBox
        Dim q As Integer
        q = 0
        Users(1).Size = New Point(100, 100)
        Users(1).Image = Image.FromFile(Application.StartupPath & "\img\05\" & 3 & ".gif")
        Users(1).Location = New Point(q + 150, 400)
        Users(1).BackColor = Color.Transparent
        Controls.Add(Users(1))
        Users(2).Size = New Point(100, 100)
        Users(2).Image = Image.FromFile(Application.StartupPath & "\img\05\" & 2 & ".gif")
        Users(2).Location = New Point(q + 300, 400)
        Users(2).BackColor = Color.Transparent
        Controls.Add(Users(2))
        CurrentSelected = 1
        AddHandler maildat.KeyDown, AddressOf Readmail_KeyDown
        maildat.Focus()
        maildat.Select()
        maildat.DeselectAll()
    End Sub

    Private Sub Unload_Mail_Message()
        For i = 0 To 2
            maildata(i).Dispose()
            maildat.Dispose()
        Next
    End Sub

    Private Sub Write_Mail(ByVal MailTo As String)
        maildata(0) = New Label
        maildata(1) = New Label
        maildata(0).Text = "To :"
        maildata(0).BackColor = Color.Transparent
        maildata(0).ForeColor = Color.Yellow
        maildata(0).Font = New Font("system", 20, FontStyle.Bold, GraphicsUnit.Pixel)
        maildata(1).Text = "Title :"
        maildata(1).BackColor = Color.Transparent
        maildata(1).ForeColor = Color.Yellow
        maildata(1).Font = New Font("system", 20, FontStyle.Bold, GraphicsUnit.Pixel)
        maildata(0).AutoSize = True
        maildata(1).AutoSize = True
        maildata(0).Location = New Point(50, 50)
        maildata(1).Location = New Point(50, 90)
        Controls.Add(maildata(0))
        Controls.Add(maildata(1))
        mailinput(0) = New TextBox
        mailinput(0).BorderStyle = BorderStyle.Fixed3D
        mailinput(0).Font = New Font("system", 20, FontStyle.Bold, GraphicsUnit.Pixel)
        mailinput(0).BackColor = Color.Black
        mailinput(0).ForeColor = Color.White
        mailinput(0).Location = New Point(103, 50)
        mailinput(0).Size = New Point(420, 20)
        Controls.Add(mailinput(0))
        mailinput(1) = New TextBox
        mailinput(1).BorderStyle = BorderStyle.Fixed3D
        mailinput(1).Font = New Font("system", 20, FontStyle.Bold, GraphicsUnit.Pixel)
        mailinput(1).BackColor = Color.Black
        mailinput(1).ForeColor = Color.White
        mailinput(1).Location = New Point(118, 90)
        mailinput(1).Size = New Point(405, 20)
        Controls.Add(mailinput(1))
        mailinput(2) = New TextBox
        mailinput(2).BorderStyle = BorderStyle.Fixed3D
        mailinput(2).Font = New Font("system", 20, FontStyle.Bold, GraphicsUnit.Pixel)
        mailinput(2).BackColor = Color.Black
        mailinput(2).Multiline = True
        mailinput(2).ForeColor = Color.White
        mailinput(2).Location = New Point(25, 125)
        mailinput(2).Size = New Point(500, 250)
        AddHandler mailinput(0).KeyDown, AddressOf MailWrite_KeyDown
        AddHandler mailinput(1).KeyDown, AddressOf MailWrite_KeyDown
        AddHandler mailinput(2).KeyDown, AddressOf MailWrite_KeyDown
        CurrentSelected = 1
        Controls.Add(mailinput(2))
        mailinput(0).Text = MailTo
        Users(0) = New PictureBox
        Users(1) = New PictureBox
        Dim q As Integer
        q = 0
        Users(0).Size = New Point(100, 100)
        Users(0).Image = Image.FromFile(Application.StartupPath & "\img\04\" & 3 & ".gif")
        Users(0).Location = New Point(q + 150, 375)
        Users(0).BackColor = Color.Transparent
        Controls.Add(Users(0))
        Users(1).Size = New Point(100, 100)
        Users(1).Image = Image.FromFile(Application.StartupPath & "\img\04\" & 2 & ".gif")
        Users(1).Location = New Point(q + 300, 375)
        Users(1).BackColor = Color.Transparent
        Controls.Add(Users(1))
        mailinput(0).Focus()
        mailinput(0).Select()
    End Sub
    Private Sub MailWrite_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs)
        Select Case e.KeyCode
            Case Keys.Right
                If CurrentSelected = 1 Then
                    Users(0).Image = Image.FromFile(Application.StartupPath & "\img\04\1.gif")
                    Users(1).Image = Image.FromFile(Application.StartupPath & "\img\04\4.gif")
                    CurrentSelected = 2
                    Exit Sub
                End If
            Case Keys.Left
                If CurrentSelected = 2 Then
                    Users(0).Image = Image.FromFile(Application.StartupPath & "\img\04\3.gif")
                    Users(1).Image = Image.FromFile(Application.StartupPath & "\img\04\2.gif")
                    CurrentSelected = 1
                    Exit Sub
                End If
            Case Keys.Enter
                If CurrentSelected = 2 Then
                    UnloadNewMM()
                    Paint_Mailbox()
                End If

                If CurrentSelected = 1 Then
                    If mailinput(0).Text = "" Then
                        MsgBox("DO NOT USE BLANK BOXES!")
                        Exit Sub
                    End If
                    If mailinput(1).Text = "" Then
                        MsgBox("DO NOT USE BLANK BOXES!")
                        Exit Sub
                    End If
                    If mailinput(2).Text = "" Then
                        MsgBox("DO NOT USE BLANK BOXES!")
                        Exit Sub
                    End If
                    mailinput(0).Enabled = False
                    mailinput(1).Enabled = False
                    mailinput(2).Enabled = False
                    SQL2.ExecuteQuery("SELECT * FROM player WHERE Username='" & mailinput(0).Text & "';")
                    SQL2.Next()
                    If SQL2.FieldData(1).data = Nothing Then
                        MsgBox("The user you were trying to mail does not exist!")
                        UnloadNewMM()
                        Paint_Mailbox()
                        Exit Sub
                    End If

                    Dim ogurt As String
                    ogurt = SQL2.FieldData(0).data.ToString
                    mailinput(1).Text = mailinput(1).Text.Replace("'", "`")
                    mailinput(2).Text = mailinput(2).Text.Replace("'", "`")
                    Dim cmd As String
                    Dim tmp As Integer
                    cmd = "INSERT INTO xmail(PlayerId,RecipientId,Subject,Message,DateTime) VALUES('" & UserID & "','" & ogurt & "','" + mailinput(1).Text + "','" + mailinput(2).Text + "',NOW());"
                    tmp = SQL1.ExecuteNonQuery(cmd)
                    If tmp = "-1" Then
                        'MsgBox("EPIC FAIL! There was a problem submitting data to the XBAND 2 Servers!! Try again later :(", MsgBoxStyle.Critical)
                        tmp = SQL1.ExecuteNonQuery(cmd)
                        If tmp = "-1" Then
                            MsgBox("EPIC FAIL! There was a problem submitting data to the XBAND 2 Servers!! Try again later :(", MsgBoxStyle.Critical)
                            UnloadNewMM()
                            Paint_Mailbox()
                        End If
                        Exit Sub
                    End If
                End If
                UnloadNewMM()
                Paint_Mailbox()
        End Select
    End Sub

    Private Sub UnloadNewMM()
        For i = 0 To 2
            mailinput(i).Dispose()
        Next
        maildata(0).Dispose()
        maildata(1).Dispose()
        Controls.Remove(Users(0))
        Controls.Remove(Users(1))
    End Sub
    Private Sub Readmail_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs)
        Select Case e.KeyCode
            Case Keys.Right
                If CurrentSelected = 1 Then
                    Users(1).Image = Image.FromFile(Application.StartupPath & "\img\05\1.gif")
                    Users(2).Image = Image.FromFile(Application.StartupPath & "\img\05\4.gif")
                    CurrentSelected = 2
                    Exit Sub
                End If
            Case Keys.Left
                If CurrentSelected = 2 Then
                    Users(1).Image = Image.FromFile(Application.StartupPath & "\img\05\3.gif")
                    Users(2).Image = Image.FromFile(Application.StartupPath & "\img\05\2.gif")
                    CurrentSelected = 1
                    Exit Sub
                End If
            Case Keys.Enter
                If CurrentSelected = 2 Then
                    UnloadReadmail()
                    Paint_Mailbox()
                    Exit Sub
                End If
                If CurrentSelected = 1 Then
                    Dim temp12 As String
                    temp12 = maildata(0).Text
                    UnloadReadmail()
                    Write_Mail(temp12)
                End If

        End Select
    End Sub

    Private Sub UnloadReadmail()
        Controls.Remove(Users(0))
        Controls.Remove(Users(1))
        Controls.Remove(Users(2))
        maildat.Dispose()
        maildata(0).Dispose()
        maildata(1).Dispose()
        maildata(2).Dispose()
    End Sub

    Private Sub frmMain_MenuComplete(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.MenuComplete

    End Sub

    Private Sub frmMain_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Me.MouseDown

    End Sub

    Private Sub frmMain_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles Me.Paint

    End Sub

    Private Sub wmp_Enter(ByVal sender As System.Object, ByVal e As System.EventArgs)

    End Sub

    Private Sub wmp_PlayStateChange(ByVal sender As Object, ByVal e As AxWMPLib._WMPOCXEvents_PlayStateChangeEvent)
        Select Case e.newState.ToString
            Case 1
                tmrMusic.Enabled = True
        End Select
    End Sub

    Private Sub Paint_Buddy_List()
        SQL1.ExecuteQuery("SELECT * FROM buddylist WHERE MYID = '" & UserID & "';")
        CurrentState = 9
        CurrentSelected = 1
        dg = New DataGridView
        dg.Location = New System.Drawing.Point(15, 25)
        dg.Size = New System.Drawing.Point(540, 350)
        'dg.HorizontalScrollingOffset = 1000
        dg.ColumnCount = 7
        dg.EnableHeadersVisualStyles = False
        dg.ColumnHeadersBorderStyle = DataGridViewHeaderBorderStyle.None
        dg.AllowUserToAddRows = False
        dg.Columns(0).Name = "Code Name"
        dg.Columns(1).Name = "Last Play"
        dg.Columns(2).Name = "Win - Loss"
        dg.Columns(3).Name = "IconID"
        dg.Columns(4).Name = "Profile"
        dg.Columns(5).Name = "ID"
        dg.Columns(3).Visible = False
        dg.Columns(4).Visible = False
        dg.Columns(5).Visible = False
        dg.Columns(3).Resizable = DataGridViewTriState.False
        dg.Columns(4).Resizable = DataGridViewTriState.False
        dg.Columns(3).AutoSizeMode = DataGridViewAutoSizeColumnMode.None
        dg.Columns(4).AutoSizeMode = DataGridViewAutoSizeColumnMode.None
        dg.AutoSizeRowsMode = DataGridViewAutoSizeRowsMode.DisplayedCells
        dg.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.DisplayedCells
        'dg.AutoSize = True
        dg.EditMode = DataGridViewEditMode.EditProgrammatically
        dg.DefaultCellStyle.Padding = New System.Windows.Forms.Padding(0, 0, 100, 0)
        dg.SelectionMode = DataGridViewSelectionMode.FullRowSelect
        dg.BackgroundColor = Color.Black
        AddHandler dg.KeyDown, AddressOf dg2_KeyDown
        dg.CellBorderStyle = DataGridViewCellBorderStyle.None
        dg.ForeColor = Color.Lime
        dg.ScrollBars = ScrollBars.Vertical
        dg.AutoSizeRowsMode = DataGridViewAutoSizeRowsMode.DisplayedCells
        dg.RowHeadersDefaultCellStyle.BackColor = Color.CornflowerBlue
        dg.ColumnHeadersDefaultCellStyle.Padding = New Windows.Forms.Padding(0, 0, 0, 10)
        dg.ColumnHeadersHeightSizeMode = DataGridViewColumnHeadersHeightSizeMode.AutoSize
        dg.RowsDefaultCellStyle.BackColor = Color.Black
        dg.ColumnHeadersDefaultCellStyle.BackColor = Color.Black
        dg.ColumnHeadersDefaultCellStyle.ForeColor = Color.Lime
        dg.ColumnHeadersDefaultCellStyle.Font = New System.Drawing.Font("system", 14, FontStyle.Bold, GraphicsUnit.Pixel)
        dg.RowsDefaultCellStyle.Font = New System.Drawing.Font("system", 14, FontStyle.Bold, GraphicsUnit.Pixel)
        dg.RowHeadersVisible = False
        dg.ColumnHeadersDefaultCellStyle.Font = New System.Drawing.Font("system", 16, FontStyle.Bold, GraphicsUnit.Pixel)
        dg.ColumnHeadersDefaultCellStyle.BackColor = Color.Black
        dg.CellBorderStyle = DataGridViewCellBorderStyle.None
        Controls.Add(dg)
        Dim j As Integer
        j = 20
        For i = 0 To 3
            Users(i) = New PictureBox
            Users(i).Image = Image.FromFile(Application.StartupPath & "\img\06\" & i + 1 & ".gif")
            Users(i).Size = New Point(100, 100)
            Users(i).Location = New Point(j, 375)
            Users(i).BackColor = Color.Transparent
            Controls.Add(Users(i))
            j = j + 150
        Next
        Users(0).Image = Image.FromFile(Application.StartupPath & "\img\06\5.gif")
        dg.Select()
        dg.Focus()
        'CurrentSelected = 4

        SQL1.Next()
        If SQL1.FieldData(0).data = Nothing Then
            Exit Sub
        End If
        Dim temp1 As String
        Dim temp2 As String
        Dim temp3 As String
        SQL2.ExecuteQuery("SELECT * FROM player WHERE Id = '" & SQL1.FieldData(2).data & "';")
        SQL2.Next()
        temp1 = SQL2.FieldData(1).data
        temp2 = SQL2.FieldData(5).data
        temp3 = SQL2.FieldData(4).data
        SQL2.ExecuteQuery("SELECT IFNULL(Wins,0), IFNULL(Losses,0), IFNULL(Draws,0) FROM " &
"( SELECT COUNT(DidWin) AS Wins FROM playerwinloss WHERE OpponentID = " & SQL1.FieldData(2).data & " AND PlayerId = " & UserID & " AND DidWin = 1) As WinTable, " &
"( SELECT COUNT(DidWin) AS Losses FROM playerwinloss WHERE OpponentID = " & SQL1.FieldData(2).data & " AND PlayerId = " & UserID & " AND DidWin = 0) As LossTable, " &
"( SELECT COUNT(DidWin) AS Draws FROM playerwinloss WHERE OpponentID = " & SQL1.FieldData(2).data & " AND PlayerId = " & UserID & " AND DidWin = 2) As DrawTable ")
        SQL2.Next()
        dg.Rows.Add(temp1, "N/A Yet!", SQL2.FieldData(0).data & "/" & SQL2.FieldData(1).data, temp2, temp3, SQL1.FieldData(0).data)
        Do Until SQL1.Next = False
            SQL2.ExecuteQuery("SELECT * FROM player WHERE Id = '" & SQL1.FieldData(2).data & "';")
            SQL2.Next()
            temp1 = SQL2.FieldData(1).data
            temp2 = SQL2.FieldData(5).data
            temp3 = SQL2.FieldData(4).data
            SQL2.ExecuteQuery("SELECT IFNULL(Wins,0), IFNULL(Losses,0), IFNULL(Draws,0) FROM " &
"( SELECT COUNT(DidWin) AS Wins FROM playerwinloss WHERE OpponentID = " & SQL1.FieldData(2).data & " AND PlayerId = " & UserID & " AND DidWin = 1) As WinTable, " &
"( SELECT COUNT(DidWin) AS Losses FROM playerwinloss WHERE OpponentID = " & SQL1.FieldData(2).data & " AND PlayerId = " & UserID & " AND DidWin = 0) As LossTable, " &
"( SELECT COUNT(DidWin) AS Draws FROM playerwinloss WHERE OpponentID = " & SQL1.FieldData(2).data & " AND PlayerId = " & UserID & " AND DidWin = 2) As DrawTable ")
            SQL2.Next()
            dg.Rows.Add(temp1, "N/A Yet!", SQL2.FieldData(0).data & "/" & SQL2.FieldData(1).data, temp2, temp3, SQL1.FieldData(0).data)
        Loop
    End Sub
    Private Sub dg2_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs)
        Select Case e.KeyCode
            Case Keys.Right
                If CurrentSelected = 1 Then
                    CurrentSelected = 2
                    Users(0).Image = Image.FromFile(Application.StartupPath & "\img\06\1.gif")
                    Users(1).Image = Image.FromFile(Application.StartupPath & "\img\06\6.gif")
                    Exit Sub
                End If
                If CurrentSelected = 2 Then
                    CurrentSelected = 3
                    Users(1).Image = Image.FromFile(Application.StartupPath & "\img\06\2.gif")
                    Users(2).Image = Image.FromFile(Application.StartupPath & "\img\06\7.gif")
                    Exit Sub
                End If
                If CurrentSelected = 3 Then
                    CurrentSelected = 4
                    Users(2).Image = Image.FromFile(Application.StartupPath & "\img\06\3.gif")
                    Users(3).Image = Image.FromFile(Application.StartupPath & "\img\06\8.gif")
                    Exit Sub
                End If
            Case Keys.Left
                If CurrentSelected = 2 Then
                    CurrentSelected = 1
                    Users(1).Image = Image.FromFile(Application.StartupPath & "\img\06\2.gif")
                    Users(0).Image = Image.FromFile(Application.StartupPath & "\img\06\5.gif")
                    Exit Sub
                End If
                If CurrentSelected = 3 Then
                    CurrentSelected = 2
                    Users(2).Image = Image.FromFile(Application.StartupPath & "\img\06\3.gif")
                    Users(1).Image = Image.FromFile(Application.StartupPath & "\img\06\6.gif")
                    Exit Sub
                End If
                If CurrentSelected = 4 Then
                    CurrentSelected = 3
                    Users(3).Image = Image.FromFile(Application.StartupPath & "\img\06\4.gif")
                    Users(2).Image = Image.FromFile(Application.StartupPath & "\img\06\7.gif")
                    Exit Sub
                End If
            Case Keys.Enter
                If CurrentSelected = 1 Then
                    Dim i As Integer
                    If dg.RowCount = 0 Then
                        Exit Sub
                    End If
                    i = dg.CurrentRow.Index
                    Dim tempo(5) As String
                    tempo(0) = dg.Item(0, i).Value.ToString
                    tempo(1) = dg.Item(1, i).Value.ToString
                    tempo(2) = dg.Item(2, i).Value.ToString
                    tempo(3) = dg.Item(3, i).Value.ToString
                    tempo(4) = dg.Item(4, i).Value.ToString
                    Unload_Buddy_List()
                    View_Profile(tempo(3), tempo(0), tempo(2), tempo(1), tempo(4))
                End If
                If CurrentSelected = 2 Then
                    If dg.RowCount = 0 Then
                        Exit Sub
                    End If
                    SQL1.ExecuteNonQuery("DELETE FROM buddylist WHERE ID='" & dg.Item(5, dg.CurrentRow.Index).Value & "';")
                    SQL1.Next()
                    Unload_Buddy_List()
                    Paint_Buddy_List()
                End If
                If CurrentSelected = 3 Then
                    Dim baconman As String
                    baconman = InputBox("Enter the Username you wish to add to your buddy list", "Buddy Add")
                    SQL1.ExecuteQuery("SELECT * FROM player WHERE Username = '" & baconman & "';")
                    SQL1.Next()
                    If SQL1.FieldData(1).data = Nothing Then
                        MsgBox("That username is not valid!")
                        Exit Sub
                    End If
                    If SQL1.FieldData(1).data.ToString = "" Then
                        MsgBox("That username is not valid!")
                        Exit Sub
                    End If
                    Dim cmd As String
                    Dim tmp As Integer
                    cmd = "INSERT INTO buddylist(MYID,FRIENDID) VALUES('" & UserID & "','" & SQL1.FieldData(0).data.ToString & "');"
                    tmp = SQL2.ExecuteNonQuery(cmd)
                    If tmp = "-1" Then
                        'MsgBox("EPIC FAIL! There was a problem submitting data to the XBAND 2 Servers!! Try again later :(", MsgBoxStyle.Critical)
                        tmp = SQL2.ExecuteNonQuery(cmd)
                        If tmp = "-1" Then
                            MsgBox("EPIC FAIL! There was a problem submitting data to the XBAND 2 Servers!! Try again later :(", MsgBoxStyle.Critical)
                        End If
                    End If
                    Unload_Buddy_List()
                    Paint_Buddy_List()
                End If
                If CurrentSelected = 4 Then
                    Unload_Buddy_List()
                    DRAW_NAV_MENU()
                End If
        End Select
    End Sub

    Private Sub Unload_Buddy_List()
        dg.Dispose()
        For i = 0 To 3
            Controls.Remove(Users(i))
        Next
    End Sub
    Private Sub View_Profile(ByVal PlayerPic As String, ByVal Username As String, ByVal WinLoss As String, ByVal LastPlay As String, ByVal Profile As String)
        Users(0) = New PictureBox
        Users(0).Image = Image.FromFile(Application.StartupPath & "\img\" & PlayerPic & ".gif")
        Users(0).Size = New Point(70, 100)
        Users(0).Location = New Point(50, 50)
        Users(0).SizeMode = PictureBoxSizeMode.StretchImage
        maildata(0) = New Label
        maildata(1) = New Label
        maildata(0).BackColor = Color.Transparent
        maildata(0).ForeColor = Color.Crimson
        maildata(0).Font = New Font("system", 24, FontStyle.Bold, GraphicsUnit.Pixel)
        maildata(0).AutoSize = True
        maildata(0).Location = New Point(120, 50)
        maildata(0).Text = Username
        maildata(1).BackColor = Color.Transparent
        maildata(1).ForeColor = Color.Lime
        maildata(1).AutoSize = True
        maildata(1).Font = New Font("system", 20, FontStyle.Bold, GraphicsUnit.Pixel)
        maildata(1).Text = "Last Played : " & LastPlay
        maildata(1).Location = New Point(120, 85)
        maildata(2) = New Label
        maildata(2).AutoSize = True
        maildata(2).BackColor = Color.Transparent
        maildata(2).ForeColor = Color.Lime
        maildata(2).Text = "Win - Loss : " & WinLoss
        maildata(2).Location = New Point(120, 123)
        maildata(2).Font = New Font("system", 20, FontStyle.Bold, GraphicsUnit.Pixel)
        maildat = New TextBox
        maildat.Multiline = True
        maildat.ReadOnly = True
        maildat.Size = New Point(500, 240)
        maildat.BackColor = Color.Black
        maildat.Font = New Font("system", 16, FontStyle.Bold, GraphicsUnit.Pixel)
        maildat.ForeColor = Color.Lime
        maildat.Location = New Point(50, 160)
        maildat.Text = Profile
        Controls.Add(Users(0))
        Controls.Add(maildata(0))
        Controls.Add(maildata(1))
        Controls.Add(maildata(2))
        Controls.Add(maildat)
        Users(1) = New PictureBox
        Users(2) = New PictureBox
        Dim q As Integer
        q = 0
        Users(1).Size = New Point(100, 100)
        Users(1).Image = Image.FromFile(Application.StartupPath & "\img\05\4.gif")
        Users(1).Location = New Point(q + 225, 400)
        Users(1).BackColor = Color.Transparent
        Controls.Add(Users(1))
        CurrentSelected = 1
        AddHandler maildat.KeyDown, AddressOf ProfileScreen_KeyDown
        maildat.Focus()
        maildat.Select()
        maildat.DeselectAll()
    End Sub
    Private Sub ProfileScreen_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs)
        Select Case e.KeyCode
            Case Keys.Enter
                Controls.Remove(Users(0))
                Controls.Remove(Users(1))
                For i = 0 To 2
                    Controls.Remove(maildata(i))
                Next
                Controls.Remove(maildat)
                Paint_Buddy_List()
        End Select
    End Sub
    Private Sub Unload_Challenge()
        For i = 0 To 6
            Controls.Remove(Title(i))
        Next
        CurrentState = 9
    End Sub

    Private Sub Paint_Options()
        Dim x As Integer
        Dim y As Integer
        x = 150
        y = 100
        For i = 0 To 2
            Title(i).BackColor = Color.Transparent
            Title(i).AutoSize = False
            Title(i).Font = New Font("System", 20, FontStyle.Bold, GraphicsUnit.Pixel)
            Title(i).Size = New Point(300, 50)
            Title(i).ForeColor = Color.Yellow
            Title(i).Location = New Point(x, y)
            Title(i).Text = SQL1.FieldData(1).data.ToString
            'x = x + 100
            y = y + 50
            Controls.Add(Title(i))
        Next
        CurrentSelected = 1
        Title(0).Text = "Profile"
        Title(1).Text = "Taunt"
        Title(2).Text = "Back"
        Title(0).BackColor = Color.Black
        Title(0).ForeColor = Color.AliceBlue
        CurrentState = 5
    End Sub

    Private Sub Matchmake(ByVal GameID As Integer)
        overmind = New XbandMatchTracker.MatchTracker
        Dim matchmaker As New XbandMatchMakingClient.XbandMatchClient

        Me.gameId = GameID

        matchmaker.Host = "127.0.0.1"
        matchmaker.Port = 10000
        'wmp.Ctlcontrols.pause()
        matchmaker.StartFindingMatch(storeusername, GameID)
        'tmrMatch.Enabled = True

    End Sub

    Private Sub MatchMakingMatchFound(ByVal Opponent As String, ByVal RemoteEndPoint As String, ByVal isHost As Boolean)
        Try

            Dim sw As New Stopwatch()
            Dim myIPAdddress As String = ""
            Dim fullPath As String = Application.StartupPath + "\zsnes\"
            Dim gameFile As String = gameId.ToString + ".smc"

            Dim h As System.Net.IPHostEntry = System.Net.Dns.GetHostEntry(System.Net.Dns.GetHostName)
            For Each ipa As System.Net.IPAddress In h.AddressList
                Select Case ipa.AddressFamily
                    Case Net.Sockets.AddressFamily.InterNetwork
                        myIPAdddress = ipa.ToString()
                        Exit For

                End Select
            Next

            Me.opponent = Opponent
            Me.gameName = ""

            'Dim nupnp As New NATUPNPLib.UPnPNATClass()
            'Dim prtCll As NATUPNPLib.IStaticPortMappingCollection = Nothing
            'If nupnp Is Nothing Then
            '    MessageBox.Show("Your router may not be able to support upnp, you may either have to manually port foward, or turn on the upnp service")

            'Else

            '    prtCll = nupnp.StaticPortMappingCollection

            '    If isHost And Not prtCll Is Nothing Then
            '        'Set the Ports
            '        'prtCll.Add(7845, "TCP", 7845, myIPAdddress, True, "NETPLAY")
            '        'MessageBox.Show(myIPAdddress)
            '        AddPort(prtCll, 7845, myIPAdddress, "NETPLAY")
            '    ElseIf prtCll Is Nothing And isHost Then
            '        MessageBox.Show("Your router may not be able to support upnp, you may either have to manually port foward, or turn on the upnp service")
            '    End If
            'End If


            If System.IO.File.Exists("c:\" + gameFile) Then
                System.IO.File.Delete("c:\" + gameFile)
            End If
            System.IO.File.Copy(fullPath + "\" + gameFile, "c:\" + gameFile)

            'If System.IO.File.Exists(fullPath + "zsnesw.cfg") Then

            '    Dim fs As New System.IO.FileStream(fullPath + "zsnesw.cfg", IO.FileMode.Open, IO.FileAccess.ReadWrite)
            '    Dim buf(fs.Length - 1) As Byte
            '    fs.Read(buf, 0, fs.Length)
            '    Dim lstBuff As New List(Of Byte)(buf)
            '    Dim strFile As String = System.Text.ASCIIEncoding.ASCII.GetString(buf)
            '    Dim ind As Integer = strFile.IndexOf("GameDirectory = ") + "GameDirectory = ".Length
            '    strFile = strFile.Substring(0, ind)
            '    strFile += fullPath
            '    fs.Close()
            '    System.IO.File.WriteAllText(fullPath + "zsnesw.cfg", strFile)

            'End If
            Dim path As String = gameId.ToString() + ".smc"
            'MessageBox.Show(System.Environment.OSVersion.Version.Major.ToString())
            'Select Case System.Environment.OSVersion.Version.Major
            '    Case 6
            '        path = Application.StartupPath + "\zsnes\" + path
            'End Select
            Dim arguments As String = storeusername + " " + path
            If isHost Then
                arguments = "/USLQ2 " + arguments
            Else
                arguments = "/UCLQ2 " + arguments + " " +
                    RemoteEndPoint.Remove(RemoteEndPoint.IndexOf(":"))
                sw.Start()
                While sw.Elapsed.TotalSeconds < 7
                End While
                sw.Stop()
            End If

            formCommands.Push(25)
            sw.Reset()
            sw.Start()
            While sw.Elapsed.TotalSeconds < 15
            End While
            sw.Stop()

            Dim zsnes As New Process()
            zsnes.StartInfo = New ProcessStartInfo(
            Application.StartupPath + "\\zsnes\\zsnesw.exe", arguments)
            overmind.Process = zsnes

            'MessageBox.Show("Hosting? -" + isHost.ToString() + vbNewLine + _
            '                "Arguments - " + arguments + vbNewLine + _
            '                "EndPoint - " + RemoteEndPoint + " Zsnes db V: 2")


            matchmaker.Ready()
            overmind.StartGame(gameId, 2)
            'Me.Hide()
            formCommands.Push(0)
            formCommands.Push(99)
            Dim properlyDone As Boolean = True
            While Not overmind.IsGameDone
                If overmind.Process.HasExited Then '
                    overmind.KillThread()
                    MessageBox.Show("Zsnes closed prematurely")
                    properlyDone = False
                    Exit While
                End If
                Threading.Thread.Sleep(10)
            End While
            If properlyDone Then

                Dim ps As New PlayerWinLoss()
                Dim winner As Integer = ps.DeterminWinLoss(gameId, overmind.KillAddressInfo)
                Dim query As String = "INSERT INTO playerwinloss(GameId,PlayerId,OpponentId,DidWin) " +
                "SELECT '" + gameId.ToString + "', '" + UserID.ToString() + "',player.ID,"

                Dim didWin As Integer = 0
                If winner = 1 And isHost Then
                    formCommands.Push(20)
                    didWin = 1
                ElseIf winner = 2 And isHost Then
                    formCommands.Push(21)
                ElseIf winner = 1 And Not isHost Then
                    formCommands.Push(21)
                ElseIf winner = 2 And Not isHost Then
                    didWin = 1
                    formCommands.Push(20)
                ElseIf winner = 3 Then
                    didWin = 2
                    formCommands.Push(22)
                End If
                query += didWin.ToString + " FROM player WHERE Username='" + Opponent + "';"
                SQL1.ExecuteNonQuery(query)

            End If
            formCommands.Push(50)
            formCommands.Push(100)
            formCommands.Push(1)
            System.IO.File.Delete("c:\" + gameId.ToString + ".smc")

            'If isHost And Not prtCll Is Nothing Then prtCll.Remove(7845, "TCP")

        Catch ex As Exception
            MessageBox.Show(ex.Message)
            formCommands.Push(50)
            formCommands.Push(100)
            formCommands.Push(1)
        End Try
    End Sub


    Private Sub tmrMatch_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tmrMatch.Tick
        tmrMatch.Enabled = False
        matchmaker.QuitFindingMatch()
        DRAW_NAV_MENU()
        XbMsgbox("Could not find an opponent! Try again, or harass people hanging out in the Midnight Lounge :)")

    End Sub

    Private Sub TauntScreenStart()
        Dim OpponentName As String = opponent
        Dim OpponentTaunt As String = ""

        TauntScreenPaint(OpponentName, OpponentTaunt, gameName)
    End Sub
    Private Sub TauntScreenPaint(ByVal OpponentName As String, ByVal OpponentTaunt As String, ByVal GameName As String)
        Controls.Remove(Users(0))
        Controls.Remove(info1)
        Dim baconbell As String
        SQL1.ExecuteQuery("SELECT * FROM player WHERE Username = '" & OpponentName & "';")
        SQL1.Next()
        OpponentTaunt = SQL1.FieldData(3).data.ToString
        baconbell = SQL1.FieldData(5).data.ToString
        SQL2.ExecuteQuery("SELECT * FROM player WHERE id = '" & UserID & "';")
        SQL2.Next()
        mytaunt = SQL2.FieldData(3).data.ToString
        info1 = New Label
        info2 = New Label
        info1.Text = storeusername
        info2.Text = OpponentName
        info1.Location = New Point(100, 50)
        info2.Location = New Point(350, 50)
        Users(0) = New PictureBox
        Users(1) = New PictureBox
        Users(0).Image = Image.FromFile(Application.StartupPath & "\img\" & storeicon & ".gif")
        Users(1).Image = Image.FromFile(Application.StartupPath & "\img\" & baconbell & ".gif")
        Title(0) = New Label
        Title(1) = New Label
        Title(0).Text = mytaunt
        Title(1).Text = OpponentTaunt
        Users(0).Size = New Point(70, 100)
        Users(1).Size = New Point(70, 100)
        Users(0).SizeMode = PictureBoxSizeMode.StretchImage
        Users(1).SizeMode = PictureBoxSizeMode.StretchImage
        Users(0).Location = New Point(100, 100)
        Users(1).Location = New Point(350, 100)
        Title(0).Location = New Point(50, 200)
        Title(1).Location = New Point(300, 200)
        Title(0).BackColor = Color.Transparent
        Title(1).BackColor = Color.Transparent
        Title(0).ForeColor = Color.Yellow
        Title(1).ForeColor = Color.Yellow
        info1.BackColor = Color.Transparent
        info2.BackColor = Color.Transparent
        info1.ForeColor = Color.Crimson
        info2.ForeColor = Color.Crimson
        info1.Size = New Point(150, 50)
        info2.Size = New Point(150, 50)
        info1.Font = New Font("system", 18, FontStyle.Bold, GraphicsUnit.Pixel)
        info2.Font = New Font("system", 18, FontStyle.Bold, GraphicsUnit.Pixel)
        Title(0).Font = New Font("system", 14, FontStyle.Bold, GraphicsUnit.Pixel)
        Title(1).Font = New Font("system", 14, FontStyle.Bold, GraphicsUnit.Pixel)
        Title(0).Size = New Point(200, 200)
        Title(1).Size = New Point(200, 200)
        Controls.Add(info1)
        Controls.Add(info2)
        Controls.Add(Users(0))
        Controls.Add(Users(1))
        Controls.Add(Title(0))
        Controls.Add(Title(1))
    End Sub
    Private Sub unload_taunt()
        Controls.Remove(info1)
        Controls.Remove(info2)
        Controls.Remove(Users(0))
        Controls.Remove(Users(1))
        Controls.Remove(Title(0))
        Controls.Remove(Title(1))
    End Sub
    Private Sub AddPort(ByVal ispmc As NATUPNPLib.IStaticPortMappingCollection,
                        ByVal Port As Integer, ByVal Ip As String, ByVal Description As String)
        Dim i As Integer
        Try
            Dim found As Boolean = False
            If ispmc.Count >= 0 Then

                For Each pm As NATUPNPLib.IStaticPortMapping In ispmc
                    If pm.InternalPort = Port Then
                        found = True
                        Exit For
                    End If
                    If ispmc.Count = i Then
                        Exit For
                    End If
                    i = i + 1
                Next

            End If
            If found Then ispmc.Remove(Port, "TCP")
            ispmc.Add(Port, "TCP", Port, Ip, True, Description)

        Catch ex As Exception
            MessageBox.Show(ex.Message & vbNewLine & "In English: We're having problems automatically forwarding your routers ports!" & vbNewLine & "Please report this error to XBAND via XMAIL or to the midnight lounge", "")
        End Try

    End Sub

    Private Sub Paint_Statistics()
        CurrentState = 6
        SQL1.ExecuteQuery("SELECT * FROM games")
        SQL1.Next()
        Users(0) = New PictureBox
        Users(0).Location = New Point(250, 100)
        Users(0).SizeMode = PictureBoxSizeMode.StretchImage
        Users(0).Size = New Point(70, 100)
        Users(0).Image = Image.FromFile(Application.StartupPath & "\img\" & storeicon & ".gif")
        Controls.Add(Users(0))
        info1 = New Label
        info1.Location = New Point(150, 80)
        info1.TextAlign = ContentAlignment.MiddleRight
        info1.Font = New Font("System", 18, FontStyle.Bold, GraphicsUnit.Pixel)
        info1.ForeColor = Color.Yellow
        info1.BackColor = Color.Transparent
        CurrentSelected = 1
        info1.Text = SQL1.FieldData(1).data.ToString
        info1.AutoSize = True
        Controls.Add(info1)
        info2 = New Label
        info2.Location = New Point(230, 200)
        info2.TextAlign = ContentAlignment.MiddleRight
        info2.Font = New Font("System", 18, FontStyle.Bold, GraphicsUnit.Pixel)
        info2.ForeColor = Color.Red
        info2.BackColor = Color.Transparent
        info2.Text = storeusername
        info2.AutoSize = True
        Controls.Add(info2)
        Title(0) = New Label
        Title(0).Location = New Point(230, 220)
        Title(0).TextAlign = ContentAlignment.MiddleRight
        Title(0).Font = New Font("System", 18, FontStyle.Bold, GraphicsUnit.Pixel)
        Title(0).ForeColor = Color.CornflowerBlue
        Title(0).BackColor = Color.Transparent
        Title(0).Text = "Wins : "
        Title(0).AutoSize = True
        Controls.Add(Title(0))
        Title(1) = New Label
        Title(1).Location = New Point(230, 240)
        Title(1).TextAlign = ContentAlignment.MiddleRight
        Title(1).Font = New Font("System", 18, FontStyle.Bold, GraphicsUnit.Pixel)
        Title(1).ForeColor = Color.CornflowerBlue
        Title(1).BackColor = Color.Transparent
        Title(1).Text = "Losses : "
        Title(1).AutoSize = True
        Controls.Add(Title(1))
        Title(2) = New Label
        Title(2).Location = New Point(230, 260)
        Title(2).TextAlign = ContentAlignment.MiddleRight
        Title(2).Font = New Font("System", 18, FontStyle.Bold, GraphicsUnit.Pixel)
        Title(2).ForeColor = Color.CornflowerBlue
        Title(2).BackColor = Color.Transparent
        Title(2).Text = "Ties : "
        Title(2).AutoSize = True
        Controls.Add(Title(2))
        SQL2.ExecuteQuery("SELECT IFNULL(Wins,0), IFNULL(Losses,0), IFNULL(Draws,0) FROM " &
    "( SELECT COUNT(DidWin) AS Wins FROM playerwinloss WHERE GameId = " & CurrentSelected & " AND PlayerId = " & UserID & " AND DidWin = 1) As WinTable, " &
    "( SELECT COUNT(DidWin) AS Losses FROM playerwinloss WHERE GameId = " & CurrentSelected & " AND PlayerId = " & UserID & " AND DidWin = 0) As LossTable, " &
    "( SELECT COUNT(DidWin) AS Draws FROM playerwinloss WHERE GameId = " & CurrentSelected & " AND PlayerId = " & UserID & " AND DidWin = 2) As DrawTable ")
        SQL2.Next()
        Title(0).Text = "Wins : " & SQL2.FieldData(0).data.ToString
        Title(1).Text = "Losses : " & SQL2.FieldData(1).data.ToString
        Title(2).Text = "Ties : " & SQL2.FieldData(2).data.ToString
    End Sub
    Private Sub unload_statistics()
        For i = 0 To 2
            Controls.Remove(Title(i))
        Next
        Controls.Remove(Users(0))
        Controls.Remove(info1)
        Controls.Remove(info2)
    End Sub

    Private Sub Wait_Screen(ByVal gametag As Integer)
        Users(0) = New PictureBox
        info1 = New Label
        Users(0).Image = Image.FromFile(Application.StartupPath & "\img\Xband2.jpg")
        Users(0).Location = New Point(190, 150)
        Users(0).SizeMode = PictureBoxSizeMode.AutoSize
        info1.AutoSize = True
        info1.Font = New Font("System", 18, FontStyle.Bold, GraphicsUnit.Pixel)
        info1.BackColor = Color.Transparent
        info1.ForeColor = Color.Yellow
        info1.Text = "GET READY WHILE WE FIND YOU AN OPPONENT!"
        info1.Location = New Point(50, 50)
        Controls.Add(Users(0))
        Controls.Add(info1)
        Matchmake(gametag)
        StopBackgroundSound()
        PlayRandomTrack()
    End Sub
    Sub StopBackgroundSound()
        My.Computer.Audio.Stop()
    End Sub


End Class
