Public Class PlayerWinLoss

    Public Function DeterminWinLoss(ByVal GameID As Integer, ByVal lstBytes As List(Of Byte())) As Integer
        Dim winner As Integer = -1

        Select Case GameID

            Case 1
                winner = DetermineMarioKartBattle(lstBytes)
            Case 2
                winner = DetermineMarioKartMatch(lstBytes)
            Case 3
                winner = DetermineNBAJam(lstBytes)
            Case 4
                winner = DetermineTetrisAndDrMario(lstBytes)
            Case 5
                winner = DetermineSuperStreetFighter2(lstBytes)
        End Select

        Return winner
    End Function

    Private Function DetermineMarioKartBattle(ByVal lstBytes As List(Of Byte())) As Integer

        If lstBytes(0)(0) > lstBytes(1)(0) Then
            Return 1
        Else
            Return 2
        End If

        Return -1
    End Function
    Private Function DetermineMarioKartMatch(ByVal lstBytes As List(Of Byte())) As Integer

        If lstBytes(0)(0) > lstBytes(1)(0) Then
            Return 1
        Else
            Return 2
        End If

        Return -1
    End Function
    Private Function DetermineNBAJam(ByVal lstBytes As List(Of Byte())) As Integer

        If lstBytes(0)(0) > lstBytes(1)(0) Then
            Return 1
        ElseIf lstBytes(0)(0) < lstBytes(1)(0) Then
            Return 2
        ElseIf lstBytes(0)(0) = lstBytes(1)(0) Then
            Return 3
        End If

        Return -1
    End Function
    Private Function DetermineTetrisAndDrMario(ByVal lstBytes As List(Of Byte())) As Integer

        If BitConverter.ToUInt32(lstBytes(0), 0) > BitConverter.ToUInt32(lstBytes(1), 0) Then
            Return 1
        ElseIf BitConverter.ToUInt32(lstBytes(0), 0) < BitConverter.ToUInt32(lstBytes(1), 0) Then
            Return 2
        ElseIf BitConverter.ToUInt32(lstBytes(0), 0) = BitConverter.ToUInt32(lstBytes(1), 0) Then
            Return 3
        End If

        Return -1
    End Function
    Private Function DetermineSuperStreetFighter2(ByVal lstBytes As List(Of Byte())) As Integer

        If lstBytes(0)(0) + lstBytes(2)(0) > lstBytes(1)(0) + lstBytes(2)(0) Then
            Return 1
        ElseIf lstBytes(0)(0) + lstBytes(2)(0) < lstBytes(1)(0) + lstBytes(2)(0) Then
            Return 2
        ElseIf lstBytes(0)(0) + lstBytes(2)(0) = lstBytes(1)(0) + lstBytes(2)(0) Then
            Return 3
        End If

        Return -1
    End Function

End Class
