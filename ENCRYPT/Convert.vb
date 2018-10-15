Public Function str2hex(ByVal str As String) As String
    Dim tmp_hex As String
    For i As Integer = 0 To str.Length - 1
        tmp_hex = tmp_hex + hex(Asc(str.Chars(i)))
    Next
    Return tmp_hex
End Function

Public Function hex2str(ByVal S_hex As String) As String
    Dim str As String
    For i As Integer = 0 To S_hex.Length - 1 Step 2
        str = str + Chr("&H" & S_hex.SubString(i, 2))
    Next
    Return str
End Function
