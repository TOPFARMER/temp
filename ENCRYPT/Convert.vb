Public Declare auto Function dec2hex2 Lib "Dll2.dll" Alias "dec2hex2" (ByVal dec As Char()) As String()
Public Declare auto Function dec2hex1 Lib "Dll2.dll" Alias "dec2hex1" (ByVal dec As String()) As String()
Public Declare auto Function dec2hex Lib "Dll2.dll" Alias "dec2hex" (ByVal dec As String()) As String()
'''
'https://www.baidu.com/s?ie=UTF-8&wd=%E5%B0%BD%E9%87%8F%E4%B8%8D%E8%A6%81%E5%9C%A8Dll%E7%9A%84%E6%8E%A5%E5%8F%A3%E4%B8%AD%E4%BD%BF%E7%94%A8string%E4%BD%9C%E4%B8%BA%E5%8F%82%E6%95%B0
'接口必须重写
Public Function str2chars(ByVal str As String) As Char()
    Dim tmp_list As New List(Of Char)
    For i As Integer = 0 To str.Length - 1
        tmp_list.Add(str.Chars(i))
    Next
    Dim tmp_val() As Char = tmp_list.ToArray
    Return tmp_val
End Function
'''写到一半不想写了，CLI对应不上数据结构，连CHAR数组也不知道如何转接
'''

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