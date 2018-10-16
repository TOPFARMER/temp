'''
'本来想直接调用别人写好的 C++ 文件

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
'''写到一半不想写了，CLS 对应不上数据结构，连CHAR数组也不知道如何转接
'''
'
'字符串（ ANSI 英文）转十六进制
Public Function str2hex(ByVal str As String) As String
    Dim tmp_hex As String
    For i As Integer = 0 To str.Length - 1
        tmp_hex = tmp_hex + hex(Asc(str.Chars(i)))
    Next
    Return tmp_hex
End Function

'十六进制转字符串（ ANSI 英文）
Public Function hex2str(ByVal S_hex As String) As String
    Dim str As String
    For i As Integer = 0 To S_hex.Length - 1 Step 2
        str = str + Chr("&H" & S_hex.SubString(i, 2))
    Next
    Return str
End Function


Public Class UFBInt 'Unfinished BigInt
    'base ： Long 型为 4 字节，32bit 为进制，其中 4bit 一个十六进制数
    Public Dim data() As New List(Of Long) '大整数的值
    
    '输入字串须为十六进制字串
    Public Sub New(ByRef str As String) '需要实现负数吗？ RSA 的话不需要

        '判断是否为负（不需要实现）

        Dim additional_zero As String
        For i As Integer = 0 To 7 - (str.Length % 8) '数长不为8的倍数，补足0
            additional_zero = additional_zero + "0"
        Next
        Dim tmp_str As String = str + additional_zero

        For i As Integer = 0 To tmp_str.Length - 1 Step 8 '8位一个base
            Dim base As Long= CLng( "&H" & tmp_str.SubString(i, 8))
            data.Add(base)
        Next

        
        Do While data(0) = 0 '去掉高位所有0
            data.RemoveAt(0)
        Loop

        data.Reverse '反转使高位在后

    End Sub

    Public Function add(ByRef val_a As UFBInt,_
                        ByRef val_b As UFBInt) As UFBInt

        Dim result As New UFBInt
        '忽略正负，负负相加情况

        Dim m_len As Integer = val_a.Count - val_b.Count '哪个数短给哪个数高位补0，方便等下逐位运算
        If m_len > 0 Then
            Do While m_len > 0
                m_len = m_len - 1
                val_b.Add(0)
            Loop
        End If
        If m_len < 0 Then
            Do While m_len < 0
                m_len = m_len + 1
                val_a.Add(0)
            Loop
        End If

        Dim carry As Integer = 0 '设置进位
        For i As Integer = 0 To val_a.Count - 1
            Dim sum As Long = val_a.data(i) + val_b.data(i) + carry
            Dim tmp As Long = "&H" & "10000"
            If sum >= tmp Then '进位了
                sum = sum Mod tmp
                carry = 1
            Else
                carry = 0
            End If
            result.data.Add(sum)
        Next

        If carry = 1 Then '加到最后还有一个进位
            result.data.Add(1)
        End If
        

    End Function

End Function

            
    