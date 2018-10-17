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
    Public data() As New List(Of Long) '大整数按 32bit 一个单元存储
    Public bit_len As Integer '大整数按 bit 计算共有多少位
    'Public is_neg As Boolean

    '输入字串须为十六进制字串
    Public Sub New(ByRef str As String) '需要实现负数吗？ RSA 的话不需要
        '是否为负（不需要实现）
        
        Dim additional_zero As String
        For i As Integer = 0 To 7 - (str.Length % 8) '数长不为8的倍数，补足0
            additional_zero = additional_zero + "0"
        Next
        Dim tmp_str As String = str + additional_zero

        For i As Integer = 0 To tmp_str.Length - 1 Step 8 '8位一个base
            Dim base As Long= CLng( "&H" & tmp_str.SubString(i, 8))
            data.Add(base)
        Next

        data.Reverse '反转使高位在后
        trim() '去掉高位0
        
        '获取大整数的二进制信息
        bit_len = (32 * data.Count) '粗略得出长度
        Dim tmp_top As Long = data(data.Count - 1)

        If tmp_top = 0 Then '细致调整长度
            bit_len = bit_len - 32
        Else
            Dim i As Long = 1 << 31 '从最高位开始按位与，若为 0 
            Do While (tmp_top And i) = False 
                bit_len = bit_len - 1 '长度减一
                i = i >> 1
            Loop
        End If
    End Sub

    '查询大整数的第 id 位 bit 为 0 还是 1
    Public Function at(ByVal id As Integer) As Boolean '检测大数的第id个二进制位为1还是0
            Dim index As Integer = id >> 5 '一个大整数位为 32bit = 2 ^ 5bit ，右移5位相当于除以 32 并取整
            Dim shift As Integer = id And 31 '即为 id & 0x1F 只取id的低5位， 相当于 id mod 32
            Dim tmp As Long = data(index)
            Return (tmp And (1 << shift))
    End Function

    '去掉高位的0
    Public Sub trim()
        Do While data.Count > 1 AndAlso data(data.Count - 1) = 0 
            data.RemoveAt(data.Count - 1)
        Loop
    End Sub
    

    '实现两个正数相加
    Public Function add(ByRef val As UFBInt) As UFBInt
        '忽略正负，负负相加情况

        Dim carry As Integer = 0 '设置进位
        Dim tmp As Long = "&H" & "10000" '进位判断辅助 Hx10000
        Dim m_len As Integer = val.data.Count - data.Count '哪个数短给哪个数高位补0，方便等下逐位运算
        If m_len > 0 Then
            Do While m_len > 0
                m_len = m_len - 1
                data.Add(0)
            Loop
        Else If m_len < 0 Then
            Do While m_len < 0
                m_len = m_len + 1
                val.Add(0)
            Loop
        End If

        For i As Integer = 0 To data.Count - 1
            Dim sum As Long = val.data(i) + data(i) + carry
            If sum >= tmp Then '进位了
                sum = sum Mod tmp
                carry = 1
            Else
                carry = 0
            End If
            data.Add(sum)
        Next

        val.trim() '若为val的高位添加0，则去掉

        If carry = 1 Then '加到最后还有一个进位
            data.Add(1)
        End If
        
        Return Me
    End Function

    '减法不实现

    '实现乘法
    Public Function multiply(ByRef val As UFBInt) As UFBInt
        If data.Count = 1 Then '判断乘数是否为0
            If data(0) = 0 Then
                Return Me
            End If
        End If
        If val.data.Count = 1 Then
            If val.data(0) = 0 Then
                data.Clear
                data.Add(0)
                Return Me
            End If
        End If

        If val.data.Length >= data.Length Then
            

    End Function
End Function

            
    