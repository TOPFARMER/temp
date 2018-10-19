Public Class UFBInt 'Unfinished BigInt
    Public data As New List(Of Long) '大整数按 64bit 一个单元存储
    Public bit_len As Integer '大整数按 bit 计算共有多少位
    Public is_neg As Boolean = False
    Public Shared ZERO As New UFBInt("0")
    Public Shared ONE As New UFBInt("1")
    Public Shared TWO As New UFBInt("2")
    Public Shared TEN As New UFBInt("10")


    '输入字串须为十六进制字串
    Public Sub New(ByRef str As String) 
        Dim tmp_str As String
        If Left(str, 1) = "-" AndAlso str.Length > 0 Then
            If str.Length > 1 Then
                is_neg = True
            End If
            tmp_str = str.SubString(1)
        End If
        
        Dim additional_zero As String
        For i As Integer = 0 To 15 - (str.Length Mod 16) '数长不为16的倍数,补足0
            additional_zero = additional_zero + "0"
        Next
        tmp_str = additional_zero + str

        For i As Integer = 0 To tmp_str.Length - 1 Step 16 '16位一个base
            Dim base As Long= CLng( "&H" & tmp_str.SubString(i, 16))
            data.Add(base)
        Next

        data.Reverse '反转使高位在后
        trim()
        
        '大整数的二进制信息
        bit_len = (64 * data.Count) '粗略得出长度
        Dim tmp_top As Long = data(data.Count - 1)

        If tmp_top = 0 Then '细致调整长度
            bit_len = bit_len - 64
        Else
            Dim i As Long = (1 << 63) '从最高位开始按位与,若为 0 
            Do While (tmp_top And i) = False 
                bit_len = bit_len - 1 '长度减一
                i = i >> 1
            Loop
        End If
    End Sub

    Public Sub New(ByRef bigint As UFBInt) 
        data.AddRange(bigint.data)
        bit_len = bigint.bit_len
        is_neg = bigint.is_neg
    End Sub


    '去掉高位的0
    Public Sub trim()
        Do While data.Count > 1 AndAlso data(data.Count - 1) = 0 
            data.RemoveAt(data.Count - 1)
        Loop
    End Sub

    '查询大整数的第 id 位 bit 为 0 还是 1
    Public Function at(ByVal id As Integer) As Boolean '检测大数的第id个二进制位为1还是0
            Dim index As Integer = id / 64 '一个大整数位为 64bit = 2 ^ 6bit ,右移6位相当于除以 64 并取整
            Dim shift As Integer = id Mod 64 '即为 id & 0x002F 只取id的低5位, 相当于 id mod 32
            Dim tmp As Long = data(index)
            Return (tmp And (1 << shift))
    End Function

    '实现按二进制位左移
    Public Sub shiftLeftByBit(ByVal len As Integer)
        Dim index As Integer = len / 64
        Dim shift As Integer = len Mod 64
        For i As Integer = 0 To index -  1
            data.Insert(0, 0)
        Next
        If shift <> 0  Then
            data.Add(0) '加多一位
            Dim temp As Long = 0
            For i As Integer = 0 To  data.Count - 1
                Dim tmp As Long = data(i)
                data(i) = (tmp << shift) Or temp '整体左移后加上低一位大整数内的高位
                temp = (tmp And (-1 << (64 - shift))) >> (64 - shift) '获取该大整数内的高位
            Next
        End If
        trim()
    End Sub
    '实现按二进制位右移
    Public Sub shiftRightByBit(ByVal len As Integer)
        If len >= bit_len Then
            data.Clear
            data.add(0)
            bit_len = 0
        Else
            Dim index As Integer = len / 64
            Dim shift As Integer = len Mod 64
            For i As Integer = 0 To index -  1
                data.RemoveAt(0)
            Next
            If shift <> 0  Then
                Dim temp As Long = 0
                For i As Integer = data.Count - 1 To 0 Step -1
                    Dim tmp As Long = data(i)
                    data(i) = (tmp >> shift) Or temp '整体右移后加上低一位大整数内的高位
                    temp = (tmp And (-1 >> (64 - shift))) << (64 - shift) '获取该大整数内的低位
                Next
            End If
        End If
        trim()
    End Sub

    Public Function toString() As String
        Dim str As String
        data.Reverse
        For Each i As Long In data
            str = str & hex(i)
        Next
        data.Reverse
        Do While Left(str, 1) = "0" AndAlso str.Length > 0 '去掉高位没用的0
           str = str.TrimStart("0")
        Loop
        If str = Nothing Then
            str = "0"
        End If
        Return str
    End Function

    '返回大整数的绝对值
    Public Function abs() As UFBInt
        Dim ans As New UFBInt(toString())
        Return ans
    End Function

    '返回是否相等
    Public Function equals(ByRef val As UFBInt) As Boolean
        If is_neg = val.is_neg Then
            If data.Count = val.data.Count Then
                For i As Integer = 0 To data.Count
                    If data(i) <> val.data(i) Then
                        Return False
                    End If
                Next
                Return True
            End If
        Else 
            Return False
        End If
    End Function

    '实现比较大小, -1 本整数比较小、 0 表示相等、 1 表示本整数比较大
    Public Function compareTo(ByRef val As UFBInt) As Integer
        If is_neg <> val.is_neg Then '符号不同，负数必小
            If is_neg = False Then
                Return -1
            Else
                Return 1
            End If
        End If

        Dim flag As Integer = 0
        If bit_len < val.bit_len Then
            flag = -1
        ElseIf bit_len > val.bit_len Then
            flag = 1
        Else
            For i As Integer = data.Count - 1 To 0 Step -1
                If data(i) > val.data(i)
                    flag = 1
                    Exit For
                End If
                If data(i) < val.data(i)
                    flag = -1
                    Exit For
                End If
            Next
        End If
        If is_neg = False Then
            flag = flag * (-1)
        End If
        Return flag
    End Function

    '实现相加
    Public Function add(ByRef val As UFBInt) As UFBInt

        If is_neg = val.is_neg Then
            Dim carry As Integer = 0 '设置进位
            Dim m_len As Integer = data.Count - val.data.Count '哪个数短给哪个数高位补0,方便等下逐位运算
            If m_len >= 0 Then
                Dim addend As New UFBInt(val)
                Do While m_len > 0
                    m_len = m_len - 1
                    addend.data.Add(0)
                Loop
                For i As Integer = 0 To data.Count - 1
                    Dim sum As Long = addend.data(i) + data(i) + carry
                    If sum < data(i) Then '进位了
                        carry = 1
                    Else
                        If data(i) + addend.data(i) < data(i) Then
                            carry = 1
                        Else
                            carry = 0
                        End If
                    End If
                    data(i) = sum
                Next
            Else If m_len < 0 Then '为了节省运行内存，复制上面的代码
                Do While m_len < 0
                    m_len = m_len + 1
                    data.Add(0)
                Loop
                For i As Integer = 0 To data.Count - 1
                    Dim sum As Long = val.data(i) + data(i) + carry
                    If sum < data(i) Then '进位了
                        carry = 1
                    Else
                        If data(i) + val.data(i) < data(i) Then
                            carry = 1
                        Else
                            carry = 0
                        End If
                    End If
                    data(i) = sum
                Next
            End If

            If carry = 1 Then '加到最后还有一个进位
                data.Add(1)
            End If
        Else '异号情况暂不考虑
            Dim a As UFBInt = abs()
            Dim b As UFBInt = val.abs()
            Dim flag As Integer = a.compareTo(b)
            if flag = -1 Then
                Me = b.subtract(a)
            ElseIf flag = 0 Then
                Me = ZERO
            Else
                Me = a.subtract(b)
                
            End If 
        End If
        Return Me
    End Function



   ' Public Shared Operator +(ByVal f As Person, ByVal d As Int32) As Person
   '     f.b = f.b + d
   '     Return f
   ' End Operator

    '实现乘法
    Public Shared Function multiply(ByRef val_a As UFBInt, ByRef val_b As UFBInt) As UFBInt
        Dim result As UFBInt

        If val_a.data.Count = 1 Then '判断乘数是否为0(代码冗长暂不化简)
            If val_a.data(0) = 0 Then
                result.data.Add(0)
                Return result
            End If
        End If
        If val_b.data.Count = 1 Then
            If val_b.data(0) = 0 Then
                result.data.Add(0)
                Return result
            End If
        End If

        If val_a.data.Count >= val_b.data.Count Then
        End If
    End Function

End Class

'字符串( ANSI 英文)转十六进制
Public Function str2hex(ByVal str As String) As String
    Dim tmp_hex As String
    For i As Integer = 0 To str.Length - 1
        tmp_hex = tmp_hex + hex(Asc(str.Chars(i)))
    Next
    Return tmp_hex
End Function

'十六进制转字符串( ANSI 英文)
Public Function hex2str(ByVal S_hex As String) As String
    Dim str As String
    For i As Integer = 0 To S_hex.Length - 1 Step 2
        str = str + Chr("&H" & S_hex.SubString(i, 2))
    Next
    Return str
End Function

