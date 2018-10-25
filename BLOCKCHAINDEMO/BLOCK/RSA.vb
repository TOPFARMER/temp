'字符串( ANSI 英文)转十六进制
Public Function str2hex(ByVal str As String) As String
    Dim tmp_hex As String
    For i As Integer = 0 To str.Length - 1
        tmp_hex = tmp_hex + Hex(Asc(str.Chars(i)))
    Next
    Return tmp_hex
End Function

'十六进制转字符串( ANSI 英文)
Public Function hex2str(ByVal S_hex As String) As String
    Dim str As String
    For i As Integer = 0 To S_hex.Length - 1 Step 2
        str = str + Chr("&H" & S_hex.Substring(i, 2))
    Next
    Return str
End Function


'加密解密内容
Public Class RSA
    Public mId As Integer
    Public public_key As PublicKey
    Private private_key As PrivateKey '尽量是私有，定义一个函数获取它

    Public Class PublicKey
        Public N As String
        Public e As String

        Public Sub New(ByRef mN As String,ByRef encrypt As String)
            N = mN
            e = encrypt
        End Sub
    End Class

    Public Class PrivateKey
        Public N As String
        Public d As String

        Public Sub New(ByRef mN As String,ByRef decrypt As String)
            N = mN
            d = decrypt
        End Sub
    End Class

    Public Sub New(ByVal id As Integer)
        Dim p As BigInt = createPrime(128, 20) ' 检验二十次，出错几率 (1/4)^20
        Dim q As BigInt = createPrime(128, 20) ' 检验二十次，出错几率 (1/4)^20
        Dim n As BigInt = p * (q) '计算出N
        Dim eul As BigInt = (p - (New BigInt("1"))) * (q - (New BigInt("1"))) '(p-1)*(q-1) 算出N的欧拉函数
        Dim e As BigInt = createOddNum(128) '设置encrypt指数
        Dim d As BigInt = e.modInverse(eul)
        Do While d.equals(0) = True OrElse e.toString.Length <> 32
            e = createOddNum(128)
            d = e.modInverse(eul)
        Loop

        public_key = New PublicKey(n.toString(), e.toString())
        private_key = New PrivateKey(n.toString(), d.toString())
        mId = id
    End Sub 

    '根据id获取私钥
    Public ReadOnly Property getPrivateKey(ByVal id As Integer) As PrivateKey
        Get
            If  id = mid Then
                Return private_key
            Else
                Return Nothing
            End If
        End Get
    End Property


    '英文字符串加密
    Public Shared Function encryptByPrivate(ByRef m As String,ByRef pri_key As PrivateKey) As String
        Dim msg As New BigInt(str2hex(m))
        Dim code As BigInt = msg.modPow(New BigInt(pri_key.d),New BigInt(pri_key.N))
        Return code.toString()
    End Function

    '解密出英文字符串
    Public Shared Function decryptByPublic(ByRef c As String,ByRef pub_key As PublicKey) As String
        Dim code As New BigInt(c)
        Dim msg As BigInt = code.modPow(New BigInt(pub_key.e),New BigInt(pub_key.N))
        Return hex2str(msg.toString())
    End Function


    '生成素数
    Public Shared Function createPrime(ByVal len As UInt32, ByVal k As UInt32)
        If len = 0 Then
            MessageBox.Show("长度不能为0！",  "Warning")
            Return Nothing
        End If
        Dim ans As BigInt = createOddNum(len)
        Do While isPrime(ans, k) = False  '素性检测
            ans = ans.add(New BigInt("2"))
        Loop
        Return ans
    End Function

    '判断是否为素数 （米勒拉宾检测） 失误率 (1/4)^k
    ' num 表示被测数， k 表示测试次数
    Public Shared Function isPrime(ByRef num As BigInt, ByVal k As UInt32) As Boolean
        If num.equals(0) Then
            MessageBox.Show("不能判断0！", "Warning")
            Return Nothing
        End If
        If num.equals(1) Then ' 1 不是素数
            Return False
        End If
        If num.equals(2) Then
            Return True
        End If

        Dim tmp As New BigInt(num)
        tmp.subtract(New BigInt("1"))
        Dim tmp_bit_len As Integer = tmp.getBitLen()
        If tmp.at(0) = 1 Then '减 1 后为奇数，则原数为偶数
            Return False
        End If

        Dim d As New BigInt(tmp) ' num-1 = 2^s*d
        Dim cnt_zero As UInt32 = 0 
        For i As UInt32 = 0 To tmp_bit_len - 1
            If tmp.at(i) = 0 Then
                cnt_zero = cnt_zero + 1
                d.shiftRightByBit(1) '算出 d
            Else
                Exit For
            End If
        Next

        For i As UInt32 = 1 To k
            Dim small As BigInt = createRandomSmaller(num) '生成一个[1, num - 1]之间的随机数a
            Dim x As BigInt = small.modPow(d, num)

            If x.equals(1) Then '可能为素数
                Continue For
            End If
            
            '测试所有 0<=j<s, a^(2^j*d) mod num != -1
            Dim ok As Boolean = True
            Dim j As UInt32 = 0
            Do While j < cnt_zero AndAlso ok = True 
                If x.compareTo(tmp) = 0 Then
                    ok = False '有一个相等，可能为素数
                End If
                x = x.multiply(x).modify(num)
                j = j + 1
            Loop
            If ok = True '存在不等的，肯定为合数
                Return False
            End If
        Next

        Return True '通过所有测试，可能为素数
    End Function

    '生成二进制长度为 len 的奇数
    Public Shared Function createOddNum(ByVal len As UInt32) As BigInt
        len = len >> 2
        If len > 1 Then
            Dim str As String = Rand.NextString(len - 1).ToUpper
            str = str & "1"
            Return New BigInt(str)
        Else
            Return New BigInt("F")
        End If
    End Function

    '随机生成比val小的数
    Public Shared Function createRandomSmaller(ByRef val As BigInt) As BigInt
        Dim tmp As New BigInt(Hex(Rand.Next(1, "&H" & "FFFFFFF")))
        Dim ans As BigInt

        ans = tmp.modify(val) '比 val 小
        If ans.equals(0) Then '必须非零
            ans = New BigInt(val)
            ans.subtract(New BigInt("1"))
        End If

        Return ans
    End Function
    
End Class

