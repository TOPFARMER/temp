'首先不能使用UINT64，因为VB计算溢出会报错
'VB的整型数字定义到数字除法会出现四舍五入，向下取整必须使用Int()函数

'   1.加减乘除四则运算
'   2.模运算
'   3.幂运算，幂模运算
'   4.拓展欧几里得算法求乘法逆元

Public Class BigInt 'Unfinished BigInt
    Public data As New List(Of UInt32) '大整数按 32bit 一个单元存储
    Public is_neg As Boolean = False
    Public Shared ZERO As UInt32 = 0
    Public Shared ONE As UInt32 = 1
    Public Shared NUM_ONE As UInt32 = 1
    Public Shared NUM_MINUSONE As UInt32 = "&H" & "FFFFFFFF"
    Public Shared BASE As Long = "&H" & "100000000"

    '输入字串须为十六进制字串
    Public Sub New(ByVal str As String)
        If Left(str, 1) = "-" AndAlso str.Length > 1 Then '若输入字串的最左端有 - 号 且长度大于 1 则为负数
            is_neg = True
            str = str.Substring(1)
        End If

        Dim additional_zero As String
        For i As Integer = 0 To 7 - (str.Length Mod 8) '数长不为8的倍数,补足0
            additional_zero = additional_zero + "0"
        Next
        str = additional_zero + str

        For i As Integer = 0 To str.Length - 1 Step 8 '8位一个base
            Dim base As UInt32 = CLng("&H" & str.Substring(i, 8))
            data.Add(base)
        Next

        data.Reverse() '反转使高位在后
        trim()

    End Sub

    Public Sub New(ByRef bigint As BigInt)
        data.AddRange(bigint.data)
        is_neg = bigint.is_neg
    End Sub

    Public Sub New(ByRef val As UInt32)
        data.Add(val)
        is_neg = False
    End Sub

    '大整数的二进制信息
    Public Function getBitLen() As Integer
        Dim bit_len As Integer
        bit_len = (32 * data.Count) '粗略得出长度
        Dim tmp_top As UInt32 = data(data.Count - 1)

        If tmp_top = 0 Then '细致调整长度
            bit_len = bit_len - 32 '若数值为 0，二进制位数也为 0
        Else
            For i As Integer = 31 To 0 Step -1 
                If (NUM_ONE << i) And tmp_top Then '按位与从高位查看是否为 0
                    Exit For
                Else
                    bit_len = bit_len - 1 '有一位为 0 即长度减一
                End If
            Next
        End If
        Return bit_len
    End Function

    '复制大数的值
    Public Sub copyVal(ByRef bigint As BigInt)
        data.Clear()
        data.AddRange(bigint.data)
    End Sub

    '去掉大数高位的0
    Public Sub trim()
        Do While data.Count > 1 AndAlso data(data.Count - 1) = 0
            data.RemoveAt(data.Count - 1)
        Loop
    End Sub

    '查询大整数的第 id 位 bit 为 0 还是 1
    Public Function at(ByVal id As Integer) As Boolean '检测大数的第id个二进制位为1还是0
        Dim index As Integer = id >> 5 '一个大整数位为 32bit = 2 ^ 5bit ,右移 5 位相当于除以 32 并取整
        Dim shift As Integer = id Mod 32 '即为 id & 0x002F 只取id的低5位 
        Dim tmp As UInt32 = data(index)
        Return (tmp And (NUM_ONE << shift))
    End Function

    '实现按二进制位左移
    '原理:
    '                高位->
    '                     [  Base1  ]-[  Base2  ]-[  Base3  ]-[  Base4  ]-[  Base5  ]-[  Base6  ]
    '   1. 整位左移<-
    '         [  Base1  ]-[  Base2  ]-[  Base3  ]-[  Base4  ]-[  Base5  ]-[  Base6  ]-[  00000  ]
    '
    '   2. 非整位左移<-
    '         HB: Base的高位  LB: Base的低位
    '         [ 0000HB1 ]-[ LB1&HB2 ]-[ LB2&HB3 ]-[ LB3&HB4 ]-[ LB4&HB5 ]-[ LB5&HB6 ]-[ LB60000 ]
    '
    Public Sub shiftLeftByBit(ByVal len As Integer)
        Dim index As Integer = len >> 5
        Dim shift As Integer = len Mod 32
        For i As Integer = 0 To index - 1
            data.Insert(0, 0)
        Next
        If shift <> 0 Then '如果左移位数不为 32 的整数倍
            data.Add(0) '加多一位
            Dim temp As UInt32 = 0
            For i As Integer = 0 To data.Count - 1
                Dim tmp As UInt32 = data(i)
                data(i) = (tmp << shift) Or temp '整体左移后加上低一位大整数内的高位
                temp = (tmp And (NUM_MINUSONE << (32 - shift))) >> (32 - shift) '获取该大整数内的高位
            Next
        End If
        trim()
    End Sub

    '实现按二进制位右移
    Public Sub shiftRightByBit(ByVal len As Integer)
        Dim bit_len As Integer = getBitLen()
        If len >= bit_len Then '若右移长度大于整个大数的二进制长度，直接赋 0 
            data.Clear()   
            data.Add(0)
        Else
            Dim index As Integer = len >> 5
            Dim shift As Integer = len Mod 32
            For i As Integer = 0 To index - 1
                data.RemoveAt(0)
            Next
            If shift <> 0 Then '若右移长度不为 32 的整数倍
                Dim temp As UInt32 = 0
                For i As Integer = data.Count - 1 To 0 Step -1
                    Dim tmp As UInt32 = data(i)
                    data(i) = (tmp >> shift) Or temp '整体右移后加上低一位大整数内的高位
                    temp = (tmp And (NUM_MINUSONE >> (32 - shift))) << (32 - shift) '获取该大整数内的低位
                Next
            End If
        End If
        trim()
    End Sub

    '将大数转换为字串输出
    Public Function toString() As String
        Dim str As String

        data.Reverse() '将高位转到前头，方便后续操作
        For Each i As UInt32 In data
            If i <> 0 Then
                Dim tmp_hex As String = CStr(Hex(i)) '取一个 base 内的数进行十六进制串转换
                For j As Integer = 0 To 7 - CStr(Hex(i)).Length '若转换的十六进制串不满 8 位，在前面补 0 
                    tmp_hex = "0" + tmp_hex
                Next
                str = str & tmp_hex
            Else
                str = str + "00000000" '若取的是 0 则输出 8 个 0
            End If
        Next
        data.Reverse() '将高位移回后头

        Do While Left(str, 1) = "0" AndAlso str.Length > 0 '去掉高位没用的0
            str = str.TrimStart("0")
        Loop
        If str = Nothing Then '若字串为空，输出 0 
            str = "0"
        Else
            If is_neg = True Then
                str = "-" & str
            End If
        End If
        Return str
    End Function

    '返回大整数的绝对值
    Public Function abs() As BigInt
        Dim ans As New BigInt("0")
        ans.data.Clear()
        ans.data.AddRange(data)
        Return ans
    End Function

    '返回是否相等
    Public Function equals(ByVal val As UInt32) As Boolean
        Dim tmp As New BigInt(val)
        If data.Count = tmp.data.Count Then '若大数位数相等
            For i As Integer = 0 To data.Count - 1
                If data(i) <> tmp.data(i) Then '每一个 base 都进行对比
                    Return False
                End If
            Next
            Return True
        Else
            Return False
        End If
    End Function

    '实现比较大小, -1 本整数比较小、 0 表示相等、 1 表示本整数比较大
    Public Function compareTo(ByRef val As BigInt) As Integer
        If is_neg <> val.is_neg Then '符号不同，负数必小
            If is_neg = True Then
                Return -1
            Else
                Return 1
            End If
        End If

        Dim flag As Integer = 0
        If data.Count < val.data.Count Then '谁的大数位更长谁更大
            flag = -1
        ElseIf data.Count > val.data.Count Then
            flag = 1
        Else
            For i As Integer = data.Count - 1 To 0 Step -1 '位数一致，从高位开始对比
                If data(i) > val.data(i) Then
                    flag = 1
                    Exit For
                End If
                If data(i) < val.data(i) Then
                    flag = -1
                    Exit For
                End If
            Next
        End If
        If is_neg = True Then '若为负数则以上结果均相反
            flag = flag * (-1)
        End If
        Return flag
    End Function

    '实现相加，加法常用，被加数直接被更改以节约内存
    '举例说明：
    ' 1234567
    ' 0001234 +
    '----------
    ' 1235801
    Public Function add(ByRef val As BigInt) As BigInt
        If is_neg = val.is_neg Then '若两加数同号
            Dim carry As Integer = 0 '设置进位
            Dim tmp_sum As Long '每一位的临时值 
            Dim a As Long '每一位的临时值 
            Dim b As Long '每一位的临时值 
            Dim m_len As Integer = data.Count - val.data.Count '哪个数短给哪个数高位补 0 ,方便等下逐位运算
            If m_len > 0 Then '加数较短
                Dim addend As New BigInt(val)
                Do While m_len > 0 '高位补 0 ，对齐两者位数
                    m_len = m_len - 1
                    addend.data.Add(0)
                Loop
                For i As Integer = 0 To data.Count - 1
                    a = addend.data(i)
                    b = data(i)
                    tmp_sum = a + b + carry
                    carry = Int(tmp_sum / BASE) '判断加和是否产生进位
                    tmp_sum = tmp_sum Mod BASE '去掉向上溢出部分
                    data(i) = tmp_sum
                Next
            Else '被加数较短               为了节约思考时间，复制上面的代码块
                Do While m_len < 0
                    m_len = m_len + 1
                    data.Add(0)
                Loop
                For i As Integer = 0 To data.Count - 1
                    a = val.data(i)
                    b = data(i)
                    tmp_sum = a + b + carry
                    carry = Int(tmp_sum / BASE)
                    tmp_sum = tmp_sum Mod BASE
                    data(i) = tmp_sum
                Next
            End If

            If carry = 1 Then '加到最后还有一个进位
                data.Add(1)
            End If
        Else '两加数异号情况
            Dim a As BigInt = abs()
            Dim b As BigInt = val.abs()
            Dim flag As Integer = a.compareTo(b)
            If flag = -1 Then '绝对值相等结果为 0 ，否则用绝对值大的减去小的， 符号随绝对值大的
                copyVal(b.subtract(a))
                is_neg = val.is_neg
            ElseIf flag = 0 Then
                data.Clear()
                data.Add(0)
                is_neg = False
            Else
                copyVal(a.subtract(b))
            End If
        End If
        Return Me
    End Function


    '实现减法，减法不太常用，可以实现得浪费内存一些
    '举例说明：
    '12345678
    '00005678 -
    '----------
    '12340000
    Public Function subtract(ByRef val As BigInt) As BigInt 
        Dim a As BigInt = abs() '被减数的绝对值赋与 a
        Dim b As BigInt = val.abs() '减数的绝对值赋与 b
        If is_neg = val.is_neg Then '如果同号
            Dim flag As Integer = a.compareTo(b)
            If flag = 1 Then ' a 的绝对值大于 b 的绝对值，直接减
                Dim tmp_sum As Long '每一位的临时值
                Dim c As Long '每一位的临时值
                Dim d As Long '每一位的临时值
                Dim borrow As Integer = 0 '生成借位
                Dim attached_cnt As Integer = data.Count - val.data.Count '得出两数的位数差
                For i As Integer = 0 To attached_cnt - 1 '谁短给谁补 0
                    b.data.Add(0)
                Next
                For i As Integer = 0 To data.Count - 1
                    c = data(i)
                    d = b.data(i)
                    tmp_sum = c - d - borrow
                    If tmp_sum < 0 Then '判断做差是否产生借位
                        borrow = 1
                    Else
                        borrow = 0
                    End If
                    tmp_sum = (tmp_sum + BASE) Mod BASE '去掉向下溢出部分
                    data(i) = tmp_sum
                Next
                trim()
            ElseIf flag = 0 Then 'a 的绝对值等于 b 的绝对值
                data.Clear()
                data.Add(0)
                is_neg = False
            Else ' a 的绝对值小于 b 的绝对值，反过来 b - a
                copyVal(b.subtract(a))
                is_neg = Not (is_neg)
            End If
        Else '如果两数异号则为加法
            copyVal(a.add(b))
        End If
        Return Me
    End Function

    '实现乘法
    '举例说明：
    '被乘数为 123 二进制数即为 0111 1011，乘数为 3 二进制数即为 11
    '       123        ->    0111 1011      
    '         3(O) x   ->           11(b) x 
    '-------------     ->   -------------   
    '                        0111 1011 (x 2 ^ 0 即左移 0 位)
    '                        1111 0110 (x 2 ^ 1 即左移 1 位)
    '                       -------------
    '       369        <-   10111 0001

    Public Function multiply(ByRef val As BigInt) As BigInt
        If equals(ZERO) OrElse val.equals(ZERO) Then '有 0 参与运算，返回 0 
            Return New BigInt("0")
        End If

        Dim ans As New BigInt("0")
        If data.Count >= val.data.Count Then '位数少的做乘数
            Dim tmp As New BigInt(Me) '被乘数
            tmp.is_neg = False
            Dim last_shift_pos As Integer = 0
            Dim bit_len As Integer = val.getBitLen() '获取乘数的二进制位
            For i As Integer = 0 To bit_len - 1 '被乘数按乘数的第 i 个二进制位是否为 1 进行左移 i 位
                If val.at(i) = True Then 
                    tmp.shiftLeftByBit(i - last_shift_pos)
                    last_shift_pos = i
                    ans = ans.add(tmp)
                End If
            Next
        Else
            Dim tmp As New BigInt(val) '被乘数
            tmp.is_neg = False
            Dim last_shift_pos As Integer = 0
            Dim bit_len As Integer = getBitLen() '获取乘数的二进制位
            For i As Integer = 0 To bit_len - 1 '被乘数按乘数的第 i 个二进制位是否为 1 进行左移 i 位
                If at(i) = True Then
                    tmp.shiftLeftByBit(i - last_shift_pos)
                    last_shift_pos = i
                    ans = ans.add(tmp)
                End If
            Next
        End If
        If is_neg <> val.is_neg Then '两数异号，结果为负
            ans.is_neg = True
        End If
        Return ans
    End Function


    '实现除法
    '举例说明：
    '被除数为 123 二进制数即为 0111 1011，除数为 13 二进制数即为 1101
    ' round 1:                   round 2:                         round 3:
    '          1                       100 1                        100 1     ----> 9
    '         ----------  ->          ----------   ->              ----------
    '    1101|0111 1011   ->     1101|0111 1011    ->         1101|0111 1011 
    '          110 1000   ->           110 1000    ->               110 1000 
    '         ----------  ->          ----------   ->              ----------
    '         0001 0011   ->          0001 0011    ->              0001 0011 
    '                                      1101                         1101 
    '                                 ----------                   ----------
    '                                 0000 0110                    0000 0110  ----> 6 (被除数经过竖式减法后剩余 6 作为余数)
    
    Public Function divideAndReminder(ByVal val As BigInt, ByRef m As BigInt) As BigInt
        If val.equals(ZERO) Then
            MessageBox.Show("除数不能为0！", "Warning")
            Return Nothing
        End If

        '取参与运算的两个数的绝对值
        Dim a As BigInt = abs()
        Dim b As BigInt = val.abs()
        Dim flag As Integer = a.compareTo(b)
        If flag = 0 Then '若两数的绝对值相等
            If is_neg = val.is_neg Then '按两数的符号判断结果为 1 还是 -1
                Return New BigInt("1")
            Else
                Return New BigInt("-1")
            End If
        End If
        If flag = -1 Then '若被除数小于除数，则结果为 0 ，将被除数赋与余数
            m = Me
            Return New BigInt("0")
        End If

        Dim ans As New BigInt("0")
        Do While True '若被除数大于除数
            Dim len As Integer = a.getBitLen() - b.getBitLen()
            Dim tmp As BigInt

            '粗略地得出倍数
            Do While len >= 0
                tmp = New BigInt(b)
                tmp.shiftLeftByBit(len)
                If tmp.compareTo(a) <> 1 Then '从 len 位开始减小，按二进制位左移除数，试探出使得除数刚好小于等于被除数所需要左移的二进制位数
                    Exit Do
                End If
                len = len - 1
            Loop
            If len < 0 Then '当前的被除数小于除数了
                Exit Do
            End If
            Dim num As UInt32 = 0

            '细致地得出倍数
            Do While tmp.compareTo(a) <> 1 '当被除数仍比左移后的除数要大
                a.subtract(tmp) '被除数减去左移后的除数
                num = num + 1 '记录被除数是左移后的除数的多少倍
            Loop


            tmp = New BigInt(num)
            If len <> 0 Then
                tmp.shiftLeftByBit(len) '两个倍数相乘
            End If

            ans.add(tmp) '将本轮倍数加入结果，此时被除数变小了，继续下一轮的倍数计算
        Loop

        If is_neg = val.is_neg Then
            ans.is_neg = False
        Else
            ans.is_neg = True
        End If

        m.data.Clear()
        m.data.AddRange(a.data) '经过多轮的计算后，被除数变为比除数更小的数，直接赋与余数
        m.is_neg = is_neg
        Return ans
    End Function

    '除法
    Public Function divide(ByRef val As BigInt) As BigInt
        Dim tmp As New BigInt("0")
        Dim ans As BigInt = divideAndReminder(val, tmp)
        Return ans
    End Function

    '取余
    Public Function remainder(ByRef val As BigInt) As BigInt
        Dim ans As New BigInt("0")
        divideAndReminder(val, ans)
        Return ans
    End Function

    '取模 结果必为正数
    Public Function modify(ByRef m As BigInt) As BigInt
        Dim ans As BigInt = remainder(m)
        If ans.is_neg = True Then '若余数为负数
            ans = ans.add(m) '则模运算结果为该负值余数加上模数 举例说明： -13 / 5 = -2 …… -3 即 -13 % 5 = (-3 + 5) = 2
        End If
        Return ans
    End Function

    'https://zh.wikipedia.org/wiki/%E5%B9%B3%E6%96%B9%E6%B1%82%E5%B9%82
    '幂乘 快速幂 平方求幂 
    '从高位开始计算，在往低位计算时发生位权累加效应
    '举例说明：计算 x 的 13次幂， 指数为 13， 二进制为 1101, 结果为 ans
    ' x ^ 13 = x ^ (2 ^ 3) * x ^ (2 ^ 2) * x ^ (2 ^ 0)
    '算法实现：
    '0. ans = 1
    '1. ans = ans ^ 2 ; (1)101 -> ans = ans * x;  (此时 ans = x ^ 1) 
    '2. ans = ans ^ 2 ; 1(1)01 -> ans = ans * x;  (此时 ans = x ^ 3) 
    '3. ans = ans ^ 2 ; 11(0)1 -> do nothing!;
    '4. ans = ans ^ 2 ; 110(1) -> ans = ans * x;  (此时 ans = x ^ 13) 
    
    Public Function pow(ByRef exponent As BigInt) As BigInt
        Dim ans As New BigInt("1")
        Dim bit_len = exponent.getBitLen()
        For i As Integer = bit_len - 1 To 0 Step -1
            ans = ans.multiply(ans)
            If exponent.at(i) Then 
                ans = multiply(ans) '从高位开始，位权累加效应
            End If
        Next
        Return ans
    End Function

    '幂模 快速幂结合模运算性质
    '模乘性质：(a * b) % p = ((a % p) * (b % p)) % p
    '举例说明：计算 x 的 13次幂模 2，模数为 p， 指数为 13， 二进制为 1101, 结果为 ans
    ' (x ^ 13) % p = (((x ^ (2 ^ 3)) % p) * ((x ^ (2 ^ 2)) % p) * ((x ^ (2 ^ 0)) % p)) % p
    '算法实现：
    '0. ans = 1
    '1. ans = (ans ^ 2) % p ; (1)101 -> ans = (ans * x) % p;  (此时 ans = (x ^ 1) % p) 
    '2. ans = (ans ^ 2) % p ; 1(1)01 -> ans = (ans * x) % p;  (此时 ans = (x ^ 3) % p) 
    '3. ans = (ans ^ 2) % p ; 11(0)1 -> do nothing!;
    '4. ans = (ans ^ 2) % p ; 110(1) -> ans = (ans * x) % p;  (此时 ans = (x ^ 13) % p) 

    Public Function modPow(ByRef exponent As BigInt, ByRef m As BigInt) As BigInt
        If m.equals(ZERO) Then
            MessageBox.Show("模数不能为0！", "Warning")
            Return Nothing
        End If
        Dim ans As New BigInt("1")
        Dim bit_len = exponent.getBitLen()
        For i As Integer = bit_len - 1 To 0 Step -1
            ans = ans.multiply(ans).modify(m)
            If exponent.at(i) Then '快速幂
                ans = multiply(ans).modify(m) '从高位开始，位权累加效应
            End If
        Next
        Return ans
    End Function

    'https://zh.wikipedia.org/wiki/%E6%89%A9%E5%B1%95%E6%AC%A7%E5%87%A0%E9%87%8C%E5%BE%97%E7%AE%97%E6%B3%95
    'EXGCD 拓展欧几里得算法 
    '   可以在得出 a 与 b 的最大公约数的同时，得出满足 ax + by = gcd(a, b) 的 x, y ， 其中 gcd(a, b) 为 a, b 的最大公约数。
    '为什么需要它？
    '   它是为RSA算法服务的：在生成密钥时 ed mod eul = 1，此时已随机得出 e 为某确定大奇数，求d。故算称为 求模逆(modInverse)。
    '   而 ed mod eul = 1 -> ed + k * eul  = 1 -> ed + k * eul = gcd(e, eul) -> EXGCD -> 得到 d 与 k
    '原理及举例说明:
    '   只是一个线性方程组的连续初等变换。
    '   现有 e = 3 ，eul = 3016 , 求 模逆元 d 与 常数 k
    '由此可得出矩阵        初等变换                     初等变换
    '   | 3016     3  |     ->       |   1      3  |    ->     |   1      1  |  
    '   |             |     ->       |             |    ->     |             |  
    '   |  1       0  |     ->       |   1      0  |    ->     |   1     -2  |  -> k
    '   |             |     ->       |             |    ->     |             |  
    '   |  0       1  |     ->       | -1005    1  |    ->     | -1005  2011 |  -> d
    '
    Public Function modInverse(ByRef m As BigInt) As BigInt
        If is_neg = True OrElse m.is_neg = True Then

            Return Nothing
        End If

        If equals(ZERO) OrElse m.equals(ZERO) Then
            Return New BigInt("0")
        End If

        '定义线性方程组
        Dim a As New List(Of BigInt)
        Dim b As New List(Of BigInt)
        Dim tmp As New List(Of BigInt)
        a.Add(New BigInt("0"))
        a.Add(New BigInt("1"))
        a.Add(New BigInt(Me))
        b.Add(New BigInt("1"))
        b.Add(New BigInt("0"))
        b.Add(New BigInt(m))
        tmp.Add(New BigInt("0"))
        tmp.Add(New BigInt("0"))
        tmp.Add(New BigInt("0"))

        '开始线性变换
        tmp(2) = a(2).modify(b(2))
        Do While True
            If tmp(2).equals(ZERO) = True Then
                Exit Do
            End If
            Dim temp As BigInt = a(2).divide(b(2))
            For i As Integer = 0 To 2
                tmp(i) = a(i)
                tmp(i).subtract(temp.multiply(b(i)))
                a(i) = b(i)
                b(i) = tmp(i)
            Next
            tmp(2) = a(2).modify(b(2))
        Loop

        If b(2).equals(ONE) Then
            If b(1).is_neg Then '模逆元不会是负数
                b(1).add(m) '则模逆元结果为该负值加上模数
            End If
            Return b(1)
        End If
        Return New BigInt("0")
    End Function

    '''
    ''
    '重载运算
    Public Shared Operator +(ByVal a As BigInt, ByVal b As BigInt) As BigInt
        Return a.add(b)
    End Operator

    Public Shared Operator -(ByVal a As BigInt, ByVal b As BigInt) As BigInt
        Return a.subtract(b)
    End Operator

    Public Shared Operator *(ByVal a As BigInt, ByVal b As BigInt) As BigInt
        Return a.multiply(b)
    End Operator

    Public Shared Operator /(ByVal a As BigInt, ByVal b As BigInt) As BigInt
        Return a.divide(b)
    End Operator

    Public Shared Operator Mod(ByVal a As BigInt, ByVal b As BigInt) As BigInt
        Return a.modify(b)
    End Operator

End Class
