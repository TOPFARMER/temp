'question1:问一下算法的过程
'question2:目前是否可以以单机多线程来模拟？
'question3:对于字符串流的共识，如何达到？
'question4:将军结点是叛徒的话，它本身需要达到共识吗？

'事件后填表
If e.DataCol.Name = "G" Then
    If e.NewValue = 1 Then
        Dim dr As DataRow
        dr = e.DataTable.DataRows(1)
        For Each dc As DataCol In e.DataTable.DataCols
            If dc.Name <> "G" Then
                dr(dc.Name) = 1
            End If
        Next
    End If
End If
            
'自定义类示例
Public Class XXX

    '计时器对象
    Private WithEvents _CtsTmr As System.Windows.Forms.Timer

    '要执行的自定义函数名称
    '用自定义函数,可以避免把类写死,增加扩展性
    Private _FunName As String

    '参数对象数组,
    '用对象数组,可以适用不同的场合,便于传递
    Private _obj() As Object

    ''' <summary>
    ''' 构造函数
    ''' </summary>
    ''' <param name="funName">要执行函数的名称</param>
    ''' <param name="obj">函数的参数</param>
    ''' <remarks></remarks>
    Sub New(ByVal funName As String, Optional ByVal obj() As Object = Nothing)
        _CtsTmr = New System.Windows.Forms.Timer '新建对象
        _CtsTmr.Interval = 500       '默认的间隔
        _FunName = funName '确定函数名称
        _obj = obj '确定参数集
    End Sub

    ''' <summary>
    ''' 计时器事件
    ''' </summary>
    ''' <param name="sender"></param>
    ''' <param name="e"></param>
    ''' <remarks></remarks>
    Private Sub CtsTmr_Tick(ByVal sender As Object, ByVal e As System.EventArgs) Handles _CtsTmr.Tick
        '打包自定义函数的参数
        '次序为:
        'sender
        'e
        '……
        Dim o() As Object = {sender, e}                 '固定的参数
        If _obj IsNot Nothing Then                      '如果还用参数
            Dim i As Integer = _obj.Length + 2          'o 是固定长度,就直接赋值了
            ReDim Preserve o(i - 1)                     '重新定义数组长度
            _obj.CopyTo(o, 2)                           '将参数数组复制到o数组
        End If
        '调用自定义函数,传递参数
        Functions.Execute(_FunName, o)         '调用自定义函数
    End Sub

    '返回计时器,用于各种设置
    Public ReadOnly Property CtsTmr As System.Windows.Forms.Timer
        Get
            Return _CtsTmr
        End Get
    End Property

End Class

'自定义函数内
Dim sender As Object = Args(0)  
Dim e As System.EventArgs = Args(1)  '前两个是默认的
Dim L As WinForm.Label = Args(2)    '第三个我们传递一个Label

L.Text = val(L.Text) + 1

'调用类示例
Dim a As XXX = Vars(e.Form.Name & "_Timer")

If  a IsNot Nothing Then
    a.CtsTmr.Start()
    
End If

Dim a As XXX = Vars(e.Form.Name & "_Timer")

If  a IsNot Nothing Then
    a.CtsTmr.Stop
    
End If


'结构体语句
Public Structure employee
    ' Public members, accessible from throughout declaration region.
    Public firstName As String
    Public middleName As String
    Public lastName As String
    ' Friend members, accessible from anywhere within the same assembly.
    Friend employeeNumber As Integer
    Friend workPhone As Long
    ' Private members, accessible only from within the structure itself.
    Private homePhone As Long
    Private level As Integer
    Private salary As Double
    Private bonus As Double
    ' Procedure member, which can access structure's private members.
    Friend Sub calculateBonus(ByVal rate As Single)
        bonus = salary * CDbl(rate)
    End Sub
    ' Property member to return employee's eligibility.
    Friend ReadOnly Property eligible() As Boolean
        Get
            Return level >= 25
        End Get
    End Property
    ' Event member, raised when business phone number has changed.
    Public Event changedWorkPhone(ByVal newPhone As Long)
End Structure


    Public ReadOnly Property GetDecisions As List(Of String)
        Get
            return decisions
        End Get
    End Property


'''
'''
'测试类代码
Dim b As New Node("1","0")
Dim a As New Traits(1,1,1,b)
Output.Show(a.mSource)

Dim m As New Dictionary(Of Integer, Dictionary(Of Integer, List(Of String)))
Dim a As New Dictionary(Of Integer, List(Of String))
Dim b As New List(Of String)
b.Add("mmm")
b.Add("mmm")
b.Add("mmm")
a.Add(0, b)
m.Add(0, a)
Output.Show(m(0)(0)(1))

If m.ContainsKey(0) = False Then '向量空间不存在的话需要新建一个
    Dim tmp1 As New Dictionary(Of Integer, List(Of String))
    m.Add(0, tmp1)
    Dim tmp2 As New List(Of String)
    m(0).Add(0, tmp2)
Else 
    If m(0).ContainsKey(0) = False Then
        Dim tmp2 As New List(Of String)
        m(0).Add(0, tmp2)
    End If
End If

Output.Show(m(0).ContainsKey(0))

Dim m As New Dictionary(Of Integer, Dictionary(Of Integer, List(Of String)))
Dim a As New Dictionary(Of Integer, List(Of String))
Dim b As New List(Of String)

If m.ContainsKey(0) = False Then '向量空间不存在的话需要新建一个
    Dim tmp1 As New Dictionary(Of Integer, List(Of String))
    m.Add(0, tmp1)
    Dim tmp2 As New List(Of String)
    m(0).Add(0, tmp2)
Else 
    If m(0).ContainsKey(0) = False Then
        Dim tmp2 As New List(Of String)
        m(0).Add(0, tmp2)
    End If
End If
Output.Show(m(0).ContainsKey(0))


Dim mChildren As New Dictionary(Of String, List(Of String)) 'map<CurrentNodePath, vector<NextNodePath>> 记录结点与其子节点的关系
Dim Shared mPathsByRank As New Dictionary(Of Integer, Dictionary(Of Integer, List(Of String))) 'map<Rank, map<SourceProcessId, vector<SendingNodePath>>> 记录每一层的所有结点

Dim tmp As New List(Of Boolean)
For i As Integer = 0 To N - 1
    tmp.Add(True)
Next
GenerateChildren(M, N, tmp, SOURCE, "", 0)


Public Sub GC(ByVal m As Integer, ByVal n As Integer, ByVal ids As List(Of Boolean), ByVal source As Integer, ByVal cur_path As String, ByVal rank As Integer)
        ids(source) = False
        cur_path += Cstr(source)
        If mPathsByRank.ContainsKey(rank) = False Then '向量空间不存在的话需要新建一个
            Dim tmp1 As New Dictionary(Of Integer, List(Of String))
            mPathsByRank.Add(rank, tmp1)
            Dim tmp2 As New List(Of String)
            mPathsByRank(rank).Add(source, tmp2)
        Else 
            If mPathsByRank(rank).ContainsKey(source) = False Then
                Dim tmp2 As New List(Of String)
                mPathsByRank(rank).Add(source, tmp2)
            End If
        End If
        mPathsByRank(rank)(source).Add(cur_path)
        If rank < m Then
            For i As Integer = 0 To ids.Count - 1
                If ids(i) = True Then
                    GenerateChildren(m, n, ids, i, cur_path, rank + 1)
                    If mChildren.ContainsKey(cur_path) = False Then
                        Dim tmp As List(Of String)
                        mChildren.Add(cur_path, tmp)
                    End If
                    mChildren(cur_path).Add(cur_path + Cstr(i))
                End If
            Next
        End If
    End Sub

    Dim tmp As New List(Of Boolean)
For i As Integer = 0 To 7
    tmp.Add(True)
Next
mPathsByRank

GC(2, 7, tmp, 1, "", 0)


Dim str As String = "你我"
Dim hex As String = ""
For i As Integer = 0 To str.Length - 1
    hex = hex + Asc(str.Chars(i))
Next

Dim str As String = "你我"

For i As Integer = 0 To str.Length - 1
    Output.Show(str.Chars(i))
Next



Output.Show(str)
Output.Show(hex)

Dim a As String = "1234"

Output.Show(dec2hex(a))
Output.Show(dec2hex1(a))
Output.Show(dec2hex2(a))


'接口必须重写
Public Functions 


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
Dim a As New BigInt("11111111111111111")
Dim b As New BigInt("11111111111111112")
Dim c As New BigInt("11111111111111111")
Output.Show(a.compareTo(b))

Output.Show(b.compareTo(a))
Output.Show(c.compareTo(a))

Dim str As String = "SAASSAASJNJJNJNJNJNJNAKNKSANK"
str = str2hex(str)
Dim a As New BigInt(str)
Dim b As Long = CLng( "&H" & 53414153)
Dim c As String = hex(b)

b = b And (-1 << 28)
b = b >> 28

Output.Show(a.data(a.data.Count - 1))
Output.Show(b)
Output.Show(c)

Output.Show(str)
Output.Show(hex2str(str))
Dim k As String = a.tostring()
Output.Show(k)
k = hex2str(k)
Output.Show(k)


Dim a As New BigInt("2")
Dim b As New BigInt("1")
Dim c As BigInt
c = a.modify(b)
Output.Show(c.Tostring())

Dim a As New BigInt("22222")
Dim b As New BigInt("22222222222222222222222222222")
Dim c As BigInt
Output.Show(a.bit_len)
a.at(0)


Dim a As New BigInt("1")
Output.Show(a.at(32))

Dim a As New BigInt("1")
Dim b As New BigInt("1111111111111111111111")
Dim c As BigInt
c = a.add(b)
Output.Show(c.Tostring())


Dim a As New BigInt("1")
Dim b As New BigInt("3")
Dim c As BigInt
c = b.abs()
Output.Show(a.compareTo(b))

Dim a As New BigInt("222222220")
Dim b As New BigInt("4")


Dim a As New BigInt("7")
Dim b As New BigInt("96")
Output.Show(a.modInverse(b).toString())

Dim a As New BigInt("0")
a = RSA.createOddNum(36)
Output.Show(a.Tostring())
a = RSA.createRandomSmaller(a)

Dim a As New BigInt("10000")
a.shiftrightbybit(10)
Output.Show(a.Tostring())

For i As Integer  = 1 To 100
    Dim a As New BigInt(hex(i))
    Output.Show(Cstr(RSA.Isprime(a, 500)) & Cstr(i))
Next

For i As Integer = 0 To 20
    Dim p As BigInt = RSA.createPrime(128, 20) ' 检验二十次,出错几率 (1/4)^20
    Dim q As BigInt = RSA.createPrime(128, 20) ' 检验二十次,出错几率 (1/4)^20
    Dim n As BigInt = p.multiply(q) '计算出N
    Dim eul As BigInt = (p - (New BigInt("1"))).multiply(q - (New BigInt("1"))) '(p-1)*(q-1) 算出N的欧拉函数
    Dim e As BigInt = RSA.createOddNum(128) '设置encrypt指数

    Dim d As BigInt = e.modInverse(eul)
    Do While d.equals(0) = True
        e = RSA.createOddNum(200)
        d = e.modInverse(eul)
    Loop

    Output.Show(e.Tostring)
    Output.Show(d.Tostring)
    Output.Show(n.Tostring)

    Dim msg As New BigInt(str2hex("Attack!"))
    Output.Show(msg.ToString)
    Dim code As BigInt = msg.modPow(New BigInt(d),New BigInt(n))
    Dim decode As BigInt = code.modPow(New BigInt(e),New BigInt(n))
    Output.Show(decode.ToString)
    Output.Show(hex2str(decode.ToString))

    Dim msg As New BigInt(str2hex("Attack!"))
    Output.Show(msg.ToString)
    Dim code As BigInt = msg.modPow(New BigInt(d.toString),New BigInt(n.toString))
    Dim decode As BigInt = code.modPow(New BigInt(e.toString),New BigInt(n.toString))
    Output.Show(decode.ToString)
    Output.Show(hex2str(decode.ToString))
Next

Dim p As New BigInt("7")
Dim q As New BigInt("60")
Output.Show(p.Modinverse(q).toString)


8C37DC337459434B9D68C2A1F6F29701
A9CA67BCF0580D5B7D6CA758D08077613746AE2A309D6DA410F232BEDA3C221
DCEA5DC2C6176DB25528A4AE4152985A78A73EE4575ADCABCEE209C48BE69499
Attack!
177E2F263B5147DEAA661963C4214091
1DB62115C773258993BA992392C2A2AB7E98BAFDC7A023092816C32DE732AD9
2AC277DF88E6A9F2849EB3A816ABB1C56D5F79150BC4DF6FE3488300BACE3
Attack!
686482167E894A328EE40862AED2EAD11
385694CB3D3822E12A2A51EE96247445DA562C9853BC3C6414AF1E8C1660E071
92A76BC91B6A8641A1D611758591F840AF99FB426E846289FEAAD9CA882E45
Attack!


    Dim p As BigInt = RSA.createPrime(128, 20) ' 检验二十次,出错几率 (1/4)^20
    Dim q As BigInt = RSA.createPrime(128, 20) ' 检验二十次,出错几率 (1/4)^20
    Dim n As BigInt = p.multiply(q) '计算出N
    Dim eul As BigInt = (p - (New BigInt("1"))).multiply(q - (New BigInt("1"))) '(p-1)*(q-1) 算出N的欧拉函数
    Dim e As BigInt = RSA.createOddNum(128) '设置encrypt指数

    Dim d As BigInt = e.modInverse(eul)
    Do While d.equals(0) = True
        e = RSA.createOddNum(200)
        d = e.modInverse(eul)
    Loop

    Output.Show(e.Tostring)
    Output.Show(d.Tostring)
    Output.Show(n.Tostring)


    Dim msg As New BigInt(str2hex("Attack!"))

    Dim code As BigInt = msg.modPow(New BigInt(d),New BigInt(n))
    Dim decode As BigInt = code.modPow(New BigInt(e),New BigInt(n))
    Dim d1 As New BigInt(d.toString)
    Dim n1 As New BigInt(n.toString)
    Dim e1 As New BigInt(e.toString)

    Output.Show(e1.Tostring)
    Output.Show(d1.Tostring)
    Output.Show(n1.Tostring)

    Output.Show(e.compareTo(e1))
    Output.Show(d.compareTo(d1))    
    Output.Show(n.compareTo(n1))

    code = msg.modPow(d1, n1)
    Output.Show(code.ToString)
    decode = code.modPow(e1, n1)   
    Output.Show(decode.ToString) 
    Output.Show(hex2str(decode.ToString))

Dim msg As New BigInt("B178CB96F306B94A0818034A0214025E1B90C369852684754A321B49DF81")
Output.Show(msg.toString)

Dim bId As Integer = 0
Dim prev_hash = "000000000000000000000000"
Dim msgs As New List(Of String)

Do While True
    Dim nonce As String = Rand.NextString(Rand.Next(1,8)).ToUpper '生成随机1到8位十六进制串
    Dim str As String = Block.GetHash(bId, nonce, msgs, prev_hash)
    If Left(str,3) = "000" Then
        Output.Show(nonce)
        Output.Show(str)

        Exit Do
    End If
Loop

Dim a As New List(Of String)
a.Add("sadasd")
a.Add("sadasd")
a.Add("sadasd")
a.Add("sadasd")
a.Add("sadasd")
Dim b As New Block(1, "2", a, "11", "222")
Output.Show(b.Msgs(0))



Dim bId As Integer = 0
Dim prev_hash = "000000000000000000000000"
Dim msgs As New List(Of String)

Do While True
    Dim nonce As String = Rand.NextString(Rand.Next(1,8)).ToUpper '生成随机1到8位十六进制串
    Dim str As String = Ledger.GetHash(bId, nonce, msgs, prev_hash)
    If Left(str,3) = "000" Then
        Output.Show(nonce)
        Output.Show(str)

        Exit Do
    End If
Loop
