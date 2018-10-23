'主机数超出10台后，源信息主机是坏结点，整个网络达成无意义共识
'使用1号作为坏主机没有产生错误
'估计是生成的决策树有问题

'全局定义

Public UNKNOWN As String = "?" '坏主机决策出信息


Public Sub SimBlockChain(Optional ByVal source As Integer = 0,Optional ByVal m As Integer = 0,Optional ByVal n As Integer = 5,Optional ByVal srcval As String = "Attack!") 
    Dim ledgers As New List(Of Ledger)
    If Ledger.CheckRSABank = 0 Then
        Ledger.SetTheRSABank(n) '初始化每个进程的RSA信息
    End If

    If ledgers.Count = 0 Then '若每个进程的账单类未创建，创建它
        For i As Integer = 0 To n - 1
            Dim tmp As New Ledger(i)
            ledgers.Add(tmp)
        Next
    End If

    Dim srcval_msg As Message '定义源信息结点
    srcval_msg = ledgers(source).SignedSrcMsg(srcval) '将源信息转换为结点
    Dim traits As New Traits(source, m, n, srcval_msg) '定义拓扑特征
    Dim decisions As List(Of String) = SimBFT(traits) '使用BFT算法，在网络中传信，得出每个主机的决策

    For Each i As String In decisions
        Output.Show(i)
    Next
    
End Sub



'账单类，负责对信息处理
Public Class Ledger
    Private Shared mRSABank As New List(Of RSA) 'vector<RSA> 用于存储生成的RSA密钥
    Public mId As Integer

    Private blockchain As New List(Of Block) '存放区块链
    Private msg_buff As New List(Of String) '存放5条信息

    Public Sub New(ByVal id As Integer)
        mId = id
    End Sub

    Public Sub GetAndCheckMessage(ByVal source As Integer, ByVal code As String) As Boolean
        If code = "Retreat!" OrElse code = "?" Then
            Return False
        End If

        Dim msg As String 
        If code.split("|")(0) = "IsMsg" Then ' 若信息为纯信息
            msg = RSA.decryptByPublic(tmp_msg(1) ,mRSABank(source).public_key) '对密文使用公钥解密
            If msg.split("|")(1) = "mId" & CStr(source) Then '校对确认内部信息有意义，密文信息加到缓冲区
                msg_buff.Add(tmp_msg(1) & "|mId" & CStr()) ' 加密部分[内容|mIdX]+[|mIdX]可见部分 
            End If
        End If
        If  code.split("|")(0) = "IsBlk" Then '若信息为区块
            msg = RSA.decryptByPublic(tmp_msg(1) ,mRSABank(source).public_key) '对密文使用公钥解密
            If msg.split("|")(1) = "mId" & CStr(source) Then '校对确认内部信息有意义，对区块进行组装并检查
                Dim blk As Block = CheckBlock(msg.split("|")(0))
            End If
        End If
    End Function


    Public Function CheckBlock(ByVal blk As String) As Block
    'todo检查block
    End Function

    ReadOnly Shared Property CheckRSABank() As Integer
        Get
            Return mRSABank.Count
        End Get
    End Property

    '清理原有的RSA密钥信息
    Public Shared Sub CleanTheRSABank() 
        mRSABank.Clear
    End Sub

    '初始化所有进程的RSA密钥信息
    Public Shared Sub SetTheRSABank(ByVal process_num As Integer)
        For id As Integer = 0 To process_num - 1
            Dim tmp As New RSA(id)
            mRSABank.Add(tmp)
        Next
    End Sub

    '对源信息签名
    Public Function SignedSrcMsg(ByVal input As Object) As Message
        Dim tmp As String = CStr(input) & "|mId" & CStr(mId) '为信息添加校验信息 信息内容 |mIdX
        Dim code As String = "IsMsg|" & RSA.encryptByPrivate(tmp ,pri_key) '返回签名信息 加入 IsMsg 标识来区分信息 可见部分[IsMsg|]+[内容|mIdX]加密部分
        Dim pri_key As RSA.PrivateKey = mRSABank(mId).getPrivateKey(mId) '获取本机私钥
        Return New Message(code, UNKNOWN) 
    End Function

    '对源区块信息签名
    Public Function SignedBlock(ByVal input As Object) As Message
        Dim tmp As String = CStr(input) & "|mId" & CStr(mId) '为信息添加校验信息 信息内容 |mIdX
        Dim code As String = "IsBlk|" & RSA.encryptByPrivate(tmp ,pri_key) '返回签名信息 加入 IsBlk 标识来区分区块 可见部分[IsBlk|]+[内容|mIdX]加密部分
        Dim pri_key As RSA.PrivateKey = mRSABank(mId).getPrivateKey(mId) '获取本机私钥
        Return New Message(RSA.encryptByPrivate(tmp ,pri_key), UNKNOWN) 
    End Function

    Public Class Block
    '   Public front_block_hash As String
    '   Public BlockId As Integer
    '   Public my_hash As String
    '   Public Nonce As Integer
    '   Public msgs As New List(Of String)
    '   Public Pid As Integer
    '   Public Sub New(ByVal pid As Integer,ByVal msg_buff As List(Of String),ByVal prev As String,ByVal blockid As Integer,ByVal nonce As String) As Block  
    '       front_block_hash=prev
    '       BlockId=blockid
    '       Nonce=nonce
    '       msgs.Clear()
    '       msgs.AddRange(msg_buff)
    '       Pid=pid
    '   End Sub
    '
    '   '计算block的hash
    '   Public Sub GetBlockHash(ByVal block As Block) As String
    '       Dim temp As String = CStr(block.BlockId) + block.Nonce '拼接字符串：blockid+nonce+5条信息+pid+front_block_hash
    '       For Each msg As String In block.msgs
    '           temp = temp + msg
    '       Next
    '
    '       temp = temp + CStr(block.Pid) + block.front_block_hash
    '
    '       '计算哈希
    '       hash = MD5Encrypt(temp)
    '       Return hash
    '   End Sub
    '
    '   '计算合理的nonce，合理之后打包成block
    '   Public Sub createBlock(ByVal msg_buff As List(Of String),ByVal prev As String,ByVal blockId As Integer,ByVal n As Integer)
    '       '循环找出pid对应的nonce
    '       Dim hash As String
    '       Dim pid As Integer
    '       pid=0
    '       Do While True
    '           
    '           Dim nonce As String 
    '           '生成长度为20的随机字符串
    '           nonce = Rand.NextString(20) 
    '           '创建一个block，把信息填入
    '           Public block As New Block(pid,msg_buff,prev,blockid,nonce)
    '           '计算block的hash
    '           hash=GetBlockHash(block)
    '
    '           '判断hash是否符合条件
    '           If Left(hash, 2) = "00" Then
    '               '将这个合理的hash填上block
    '               block.my_hash=hash 
    '               'todo发送block给各个进程
    '               
    '               Exit Do
    '           End If  
    '
    '           'pid+1
    '           pid=pid+1
    '           pid=pid%n
    '
    '       Loop
    '
    '   End Sub
    '
    End Class

End Class





'''
'加入功能(实现模拟BFT网络模块一个)
    'vector<String>(Decisions) SimBFT(int SourceID, int FaultyNum, int ProcessNum, [String srcValue] = "Attack!"){}

'''
'主要逻辑(下文提到网络中的主机即为进程):
    '注意:若源进程非坏进程,达到共识的所有进程决策均与源进程一致.否则其他进程的共识有可能与源进程不一致.
    '当输入网络的坏主机数 m >= n/3 提供警告
    '0.收到外界输入的信息
    '1.生成网络拓扑特征
        '->定义网络中的总主机数目
        '->定义输入网络的源信息(即将外界信息转换为源信息)
        '->定义发信主机编号
        '->随机设置坏主机编号 SetFaultyProcess()
            '->发信主机被设为坏主机,提供警告
        '->统一定义坏主机的发信行为:随机发送8位字符长度的字符串 GetValue()
        '->设置主机决策默认值,用于打破决策时1与0相等的情况 GetDefault()
            '->DefaultValue = "Retreat!"
    '2.生成网络主机(进程)
        '->每个主机共用同一个网络拓扑特征
        '->定义每个主机收信后存储信息的决策树
            '->生成决策树 GenerateChildren()
            '->用一个广义表(List)存储每个结点与子节点间的路径关系,用于深度遍历 mChildren
            '->用一个广义表(List)存储每一层所有结点的路径信息,用于每层的水平遍历 mPathsByRank
            '->用一个广义表(List)存储每个路径对应的结点信息 mMessages
        '->定义每个主机的行为
            '->发信 SendMessage()
                '->即往其他主机的决策树的每个结点填入信息
            '->收信并存储入决策树 ReceiveMessage()
                '->对本机决策树每层水平遍历，并调用决策函数
            '->决策 Decide() 
                '->统计数目最多的信息，送往上层
    '3.主机互相发信息
    '4.各自统计信息并决策
    '5.将决策存储入字符串组并返回 return vector<string> decisions
'''


Public Function SimBFT(ByVal traits As Traits) As List(Of String)
    If traits.mN < 4 Then
        MessageBox.Show("总主机数不能少于4", "Warning")
        Return Nothing
    End If

    If traits.mM >= (traits.mN / 3) Then
        MessageBox.Show("网络中的坏主机数量超出算法可容忍数量", "Warning")
    End If
    
    '初始化整个网络
    Dim processes As New List(Of Process)
    Dim decisions As New List(Of String)
    For i As Integer = 0 To traits.mN - 1 '启动每个进程
        Dim tmp_process As New Process(i, traits)
        processes.Add(tmp_process)
    Next

    '进程们的行为
    For i As Integer = 0 To traits.mM '一共m轮
        For Each a_process As Process In processes '给别的进程发信
            a_process.SendMessage(i, processes)
        Next
    Next
    For Each a_process As Process In processes '决策阶段
        decisions.Add(a_process.Decide()) 'a_process.Decide()
    Next
    Process.ClearPathsTree() '清理进程通信的路径树

    Return decisions
End Function


'决策树所使用的信息结点
Public Class Message
    Public input_value As String
    Public output_value As String

    Sub New(ByVal input As String, ByVal output As String, Optional is_blk As Boolean = False)
        input_value = input
        output_value = output
    End Sub

End Class

'定义整个拓扑特征
Public Class Traits
    Public mSource As Integer
    Public mM As Integer
    Public mN As Integer
    Public mSrcVal As Message
    Public mFaultyProcesses As New List(Of Integer) 

    Sub New(ByVal source As Integer, ByVal m As Integer, ByVal n As Integer, ByVal srcval As Message)
        mSource = source
        mM = m
        mN = n
        mSrcVal = srcval
        SetFaultyNode(m, n)
    End Sub

    '随机设置坏进程
    Public Sub SetFaultyNode(ByVal m As Integer, ByVal n As Integer) 
        If mFaultyProcesses IsNot Nothing Then
            mFaultyProcesses.Clear
        End If
        For i As Integer = 1 To m
            Dim j As Integer = Rand.Next(0, n)
            Do While mFaultyProcesses.Contains(j) = True '检测是否已经存在这个坏进程
                j = Rand.Next(0, n) '若该进程已为坏进程,则再随机设置数值
            Loop
            mFaultyProcesses.Add(j)
        Next

        '打印提示，哪些是坏主机
        If mFaultyProcesses IsNot Nothing Then 
            Dim faulty_name As String = ""
            For Each k As Integer In mFaultyProcesses
                If mFaultyProcesses.IndexOf(k) <> (mFaultyProcesses.Count - 1) Then
                    faulty_name = faulty_name & CStr(k) & ","
                Else
                    faulty_name = faulty_name & CStr(k)
                End If
            Next
            If mFaultyProcesses.Contains(mSource) Then
                MessageBox.Show(" 主机: " & faulty_name & "号 被设为坏主机. " & Chr(10) & "发信主机是一个坏主机!" , "Warning")
            Else
                If faulty_name <> "" Then
                MessageBox.Show(" 主机: " & faulty_name & "号 被设为坏主机. ", "Warning")
                End If
            End If
        End If
    End Sub

    '获取前一轮信息的行为模式,可以更改
    Public Function GetValue(ByVal value As String, ByVal source As Integer) As String
        If mFaultyProcesses.Contains(source) = False Then
            Return value
        Else
            Return Rand.NextString(8)  '随机发8字符长度的字符串
        End If
    End Function

    '取任意值以打破0与1信息数量相同的情况
    Public Function GetDefault() As String
        Return "Retreat!"
    End Function

End Class

Public Class Process
    Private mId As Integer '每个进程的ID
    Private mMessages As New Dictionary(Of String, Message) 'map<Path, Message> 存储树中所有的具体结点
    Private Shared mTraits As Traits '程序开始拓扑特征不变，类内部共享节约内存
    Private Shared mChildren As New Dictionary(Of String, List(Of String)) 'map<CurrentMessagePath, vector<NextMessagePath>> 记录结点与其子节点的关系
    Private Shared mPathsByRank As New Dictionary(Of Integer, Dictionary(Of Integer, List(Of String))) 'map<Rank, map<SourceProcessId, vector<SendingMessagePath>>> 记录每一层的所有结点

    Sub New(ByVal id As Integer, ByRef traits As Traits)
        mId = id
        mTraits = traits
        If mChildren.Count = 0 Then '若未创建收信决策树，创建共有的决策树
            Dim tmp As New List(Of Boolean)
            For i As Integer = 0 To mTraits.mN - 1
                tmp.Add(True)
            Next
            GenerateChildren(mTraits.mM, mTraits.mN, tmp, mTraits.mSource, "", 0)
        End If
        If mId = mTraits.mSource Then
            mMessages.Add("", mTraits.mSrcVal)
        End If
    End Sub

    '生成决策树的各个子节点
    Private Sub GenerateChildren(ByVal m As Integer, ByVal n As Integer, ByVal ids As List(Of Boolean), ByVal source As Integer, ByVal cur_path As String, ByVal rank As Integer)
        ids(source) = False
        cur_path += Cstr(source)
        If mPathsByRank.ContainsKey(rank) = False Then '先检测是否存在以防止非法访问
            Dim tmp1 As New Dictionary(Of Integer, List(Of String))
            mPathsByRank.Add(rank, tmp1)
            Dim tmp2 As New List(Of String)
            mPathsByRank(rank).Add(source, tmp2)
        Else 
            If mPathsByRank(rank).ContainsKey(source) = False Then '先检测是否存在以防止非法访问
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
                        Dim tmp As New List(Of String)
                        mChildren.Add(cur_path, tmp)
                    End If
                    mChildren(cur_path).Add(cur_path + Cstr(i))
                End If
            Next
        End If
        ids(source) = True '引用的ids不知道什么原因在整个递归中成为了静态变量,返回时要把id调回true
    End Sub

    '获取信息
    Private Sub ReceiveMessage(ByRef path As String, ByVal msg As Message)
        mMessages(path) = msg
    End Sub

    '统计信息
    Private Function GetMajority(ByRef path As String) As String
        Dim counts As New Dictionary(Of String, Integer)

        'store massages to the hashmap and counts them
        If mChildren.ContainsKey(path) Then    '先检测是否存在以防止非法访问
            For Each child As String In mChildren(path)
                If mMessages.ContainsKey(child) Then    '先检测是否存在以防止非法访问
                    If mMessages(child).output_value IsNot Nothing Then
                        If counts.ContainsKey(mMessages(child).output_value) = False Then '如果 map 为空
                            counts.Add(mMessages(child).output_value, 0) 'store it into the hashmap
                        Else
                            counts(mMessages(child).output_value) = counts(mMessages(child).output_value) + 1 '直接自增不符合语法,只好写得这么复杂
                        End If
                    End If
                End If
            Next

            'get the majority
            Dim looping_flag As Boolean = False
            Dim tie_flag As Boolean = False
            Dim max_key As String
            For Each i As String In counts.Keys
                If looping_flag = False Then
                    max_key = i
                    looping_flag = True
                Else
                    If counts(i) > counts(max_key) Then
                        max_key = i
                        tie_flag = False
                    ElseIf counts(i) = counts(max_key) Then
                        max_key = i
                        tie_flag = True
                    End If
                End If
            Next
            If tie_flag = True Then '出现数量相等的两条信息，返回撤退
                Return mTraits.GetDefault()
            Else
                Return max_key
            End If

        End If
    End Function

    '清空决策树
    Public Shared Sub ClearPathsTree()
        Process.mChildren.Clear '清除Process类内部的共享变量
        Process.mPathsByRank.Clear
    End Sub

    '发送信息
    Public Sub SendMessage(ByVal r0und As Integer, ByRef processes As List(Of Process))
        If mPathsByRank(r0und).ContainsKey(mId) Then '若该轮存在mId号进程发出的信息
            For Each src_msg_next_path As String In mPathsByRank(r0und)(mId)
                If mMessages.ContainsKey(src_msg_next_path.SubString(0, src_msg_next_path.Length - 1)) Then '若路径树中存在该路径相关的结点
                    Dim src_msg = mMessages((src_msg_next_path.SubString(0, src_msg_next_path.Length - 1))) '得出发信结点信息
                    For Each a_process As Process In processes  
                       If processes.IndexOf(a_process) <> mTraits.mSource Then '如果不是源进程则收信
                           Dim tmp_msg As New Message(mTraits.GetValue(src_msg.input_value, mId), UNKNOWN)
                           a_process.ReceiveMessage(src_msg_next_path, tmp_msg)
                       End If
                   Next
                End If
            Next
        End If
    End Sub

    '决策
    Public Function Decide() As String
        If mId = mTraits.mSource Then '若是将军
            If mTraits.mFaultyProcesses.Contains(mId) = False Then '将军不是坏进程
                Return mMessages("").input_value
            Else
                Return mMessages("").output_value
            End If
        End If
        If mTraits.mFaultyProcesses.Contains(mId) = False Then '若不是坏进程
            For i As Integer = 0 To mTraits.mN - 1 '最后一层直接把输入值作为输出值
                If mPathsByRank(mTraits.mM).ContainsKey(i) Then '先检测是否存在以防止非法访问
                    For Each path As String In mPathsByRank(mTraits.mM)(i)
                        If mMessages.ContainsKey(path) Then '先检测是否存在以防止非法访问
                            mMessages(path).output_value = mMessages(path).input_value
                        End If
                    Next
                End If
            Next
            For r0und As Integer = mTraits.mM - 1 To 0 Step -1
                For i As Integer = 0 To mTraits.mN - 1 
                    If mPathsByRank.ContainsKey(r0und) Then '先检测是否存在以防止非法访问
                        If mPathsByRank(r0und).ContainsKey(i) Then '先检测是否存在以防止非法访问
                            For Each path As String In mPathsByRank(r0und)(i)
                                If mMessages.ContainsKey(path) Then '先检测是否存在以防止非法访问
                                    mMessages(path).output_value = GetMajority(path)
                                End If
                            Next
                        End If
                    End If
                Next
            Next
        End If
        Return mMessages(mPathsByRank(0)(mTraits.mSource)(0)).output_value
    End Function

End Class