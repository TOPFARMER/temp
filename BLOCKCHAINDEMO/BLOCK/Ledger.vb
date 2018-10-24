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
    srcval_msg = ledgers(source).SignedSrcMsg(srcval) '将源信息转换为可发送的信息
    Dim traits As New Traits(source, m, n, srcval_msg) '定义拓扑特征
    Dim decisions As List(Of String) = SimBFT(traits) '使用BFT算法，在网络中传信，得出每个主机的决策
    
    '定义账单类行为
    For i As Integer = 0 To n -1 '开始记账
        ledgers(i).GetAndCheckMessage(source ,decisions(i))
    Next

    '开始检查缓冲区是否有5条消息
    Dim competitors As New List(Of Integer) '统计同时计算区块的主机
    For i As Integer = 0 To n -1 
        If ledgers(i).CheckMsgBox = True Then
            competitors.Add(i)
        End If
    Next
    If competitors.Count > 0 Then '开始竞争
        Dim SGHAN As New Dictionary(Of Integer, String)  'map<Id, Hash&Nonce> 同时得出正确哈希的存储表 SimultaneouslyGenerateHashAndNonce 
        Do While True
            For Each i As Integer In competitors '每个竞争者同时哈希一次
                Dim hash As String = ledgers(i).MineOnce()
                If Left(hash, 3) = "000" Then
                    SGHAN.Add(i, hash)
                End If
            Next
            If SGHAN.Count > 0 Then '该轮有主机得出正确哈希，结束循环
                Exit Do
            End If
        Loop
        For i As Integer = 0 To n - 1 '正确哈希的主机生成区块
            If SGHAN.ContainsKey(i) Then
                Dim blk As Block = ledgers(i).GenerateBlock(SGHAN(i)) 

                '暂时只取一个主机发区块                
                srcval_msg = ledgers(i).CSrcBlock(blk) '将区块转换为信息
                traits = New Traits(source, m, n, srcval_msg) '定义拓扑特征
                decisions  = SimBFT(traits) '使用BFT算法，在网络中传信，得出每个主机的决策
                For j As Integer = 0 To n -1 '开始记账
                    ledgers(j).GetAndCheckMessage(source ,decisions(j))
                Next

                '暂时清空所有机的信息缓存
                For j As Integer = 0 To n -1 '本轮同步结果写入存储
                    ledgers(j).CleanMsgsBuff
                Next

                Exit For
                '暂时只取一个主机发区块

            End If
        Next
    End If
 
    For i As Integer = 0 To n -1 '本轮同步结果写入存储
        ledgers(i).WriteToStorge()
    Next

End Sub

'账单类，负责对信息处理
Public Class Ledger
    Private Shared mRSABank As New List(Of RSA) 'vector<RSA> 用于存储生成的RSA密钥
    Private Shared mChainStorge As New List(Of List(Of Block)) 'vector<vector<Block>> 用于存储每个主机的区块链
    Private Shared mMsgStorge As New List(Of List(Of String)) 'vector<vector<Message>> 用于存储每个主机的区块链


    '基块的信息
    Public Shared TMP_HASH As String = "00000000000000000000000000000000"
    Public Shared BASE_HASH As String = "0002dQ2vxDRHZbuTmKgOWg=="
    Public Shared BASE_NONCE As String = "48E66"
    Public Shared BASE_MSGS As New List(Of String)
    Public Shared BLOCK_BASE As Block = New Block(0, BASE_NONCE, BASE_MSGS, TMP_HASH, BASE_HASH)


    Public mId As Integer
    Private blockchain As New List(Of Block) '存放区块链
    Private msg_buff As New List(Of String) '存放5条信息

    Public Sub New(ByVal id As Integer)
        mId = id

       '若从未开启过主机,则按序生成主机时，已有链数与新建主机号相等
       '同时说明消息存储结构也为空
       If mId = mChainStorge.Count Then
           blockchain.Add(BLOCK_BASE)
           mChainStorge.Add(blockchain)
           mMsgStorge.Add(New List(Of String))
       End If

       '若已有该主机的链，取回本地
       '同时说明消息存储结构已不为空，将消息取回本地
       If mId < mChainStorge.Count Then
           blockchain.AddRange(mChainStorge(mId))
           msg_buff.AddRange(mMsgStorge(mId))
       End If

    End Sub

    '对源信息签名
    Public Function SignedSrcMsg(ByVal input As Object) As Message
        Dim tmp As String = CStr(input) & "|mId" & CStr(mId) '为信息添加校验信息 信息内容 |mIdX
        Dim pri_key As RSA.PrivateKey = mRSABank(mId).getPrivateKey(mId) '获取本机私钥
        Dim code As String = "IsMsg|" & RSA.encryptByPrivate(tmp ,pri_key) '返回签名信息 加入 IsMsg 标识来区分信息 可见部分[IsMsg|]+[内容|mIdX]加密部分
        Return New Message(code, UNKNOWN) 
    End Function

    '将共识得出得信息进行有效性检查并添加到本地的表中
    Public Sub GetAndCheckMessage(ByVal source As Integer, ByVal code As String)
        If code = "Retreat!" OrElse code = "?" Then '获得无意义信息，啥也不干
        Else
            Dim msg As String 
            If code.split("|")(0) = "IsMsg" Then ' 若信息为纯信息
                msg = RSA.decryptByPublic(code.split("|")(1) ,mRSABank(source).public_key) '对密文使用公钥解密
                If msg.split("|")(1) = "mId" & CStr(source) Then '校对确认内部信息有意义，密文信息加到缓冲区
                    msg_buff.Add(code.split("|")(1) & "|mId" & CStr(source)) ' 加密部分[内容|mIdX]+[|mIdX]可见部分 
                End If
            End If
            If  code.split("|")(0) = "IsBlk" Then '若信息为区块
                Dim blk As Block = CheckBlock(code.split("|")(1)) '区块进行组装并检查
                    If blk IsNot Nothing Then
                        blockchain.Add(blk)
                    End If
            End If
        End If
    End Sub

    '检查区块有效性
    Public Function CheckBlock(ByVal str As String) As Block 
        Output.Show(str)
        Dim blk As Block = Msg2Block(str)
        If blk.BlockId <> blockchain.Count Then '收到的块编号不是最新则丢弃
            Return Nothing
        End If
        If blk.PrevHash <> blockchain(blockchain.Count - 1).MyHash Then '收到的块的上一个哈希对不齐，丢弃
            Return Nothing
        End If
        For Each code As string In blk.Msgs '检查内部记录是否符合签名，不符合则丢弃
            If Left(code.split("|")(1), 3) = "mId" Then '检查末尾
                Dim i As Integer = CInt(Right(code.split("|")(1), 1)) '得到信息来源主机号
                Dim msg As String = RSA.decryptByPublic(code.split("|")(0) ,mRSABank(i).public_key) '对密文使用公钥解密
                If msg.split("|")(0) <> "mId" & CStr(i) Then '解出信息无意义,丢弃
                    Return Nothing
                End If
            End If
        Next
        If blk.MyHash <> GetHash(blk.BlockId, blk.Nonce, blk.Msgs, blk.PrevHash) Then '哈希对不上，丢弃
            Return Nothing
        End If
        Return blk
    End Function

    '检查信息缓冲区的信息条数
    Public Function CheckMsgBox() As Boolean
        If msg_buff.Count >= 5 Then
            Return True
        Else
            Return False
        End If
    End Function

    '清空自己的信息缓存
    Public Sub CleanMsgsBuff()
        msg_buff.Clear
    End Sub

    '使用得出的哈希+NONCE字符串生成区块
    Public Function GenerateBlock(ByVal hash_nonce As String) As Block
        Dim bId As Integer = blockchain.Count
        Dim nonce = hash_nonce.SubString(24)
        Dim prev_hash = blockchain(blockchain.Count - 1).MyHash
        Dim msgs As List(Of String) = msg_buff
        Dim my_hash As String = Left(hash_nonce, 24)
        Return New Block(bId, nonce, msgs, prev_hash, my_hash)
    End Function
  
    '得出合格的哈希
  ' Public Function Mining() As String
  '     Dim bId As Integer = blockchain.Count
  '     Dim nonce = Rand.NextString(Rand.Next(1,8)).ToUpper '生成随机1到8位十六进制串
  '     Dim prev_hash = blockchain(blockchain.Count - 1).MyHash
  '     Dim msgs As List(Of String) = msg_buff

  '     Do While True
  '         Dim nonce As String = Rand.NextString(Rand.Next(1,8)).ToUpper '生成随机1到8位十六进制串
  '         Dim str As String = Block.GetHash(bId, nonce, msgs, prev_hash)
  '         If Left(str,3) = "000" Then
  '             Return str
  '             Exit Do
  '         End If
  '     Loop
  ' End Function
    '只算一次版本,不检测正确性
    '返回24位哈希和不确定位数nonce
    Public Function MineOnce() As String
        Dim bId As Integer = blockchain.Count
        Dim nonce = Rand.NextString(Rand.Next(1,8)).ToUpper '生成随机1到8位十六进制串
        Dim prev_hash = blockchain(blockchain.Count - 1).MyHash
        Dim msgs As List(Of String) = msg_buff
        Dim str As String = GetHash(bId, nonce, msgs, prev_hash)
        Return str & nonce
    End Function

    '计算区块的哈希
    Public Shared Function GetHash(ByVal bId As Integer, ByVal nonce As String, ByVal msgs As List(Of String), ByVal prev_hash As String) As String
        Dim tmp_msgs As String = CStr(bId) + nonce '拼接字符串：blockid + nonce + 5条信息 + prev_hash
        For Each msg As String In Msgs
            tmp_msgs = tmp_msgs + msg
        Next

        '计算哈希
        Return MD5Encrypt(tmp_msgs)
    End Function

    '转换区块为信息
    Public Function CSrcBlock(ByVal blk As Block) As Message
        Dim msg As String = "IsBlk|" & blk.Block2Msg() '返回签名信息 加入 IsBlk 标识来区分区块 可见部分[IsBlk|内容]
        Return New Message(msg, UNKNOWN) 
    End Function

    
    '字符串转为区块
    Public Shared Function Msg2Block(ByRef str As String) As Block
        Dim tmp() As String = str.split("\")
        Dim bId As Integer = CInt(tmp(0))
        Dim nonce As Integer = tmp(1)
        Dim msgs As New List(Of String)
        For i As Integer = 2 To 6
            msgs.Add(tmp(i))
        Next
        Dim prev_hash As String  = tmp(7)
        Dim my_hash As String = tmp(8)
        Return New Block(bId, nonce, msgs, prev_hash, my_hash)
    End Function
    
    '关机前把链与消息写入存储
    Public Sub WriteToStorge()
        mChainStorge(mId) = blockchain
        mMsgStorge(mId) = msg_buff
    End Sub

    '初始化所有进程的RSA密钥信息
    Public Shared Sub SetTheRSABank(ByVal process_num As Integer)
        For id As Integer = 0 To process_num - 1
            Dim tmp As New RSA(id)
            mRSABank.Add(tmp)
        Next
    End Sub

    '检查RSA密钥信息是否已经初始化
    ReadOnly Shared Property CheckRSABank() As Integer
        Get
            Return mRSABank.Count
        End Get
    End Property

    '清理原有的RSA密钥信息
    Public Shared Sub CleanTheRSABank() 
        mRSABank.Clear
    End Sub

    '清理所有的链
    Public Shared Sub CleanALLChains()
        mChainStorge.Clear
    End Sub

    '清理所有主机的信息
    Public Shared Sub CleanALLMsgs()
        mMsgStorge.Clear
    End Sub

End Class

Public Class Block
    Public BlockId As Integer
    Public Nonce As String
    Public Msgs As New List(Of String)
    Public PrevHash As String
    Public MyHash As String

    '构造区块
    Public Sub New(ByVal bId As Integer, ByVal nonce As String, ByVal msgs As List(Of String), ByVal prev_hash As String, ByVal my_hash As String)
        BlockId = bId
        Nonce = nonce
        Msgs.AddRange(msgs)
        PrevHash = prev_hash
        MyHash = my_hash
    End Sub
    
    '区块转为字符串
    Public Function Block2Msg() As String
        Dim msgs As String
        For Each msg As String In Msgs
            msgs = msgs & msg & "\"
        Next
        Dim tmp As String = CStr(BlockId) & "\" & Nonce & "\"  & msgs & PrevHash & "\" & MyHash
        Return tmp
    End Function

End Class
