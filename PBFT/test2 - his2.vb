'全局定义

Public ONE As String = "1"
Public ZERO As String = "0"
Public UNKNOWN As String = "?"
Public FAULTY As String = "X"

Public N As Integer = 7
Public M As Integer = 2
Public SOURCE As Integer = 3 '指定将军结点
Public SRCVAL As New Node(ZERO, UNKNOWN) '将军收到的源数据
Public FaultyProcesses() As Integer = {2, 3} '指定坏进程

'决策树所使用的信息结点

Public Class Node
    Public input_value As String
    Public output_value As String

    Sub New(ByVal input As String, ByVal output As String)
        input_value = input
        output_value = output
    End Sub

End Class

'定义整个拓扑特征

Public Class Traits
    Public mSource As Integer
    Public mM As Integer
    Public mN As Integer
    Public mSrcVal As Node
    Public mFaultyProcesses() As Integer

    Sub New(ByVal source As Integer, ByVal m As Integer, ByVal n As Integer, ByVal srcval As Node, ParamArray mfaultyprocesses As Integer())
        mSource = source
        mM = m
        mN = n
        mSrcVal = srcval
        mFaultyProcesses = mfaultyprocesses
    End Sub

    '获取前一轮信息的行为模式,可以更改
    Public Function GetValue(ByVal value As String, ByVal source As Integer) As String
        If Array.IndexOf(mFaultyProcesses, source) = -1 Then
            Return value
        Else
            return Cstr(Rand.Next(2)) '随机发0或1
        End If
    End Function

    '取任意值以打破0与1信息数量相同的情况
    Public Function GetDefault() As String
        Return ONE
    End Function

End Class

Public Class Process
    Private mId As Integer '每个进程的ID
    Private mNodes As New Dictionary(Of String, Node) 'map<Path, Node> 存储树中所有的具体结点
    Private Shared mTraits As New Traits(SOURCE, M, N, SRCVAL, FaultyProcesses) 
    Private Shared mChildren As New Dictionary(Of String, List(Of String)) 'map<CurrentNodePath, vector<NextNodePath>> 记录结点与其子节点的关系
    Private Shared mPathsByRank As New Dictionary(Of Integer, Dictionary(Of Integer, List(Of String))) 'map<Rank, map<SourceProcessId, vector<SendingNodePath>>> 记录每一层的所有结点
    
    Private Sub GenerateChildren(ByVal m As Integer, ByVal n As Integer, ByVal ids As List(Of Boolean), ByVal source As Integer, ByVal cur_path As String, ByVal rank As Integer)
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

    Private Sub ReceiveMessage(ByRef path As String, ByVal node As Node)
        mNodes(path) = node
    End Sub

    Private Function GetMajority(ByRef path As String) As String
        Dim counts As Dictionary(Of String, Integer)
        counts.Add(ONE, 0)
        counts.Add(ZERO, 0)
        counts.Add(UNKNOWN, 0)
        For Each child As String In mChildren(path)
            counts(mNodes(child).output_value) = counts(mNodes(child).output_value) + 1 '直接自增不符合语法，只好写得这么复杂
        Next
        If counts(ONE) > (mChildren(path).Count / 2) Then
            return ONE
        End If
        If counts(ZERO) > (mChildren(path).Count / 2) Then
            return ZERO
        End If
        If counts(ONE) = counts(ZERO) AndAlso counts(ONE) = (mChildren(path).Count / 2) Then
            return mTraits.GetDefault()
        End If
    End Function

    Sub New(ByVal id As Integer)
        mId = id
        If mChildren.Count = 0 Then
            Dim tmp As New List(Of Boolean)
            For i As Integer = 0 To mTraits.mN - 1
                tmp.Add(True)
            Next
            GenerateChildren(mTraits.mM, mTraits.mN, tmp, mTraits.mSource, "", 0)
        End If
        If mId = mTraits.mSource Then
            mNodes.Add("", mTraits.mSrcVal)
        End If
    End Sub

    Public Sub SendMessage(ByVal r0und As Integer, ByRef processes As List(Of Process))
        For Each src_node_next_path As String In mPathsByRank(r0und)(mId) 
            Dim src_node = mNodes((src_node_next_path.SubString(0, src_node_next_path.Length - 1))) '得出发信结点信息
            For i As Integer = 0 To mTraits.mN - 1
                If i <> mTraits.mSource Then '如果不是源进程则收信
                Dim tmp_node As New Node(mTraits.GetValue(src_node.input_value, mId), UNKNOWN)
                    processes(i).ReceiveMessage(src_node_next_path, tmp_node)
                End If
            Next
        Next
    End Sub

    Public Function Decide() As String
        If mId = mTraits.mSource Then
            return mNodes("").input_value
        End If
        For i As Integer = 0 To mTraits.mN - 1 '最后一层直接把输入值作为输出值
            For Each path As String In mPathsByRank(mTraits.mM)(i)
                mNodes(path).output_value = mNodes(path).input_value
            Next
        Next
        For r0und As Integer = mTraits.mM - 1 To 0 Step -1
            For i As Integer = 0 To mTraits.mN - 1 
                For Each path As String In mPathsByRank(r0und)(i)
                    mNodes(path).output_value = GetMajority(path)
                Next
            Next
        Next
        return mNodes(mPathsByRank(0)(mTraits.mSource)(0)).output_value
    End Function

End Class

Public Sub SimBFT()
    Dim processes As List(Of Process)
    For i As Integer = 0 To N - 1
        Dim tmp_process As New Process(i)
        processes.Add(tmp_process)
    Next
    For i As Integer = 0 To M '一共m轮
        For Each a_process As Process In processes '给别的进程发信
            a_process.SendMessage(i, processes)
        Next
    Next
    For Each a_process As Process In processes
        Output.Show(a_process.Decide())
    Next
End Sub