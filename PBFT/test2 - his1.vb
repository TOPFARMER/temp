'全局定义

Public ONE As String = "1"
Public ZERO As String = "0"
Public UNKNOWN As String = "?"
Public FAULTY As String = "X"

Public N As Integer = 7
Public M As Integer = 2
Public SOURCE As Integer = 3
Public SRCVAL As New Node(ZERO, UNKNOWN) '将军收到的源数据

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

    Sub New(ByVal source As Integer, ByVal m As Integer, ByVal n As Integer, ByVal srcval As Node)
        mSource = source
        mM = m
        mN = n
        mSrcVal = srcval
    End Sub

    '获取前一轮信息的行为模式,可以更改
    Function GetValue(ByVal value As String, ByVal source As Integer, ByVal destination As Integer, ByRef path As String) As String
        If source = mSource Then
            If destination > 2 Then
                Return ZERO
            Else
                Return ONE
            End If
        ElseIf source = 2 Then '另一个坏进程的行为模式
            Return ONE
        Else
            Return value
        End If
    End Function

    '取任意值以打破0与1信息数量相同的情况
    Function GetDefault() As String
        Return ONE
    End Function

End Class

Public Class Process
    Private mId As Integer
    Private mNodes As New Dictionary(Of String, Node) 'map<Path, Node>
    Private Shared mTraits As New Traits(SOURCE, M, N, SRCVAL)
    Private Shared mChildren As New Dictionary(Of String, List(Of String))
    Private Shared mPathsByRank As New Dictionary(Of Integer, Dictionary(Of Integer, List(Of String)))
    
    Private Sub GenerateChildren(ByVal m As Integer, ByVal n As Integer, ByVal ids As List(Of Boolean),Optional ByVal source As Integer, Optional ByVal cur_path As String = "",Optional ByVal rank As Integer = 0)
        ids(source) = False
        cur_path += Cstr(source)
    End Sub

    Private Sub ReceiveMessage()
    End Sub

    Private Function GetMajority(ByRef path As String) As String
    End Function

    Sub New(ByVal id As Integer)
        mId = id
        If mChildren.Count = 0 Then
            Dim tmp As New List(Of Boolean)
            For i As Integer = 0 To mTraits.mN - 1
                tmp.Add(True)
            Next
            GenerateChildren(mTraits.mM, mTraits.mN, mTraits.mSource)
        End If
        If mId = mTraits.mSource Then
            mNodes.Add("")
    End Sub

    Public Sub SendMessage()
    End Sub

    Public Function Decide()
    End Function



End Class

'测试类代码
Dim b As New Node("1","0")
Dim a As New Traits(1,1,1,b)
Output.Show(a.mSource)