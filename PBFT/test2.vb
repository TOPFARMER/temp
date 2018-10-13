'全局定义

Public ONE As String = "1"
Public ZERO As String = "0"
Public UNKNOWN As String = "?"
Public FAULTY As String = "X"

'决策树所使用的信息结点

Public Class Node
    Public input_value As String
    Public output_value As String

    Sub New(ByVal input As String,ByVal output As String)
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

    Sub New(ByVal source As Integer, ByVal m As Integer,ByVal n As Integer,ByVal srcval As Node)
        mSource = source
        mM = m
        mN = n
        mSrcVal = srcval
    End Sub

End Class


Public Structure employee
    Public firstName As String
    Public middleName As String
    Public lastName As String
End Structure