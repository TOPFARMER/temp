'项目属性->LoadUserSetting
'隔离各个用户
If User.Group = "User" Then
    For Each t As Table In Tables
        t.Visible = False
    Next
    Tables("SendingBox").Visible = True
    Tables("ConfirmSignature").Visible = True
    Tables(User.Name).Visible = True
    For Each c As Col In Tables("SendingBox").Cols
        c.Visible = False
    Next
    Tables("SendingBox").Cols(User.Name).Visible = True
Else
    For Each t As Table In Tables
        t.Visible = True
        For Each c As Col In t.Cols
            c.Visible = True
        Next
    Next
    '清空用户表
  ' For Each i As String In User.Group
  '     For Each j As DataRow In DataTables(i).DataRows
  '         j.Delete()
  '     Next
  ' Next
  ' DataTables(i).AddNew()
End If

'Table:SendingBox->表事件->DataColChanged
'定义发送信息行为
If e.DataTable.Name = "SendingBox" Then 
    If Left(e.DataCol.Name, 4) = "User" Then 
        If e.NewValue <> "" Then
        SimBlockChain(CInt(Right(e.DataCol.Name, 1)), 0, 5, e.DataRow(e.DataCol.Name))
        MessageBox.Show("发送完毕！")
        For i As Integer = 0 To 4
            Dim blk As Block = Ledger.GetNewBlock(i)
            If blk.BlockId = 0 Then '若为基块直接输出
                DataTables("User" & CStr(i)).DataRows(0)("块号") = "0"
                DataTables("User" & CStr(i)).DataRows(0)("NONCE") = "0555DD"
                DataTables("User" & CStr(i)).DataRows(0)("前区块哈希")= "000000000000000000000000"
                DataTables("User" & CStr(i)).DataRows(0)("本区块哈希") = "0008NtOX5fpuSX21PjSrZw=="
            Else
                Dim exist As Boolean = False
                For Each rows As DataRow In DataTables("User" & CStr(i)).DataRows
                    If rows("块号") = CStr(blk.BlockId) Then '已存在该块号记录
                        exist = True 
                        Exit For
                    End If
                Next
                If exist = False Then
                    Dim block_info As DataRow = DataTables("User" & CStr(i)).AddNew()
                    block_info("块号") = blk.BlockId
                    block_info("NONCE") = blk.Nonce
                    block_info("记录1") = blk.Messages(0) '可以定义循环,但不直观
                    block_info("记录2") = blk.Messages(1)
                    block_info("记录3") = blk.Messages(2)
                    block_info("记录4") = blk.Messages(3)
                    block_info("记录5") = blk.Messages(4)
                    block_info("前区块哈希")= blk.PrevHash
                    block_info("本区块哈希") = blk.MyHash
                    Dim log_info As DataRow = DataTables("User" & CStr(i)).AddNew()
                    log_info("记录1") = Ledger.GetDecryptMsg(blk.Messages(0))(0) '可以定义循环,但不直观
                    log_info("记录2") = Ledger.GetDecryptMsg(blk.Messages(1))(0)
                    log_info("记录3") = Ledger.GetDecryptMsg(blk.Messages(2))(0)
                    log_info("记录4") = Ledger.GetDecryptMsg(blk.Messages(3))(0)
                    log_info("记录5") = Ledger.GetDecryptMsg(blk.Messages(4))(0)
                    If User.Name = "User" & CStr(i) Then
                        MessageBox("收到新区快！")
                    End If
                    If User.Group <> "User" Then
                        MessageBox(CStr(i) & "号主机收到新区快！")
                    End If
                End If
            End If
        Next
        End If
    End If
End If


'Table:ConfirmSignature->表事件->DataColChanged
'查询签名信息
If e.DataTable.Name = "ConfirmSignature" Then 
    If e.DataCol.Name = "待解密字串" Then 
        If e.NewValue <> "" Then
            MessageBox.Show("解密完毕!")
            e.DataRow("字串内容") = Ledger.GetDecryptMsg(e.NewValue)(0)
            e.DataRow("发出主机") = Ledger.GetDecryptMsg(e.NewValue)(1)
            e.DataRow("对应公钥") = Ledger.GetDecryptMsg(e.NewValue)(2)
        End If
    End If
End If