Public Plaintext As Integer
Public Ciphertext As Long
Public n As Integer
Public d As Integer
Public e As Integer = 0


Public Sub RSA_Initialize()
	Dim prime(5000) As Integer
	Dim count_Prime As Integer = ProducePrimeNumber(prime)
	Randomize
	Dim ranNum1 As Integer = int(rnd*(count_Prime+1))
	Dim ranNum2 As Integer = int(rnd*(count_Prime+1))
	Dim p As Integer = prime(ranNum1)
	Dim q As Integer = prime(ranNum2)
	n = p*q
	Dim o As Integer = (p-1)*(q-1)
        Dim j As Integer
	For j = 3 To o Step 1331
		Dim gcd As Integer = Exgcd(j, o, d)
		If gcd = 1 And d > 0
			e=j
			Exit For
		End If
	Next
End Sub

Public Function dec2hex(ByVal i As Integer, ByVal width As Integer) As String
	Return right("0" & hex(i),width)
End Function

Public Function BianaryTransform(ByVal num As Integer, ByVal bin_num() As Integer) As String
	Dim i As Integer = 0,Modnum As Integer = 0
	While num <> 0
		Modnum = num Mod 2
		bin_num(i)=Modnum
		num=num/2
		i=i+1
	End While
	Return i
End Function

Public Function Modular_Exonentiation(ByVal a As Long, ByVal b As Integer, ByVal n As Integer) As Long
	Dim c As Integer = 0,bin_num(1000) As Integer
	Dim d As Long = 1
	Dim k As Integer = BianaryTransform(b,bin_num)-1
	Dim i As Integer
	For i = k To 0 Step -1
		c = 2*c
		d = (d*d) Mod n
		If bin_num(i) = 1
			c = c + 1
			d = (d*a) Mod n
                End If
        Next
	Return d
End Function

Public Function ProducePrimeNumber(ByVal prime() As Integer) As Integer
	Dim c As Integer = 0,vis(1001) As Integer
	Dim i As Integer
        For i = 0 To UBound(vis)
		vis(i)=0
        Next
        Dim z As Integer
	For z = 2 To 1000
		If vis(z)=0
			prime(c)=z
                        c=c+1
                        Dim j As Integer
			For j = z*z To 1000 Step z
				vis(j)=1
                        Next
                End If
        Next
	Return c
End Function

Public Function Exgcd(ByVal m As Integer, ByVal n As Integer, ByRef x As Integer) As Integer
	Dim x1 As Integer = 0,y1 As Integer = 1,x0 As Integer = 1,y0 As Integer = 0, y As Integer = 1
	x=0
	Dim r As Integer = m Mod n
	Dim q As Integer = (m-r)/n
	While r
		x=x0-q*x1
		y=y0-q*y1
		x0=x1
		y0=y1
		x1=x
		y1=y
		m=n
		n=r
		r=m Mod n
		q=(m-r)/n
	End While
	Return n
End Function


Public Sub RSA_Encrypt()
	Ciphertext = Modular_Exonentiation(Plaintext, e, n)
End Sub

Public Sub RSA_Decrypt()
        Ciphertext = Modular_Exonentiation(Ciphertext, d, n)
End Sub

Public Function RSA_Encrypt(ByVal num As Long) As Long
    While e=0
		RSA_Initialize()
	End While
    Return Modular_Exonentiation(num, e, n)
End Function

Public Function RSA_Decrypt(ByVal num As Long) As Long
    While e=0
		RSA_Initialize()
	End While
    Return Modular_Exonentiation(num, d, n)
End Function

Public Function RSA_Initial() As Integer()
    While e=0
	RSA_Initialize()
    End While
    Dim q() As Integer = {d,e}
    Return q
End Function

Sub main(ByVal num As Integer)
	Plaintext = num
	While e=0
		RSA_Initialize()
	End While
	RSA_Encrypt()
	RSA_Decrypt()
End Sub