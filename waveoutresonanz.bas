/'
	endlich ein gescheites zimmerresonanz programm

	ESC:  quit
	cursor up/down:  display scaling
	scrollwheel up/down:  volume

'/


#Include "windows.bi"
#Include "win/mmsystem.bi"
#Include "gl/gl.bi"
#Include "gl/glu.bi"
#Include "fbgfx.bi"
#Include "string.bi"


Const PI = 6.283185307

Const SX = 1280			' screen size
Const SY = 600

Const SR = 44100			' samplerate

Const SBUF = 44100		' eine sekunde loop ausgabe buffer
Const SINBUF = 4*441		' zwei buffer je vier hundertstel sekunden eingabe

Const LAT = 8*441			' latenz 8 hundertstel

Const NF = 12000			' anzahl frequenzen

Const VMAX = 32766		' volume max

Const C = 343			' schallgeschwindigkeit luft 20 grad celsius in m/s

Dim Shared As Integer ende
Dim Shared As Double amp(NF)		' gemessene lautstärke

Dim Shared As Double fn, vf
Dim Shared As Integer vol, df, mx
Dim Shared As HWAVEOUT hwo
Dim Shared As Byte Ptr buf



Declare Sub main
main
End


' print a text at a screen position using opengl calllists
'
Sub mytextout (x As Double, y As Double, z As Double, s As String)
	glRasterPos3d (x, y, z)
	glListBase (1000)
	glCallLists (Len(s), GL_UNSIGNED_BYTE, StrPtr(s))
End Sub


' draw the lines
'
Sub drawlines ()
	Dim As Integer x, t
	Dim As Double a

	glColor3d (1,1,1)
	glBegin (GL_QUADS)
	For x = 0 To SX-1
		t = x+df
		If t>=0 And t<NF Then
			a = 10+vf*amp(t)/65530.0
		Else
			a = 5
		EndIf
		glVertex3d(x  ,0,0)
		glVertex3d(x+1,0,0)
		glVertex3d(x+1,a,0)
		glVertex3d(x  ,a,0)
	Next
	glEnd ()
End Sub


' write sine wave to waveout buffer ahead of waveout position
'
Sub waveoutthread (para As Any Ptr)
	Dim As MMTIME tim
	Dim As Integer t, p, mp
	Dim As Double a, w, v, fr

	w = 0
	fr = 10
	fn = 10
	vol = 1

	Do
		Sleep 10

		tim.wType = TIME_SAMPLES
		waveOutGetPosition (hwo, @tim, SizeOf(tim))
		p = tim.sample Mod SBUF

		Do
			a = v*Sin(w)
			'a = v*w
			w += PI*fr/SR
			If w>=PI Then
				w -= PI
				fr = fn
				'v = VMAX
				'v = VMAX*(1-my/SY)
				v = VMAX/10*vol
			EndIf

			t = (mp+LAT) Mod SBUF
			*Cast(Short Ptr,buf+t*4+0) = Int(a)
			*Cast(Short Ptr,buf+t*4+2) = Int(a)

			mp = (mp+1) Mod SBUF

			If mp=p Then Exit Do
		Loop

	Loop Until ende

End Sub


' get recorded amplitude and switch wavein buffers
'
Sub waveInProc (thwi As HWAVEIN, uMsg As Integer, dwInstance As Integer, param1 As Integer, param2 As Integer)
	Dim As WAVEHDR Ptr pwh
	Dim As Byte Ptr buf
	Dim As Double a, f
	Dim As Integer t

	If uMsg=WIM_DATA And ende=0 Then
		pwh = Cast(WAVEHDR Ptr, param1)
		buf = pwh->lpData

		f = 0
		For t = 0 To SINBUF-1
			a = Abs(*Cast(Short Ptr,buf+t*4))+Abs(*Cast(Short Ptr,buf+t*4+2))
			If a>f Then f = a
		Next
		If mx>=0 And df>=0 And df<NF-SX Then amp(mx+df) = f

		waveInAddBuffer (thwi, pwh, SizeOf(WAVEHDR))
	EndIf

End Sub


' main
'
Sub main ()
	Dim As WAVEOUTCAPS woc
	Dim As WAVEINCAPS wic
	Dim As HWAVEIN hwi
	Dim As WAVEFORMATEX wfx, iwfx
	Dim As WAVEHDR whd, iwhd(2)
	Dim As Byte Ptr inbuf(2)
	Dim As Any Ptr thr
	Dim As HWND hwnd
	Dim As HDC hdc
	Dim As HGLRC hglrc
	Dim As Integer d, my, button, wheel, owheel
	Dim As Double lambda


	'ScreenRes SX,SY,32,,FB.GFX_OPENGL+FB.GFX_MULTISAMPLE
	ScreenRes SX,SY,32,,FB.GFX_OPENGL

	ScreenControl (FB.GET_WINDOW_HANDLE, Cast (Integer, hwnd))
	hdc = GetDC (hwnd)
	hglrc = wglCreateContext (hdc)
	wglMakeCurrent (hdc, hglrc)
	SelectObject (hdc, GetStockObject (SYSTEM_FONT))
	wglUseFontBitmaps (hdc, 0, 255, 1000)

	' turn vertical sync off (otherwise it is on by default)
	'Dim SwapInterval As Function (ByVal interval As Integer) As Integer
	'SwapInterval = ScreenGLProc ("wglSwapIntervalEXT")
	'SwapInterval (0)

	glViewport (0, 0, SX, SY)
	glMatrixMode (GL_PROJECTION)
	glLoadIdentity ()
	glOrtho (0, SX, 0, SY, -1, 1)
	glMatrixMode (GL_MODELVIEW)
	glLoadIdentity ()

	glClearColor (0.8, 0.6, 0.4, 1)		' background color
	glEnable (GL_DEPTH_TEST)


	wfx.wFormatTag = WAVE_FORMAT_PCM
	wfx.nChannels = 2
	wfx.nSamplesPerSec = SR
	wfx.wBitsPerSample = 16
	wfx.nBlockAlign = wfx.nChannels*wfx.wBitsPerSample/8
	wfx.nAvgBytesPerSec = wfx.nSamplesPerSec*wfx.nBlockAlign
	wfx.cbSize = 0
	waveOutOpen (@hwo, 0, @wfx, 0, 0, CALLBACK_NULL)

	buf = Callocate (SBUF*4)
	whd.lpData = buf
	whd.dwBufferLength = SBUF*4
	whd.dwBytesRecorded = 0
	whd.dwFlags = WHDR_BEGINLOOP + WHDR_ENDLOOP
	whd.dwLoops = 65000
	waveOutPrepareHeader (hwo, @whd, SizeOf(whd))
	waveOutWrite (hwo, @whd, SizeOf(whd))


	iwfx.wFormatTag = WAVE_FORMAT_PCM
	iwfx.nChannels = 2
	iwfx.nSamplesPerSec = SR
	iwfx.wBitsPerSample = 16
	iwfx.nBlockAlign = iwfx.nChannels*iwfx.wBitsPerSample/8
	iwfx.nAvgBytesPerSec = iwfx.nSamplesPerSec*iwfx.nBlockAlign
	iwfx.cbSize = 0
	waveInOpen (@hwi, 0, @iwfx, Cast(DWORD_PTR, @waveInProc), 0, CALLBACK_FUNCTION)

	For d = 0 To 1
		inbuf(d) = Allocate (SINBUF*4)
		iwhd(d).lpData = inbuf(d)
		iwhd(d).dwBufferLength = SINBUF*4
		iwhd(d).dwBytesRecorded = 0
		iwhd(d).dwFlags = 0
		iwhd(d).dwLoops = 0
		waveInPrepareHeader (hwi, @iwhd(d), SizeOf(iwhd(d)))
		waveInAddBuffer (hwi, @iwhd(d), SizeOf(iwhd(d)))
	Next
	waveInStart (hwi)


	thr = ThreadCreate (@waveoutthread)


	vf = 1000
	df = 6000

	Do
		glClear (GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT)

		glColor3d (1,1,1)
		mytextout (20, SY-1*20, 0, "frequency: "+Format(fn,"0.00")+" hz   wavelength: "+Format(lambda,"0.000")+" m   volume: "+Str(vol))

		drawlines ()

		If Inkey=Chr(27) Then
			vol = 0
			Sleep 500
			Exit Do
		EndIf
		If GetAsyncKeyState (VK_UP  )<0 Then vf *= 1.05
		If GetAsyncKeyState (VK_DOWN)<0 Then vf /= 1.05

		owheel = wheel
		GetMouse mx, my, wheel, button
		If mx<0 Then wheel = owheel

		If mx>=0 And df>=0 And df<NF-SX Then
			'lambda = (mx+df+1)/1000
			'fn = C/lambda

			'fn = (mx+df+1)/100
			fn = 2^((mx+df+1)/1000)

			lambda = C/fn

			vol += wheel-owheel
			If vol<0 Then vol = 0
			If vol>10 Then vol = 10
		EndIf

		If mx>0 Then
			d = Sgn(mx-SX/2)*(2^Int(Abs(mx-SX/2)/SX*11)-1)
			df += d
			If df<0 Then df = 0 : d = 0
			If df>NF-SX-1 Then df = NF-SX-1 : d = 0
			SetMouse mx-d
		EndIf

		Flip
	Loop

	ende = 1

	ThreadWait (thr)

	waveInStop (hwi)
	waveInReset (hwi)
	waveInUnPrepareHeader (hwi, @iwhd(0), SizeOf(iwhd(0)))
	DeAllocate (inbuf(0))
	waveInUnPrepareHeader (hwi, @iwhd(1), SizeOf(iwhd(1)))
	DeAllocate (inbuf(1))
	waveInClose (hwi)

	waveOutPause (hwo)
	waveOutReset (hwo)
	waveOutUnprepareHeader (hwo, @whd, SizeOf(whd))
	DeAllocate(buf)
	waveOutClose (hwo)

End Sub
