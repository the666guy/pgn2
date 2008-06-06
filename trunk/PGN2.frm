VERSION 5.00
Object = "{648A5603-2C6E-101B-82B6-000000000014}#1.1#0"; "MSCOMM32.OCX"
Object = "{48E59290-9880-11CF-9754-00AA00C00908}#1.0#0"; "msinet.ocx"
Begin VB.Form PGN2Form 
   Caption         =   "Physical GMail Notifier v2"
   ClientHeight    =   3765
   ClientLeft      =   60
   ClientTop       =   450
   ClientWidth     =   4470
   LinkTopic       =   "Form1"
   ScaleHeight     =   3765
   ScaleWidth      =   4470
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton CmdExit 
      Caption         =   "Bye"
      Height          =   375
      Left            =   120
      TabIndex        =   5
      Top             =   3240
      Width           =   4215
   End
   Begin VB.CommandButton CmdSend 
      Caption         =   "Send to serial"
      Height          =   375
      Left            =   1200
      TabIndex        =   4
      Top             =   120
      Width           =   1215
   End
   Begin VB.TextBox TxtSend 
      Height          =   375
      Left            =   120
      TabIndex        =   3
      Text            =   "0"
      Top             =   120
      Width           =   855
   End
   Begin VB.CommandButton CmdReload 
      Caption         =   "Reload settings"
      Height          =   375
      Left            =   2520
      TabIndex        =   2
      Top             =   120
      Width           =   1815
   End
   Begin VB.Timer Timer 
      Interval        =   15000
      Left            =   4440
      Top             =   1320
   End
   Begin InetCtlsObjects.Inet INet 
      Left            =   4440
      Top             =   720
      _ExtentX        =   1005
      _ExtentY        =   1005
      _Version        =   393216
   End
   Begin VB.CommandButton CmdCheck 
      Caption         =   "Manually check mail"
      Height          =   495
      Left            =   120
      TabIndex        =   1
      Top             =   600
      Width           =   4215
   End
   Begin VB.TextBox TxtDebug 
      Height          =   1935
      Left            =   120
      MultiLine       =   -1  'True
      ScrollBars      =   3  'Both
      TabIndex        =   0
      Top             =   1200
      Width           =   4215
   End
   Begin MSCommLib.MSComm Serial 
      Left            =   4440
      Top             =   120
      _ExtentX        =   1005
      _ExtentY        =   1005
      _Version        =   393216
      CommPort        =   11
      DTREnable       =   -1  'True
   End
End
Attribute VB_Name = "PGN2Form"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'   PHYSICAL GMAIL NOTIFIER 2
'   Based on a Jamie Matthews idea
'   (his work: http://www.j4mie.org/2008/02/15/how-to-make-a-physical-gmail-notifier/)
'
'
'
'       L I C E N S E :
'
'   Attribution-Noncommercial-Share Alike 2.5 Italy
'   You are free:
'       * to Share — to copy, distribute and transmit the work
'       * to Remix — to adapt the work
'   Under the following conditions:
'       * Attribution. You must attribute the work in the manner specified
'         by the author or licensor (but not in any way that suggests that
'         they endorse you or your use of the work).
'       * Noncommercial. You may not use this work for commercial purposes.
'       * Share Alike. If you alter, transform, or build upon this work, you
'         may distribute the resulting work only under the same or similar
'         license to this one.
'       * For any reuse or distribution, you must make clear to others the
'         license terms of this work. The best way to do this is with a link
'         to this web page:
'         http://creativecommons.org/licenses/by-nc-sa/2.5/it/deed.en
'       * Any of the above conditions can be waived if you get permission from
'         the copyright holder.
'       * Nothing in this license impairs or restricts the author's moral rights.
'
'       Your fair dealing and other rights are in no way affected by the above.
'       This is a human-readable summary of the Legal Code.
'       Full license: http://creativecommons.org/licenses/by-nc-sa/2.5/it/legalcode
'
'       KTHXBAI

Option Explicit
Dim Username As String
Dim Password As String


Private Sub CmdCheck_Click()
    CheckMail 'Oh, if i click "check mail", the app checks mail. Interesting!
End Sub

Private Sub CmdExit_Click()
    If Serial.PortOpen Then Serial.PortOpen = False 'If we forgot open the serial.. we close it
    Log ("COM closed. Bye")
    Unload Me
    End
End Sub

Private Sub CmdReload_Click()
    LoadSettings 'Reload only re-load settings. Cool.
End Sub


Private Sub CheckMail()
On Error GoTo ERRR 'error handling. a must.
    Log ("Checking mail...")
    Dim STRTemp As String 'in "strtemp" we put the whole web page
    STRTemp = INet.OpenURL("https://" & Username & ":" & Password & "@mail.google.com/gmail/feed/atom")
    STRTemp = Right(STRTemp, Len(STRTemp) - InStr(1, LCase(STRTemp), "fullcount") - 9)
    'ok with this step we delete all the crappy things to the left of our mail count
    STRTemp = Left(STRTemp, InStr(1, STRTemp, "<") - 1)
    'and with this step we remove the right part, too. So, we've got only the mail count!
    Log ("New Mail(s): " & Int(STRTemp))
    If Int(STRTemp) > 9 Then STRTemp = ":" 'if we have more than 9 mails, we tell arduino to keep the led on
    SendToArduino (STRTemp) 'send to arduino the mail count!
Exit Sub
ERRR:
Log ("Error in CheckMail: " & Err.Description)
Resume Next
End Sub


Public Sub SendToArduino(Text As String)
On Error Resume Next
    With Serial
        .DTREnable = False 'oh, some settings.
        .RTSEnable = False 'this, too.
        .PortOpen = True 'we open the port...
        .Output = Text 'we send the string...
        .PortOpen = False 'and we close the port! Remember: always close the door.
    End With
End Sub

Private Sub CmdSend_Click()
On Error Resume Next
    SendToArduino (TxtSend.Text) 'with this great button we send the text to arduino.
    'oh arduino accepts 1 byte at time, so if you send "100" it's like sending "1", "0", "0".
    'so you will see "0" mails. ok?
    Log ("Sent: " & TxtSend.Text)
End Sub


Private Sub Form_Load()
    LoadSettings 'when Physical GMail Notifier v2 starts, it reads the settings!
End Sub

Public Sub LoadSettings()
On Error GoTo ERRR
    
    Log ("Loading settings...")
    
    Dim IniFile As String
    IniFile = App.Path          'c:\blabla
    If Right(App.Path, 1) <> "\" Then IniFile = IniFile & "\"   'c:\blabla\
    IniFile = IniFile & "PGN2.ini"    'c:\blabla\PGN2.ini
    Log ("INI: " & IniFile)
    
    PGN2Form.Visible = IIf(GetFromINI("general", "hidden", 0, IniFile) = 1, 0, 1)
    Log ("Hidden: " & Not (PGN2Form.Visible))
    'visible is the opposite of hidden. hidden is the opposite of visible.
        
    Timer.Enabled = False 'stop the timer!
    Timer.Interval = Int(GetFromINI("general", "interval", 60, IniFile)) * 1000 'set the timer!
    Timer.Enabled = True 'start the timer!
    Log ("Interval: " & Timer.Interval & "ms")
    
    If Serial.PortOpen Then Serial.PortOpen = False 'close and set the com port number
    Serial.CommPort = GetFromINI("general", "comport", 1, IniFile)
    Log ("COM: " & Serial.CommPort)
    
    Username = GetFromINI("general", "username", "", IniFile) 'need comments here?
    Log ("Username: " & Username)
    
    Password = GetFromINI("general", "password", "", IniFile)
    Log ("Password: [hidden]")

    Log ("...Settings loaded")

Exit Sub
ERRR:
Log ("Error in LoadSettings: " & Err.Description)
Resume Next
End Sub



Public Sub Log(Text As String)
On Error GoTo ERRR
    TxtDebug.Text = Text & vbCrLf & TxtDebug.Text
Exit Sub
ERRR:
MsgBox "Error while logging: " & Err.Description
Resume Next
End Sub

Private Sub Timer_Timer()
    CheckMail 'every time the timer tick, we check mail. oh, sorry. the app* checks mail.
End Sub

Private Sub TxtDebug_Change()

End Sub
