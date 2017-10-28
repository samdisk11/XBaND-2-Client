<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmMain
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Me.tmrFlash = New System.Windows.Forms.Timer(Me.components)
        Me.tmrMusic = New System.Windows.Forms.Timer(Me.components)
        Me.tmrMatch = New System.Windows.Forms.Timer(Me.components)
        Me.SuspendLayout()
        '
        'tmrFlash
        '
        Me.tmrFlash.Interval = 150
        '
        'tmrMusic
        '
        Me.tmrMusic.Interval = 300
        '
        'tmrMatch
        '
        Me.tmrMatch.Interval = 30000
        '
        'frmMain
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackgroundImage = Global.Xband_2.My.Resources.Resources.xback
        Me.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Stretch
        Me.ClientSize = New System.Drawing.Size(576, 494)
        Me.DoubleBuffered = True
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog
        Me.MaximizeBox = False
        Me.Name = "frmMain"
        Me.Text = "Xband 2.0"
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents tmrFlash As System.Windows.Forms.Timer
    Friend WithEvents tmrMusic As System.Windows.Forms.Timer
    Friend WithEvents tmrMatch As System.Windows.Forms.Timer

End Class
