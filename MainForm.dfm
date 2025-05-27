object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'WinInspector - TDWinInfo Reborn'
  ClientHeight = 585
  ClientWidth = 561
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object memoInfo: TMemo
    Left = 0
    Top = 0
    Width = 561
    Height = 585
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      'Avvio in corso...')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitWidth = 624
    ExplicitHeight = 441
  end
  object timerUpdate: TTimer
    Interval = 500
    OnTimer = timerUpdateTimer
    Left = 32
    Top = 32
  end
end
