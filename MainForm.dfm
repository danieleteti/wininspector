object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'WinInspector - The TDWinInfo Legacy Continues'
  ClientHeight = 596
  ClientWidth = 590
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object memoInfo: TMemo
    Left = 0
    Top = 0
    Width = 590
    Height = 577
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
    ExplicitHeight = 422
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 577
    Width = 590
    Height = 19
    Panels = <
      item
        Text = 
          'Copyright 2025 by Daniele Teti - https://github.com/danieleteti/' +
          'wininspector'
        Width = 50
      end>
    ExplicitTop = 422
    ExplicitWidth = 624
  end
  object timerUpdate: TTimer
    Interval = 500
    OnTimer = timerUpdateTimer
    Left = 32
    Top = 32
  end
end
