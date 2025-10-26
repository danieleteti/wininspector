object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'WinInspector - The TDWinInfo Legacy Continues'
  ClientHeight = 596
  ClientWidth = 700
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
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 700
    Height = 577
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Window Info'
      object memoWindowInfo: TMemo
        Left = 0
        Top = 0
        Width = 692
        Height = 547
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'DLL/Libraries'
      ImageIndex = 1
      object memoDLLInfo: TMemo
        Left = 0
        Top = 0
        Width = 692
        Height = 547
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 577
    Width = 700
    Height = 19
    Panels = <
      item
        Text =
          'Copyright 2025 by Daniele Teti - https://github.com/danieleteti/' +
          'wininspector'
        Width = 50
      end>
  end
  object timerUpdate: TTimer
    Interval = 500
    OnTimer = timerUpdateTimer
    Left = 32
    Top = 32
  end
end
