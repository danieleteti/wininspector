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
  object PanelStatus: TPanel
    Left = 0
    Top = 0
    Width = 700
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblStatus: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 690
      Height = 15
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      Caption = 'Inspecting...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitWidth = 67
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 25
    Width = 700
    Height = 552
    ActivePage = TabSheet1
    Align = alClient
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Window Info'
      object memoWindowInfo: TMemo
        Left = 0
        Top = 0
        Width = 692
        Height = 522
        Align = alClient
        BorderStyle = bsNone
        Ctl3D = False
        DoubleBuffered = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Consolas'
        Font.Style = []
        ParentCtl3D = False
        ParentDoubleBuffered = False
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
        WordWrap = False
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'DLL/Libraries'
      ImageIndex = 1
      object memoDLLInfo: TMemo
        Left = 0
        Top = 0
        Width = 692
        Height = 522
        Align = alClient
        BorderStyle = bsNone
        DoubleBuffered = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Consolas'
        Font.Style = []
        ParentDoubleBuffered = False
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
        WordWrap = False
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
    Interval = 800
    OnTimer = timerUpdateTimer
    Left = 56
    Top = 88
  end
end
