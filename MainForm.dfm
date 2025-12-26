object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'WinInspector - The TDWinInfo Legacy Continues'
  ClientHeight = 650
  ClientWidth = 920
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
    Width = 920
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblStatus: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 910
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
  object PanelToolbar: TPanel
    Left = 0
    Top = 25
    Width = 920
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 1
    object btnToggleFreeze: TSpeedButton
      Left = 0
      Top = 0
      Width = 150
      Height = 32
      Align = alLeft
      Caption = 'Freeze (Ctrl+F12)'
      Flat = True
      OnClick = btnToggleFreezeClick
      ExplicitLeft = 5
      ExplicitTop = 4
      ExplicitHeight = 24
    end
    object btnCopyWindowInfo: TSpeedButton
      Left = 150
      Top = 0
      Width = 120
      Height = 32
      Align = alLeft
      Caption = 'Copy Window'
      Flat = True
      OnClick = btnCopyWindowInfoClick
    end
    object btnCopyDLLInfo: TSpeedButton
      Left = 270
      Top = 0
      Width = 120
      Height = 32
      Align = alLeft
      Caption = 'Copy DLLs'
      Flat = True
      OnClick = btnCopyDLLInfoClick
    end
    object btnClearHistory: TSpeedButton
      Left = 390
      Top = 0
      Width = 120
      Height = 32
      Align = alLeft
      Caption = 'Clear History'
      Flat = True
      OnClick = btnClearHistoryClick
    end
    object lblColorInfo: TLabel
      Left = 660
      Top = 8
      Width = 189
      Height = 14
      Caption = 'Color: #000000 | RGB(0,0,0)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
    end
    object PanelColorPreview: TPanel
      Left = 630
      Top = 4
      Width = 24
      Height = 24
      BevelOuter = bvLowered
      Color = clBlack
      ParentBackground = False
      TabOrder = 0
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 57
    Width = 920
    Height = 574
    ActivePage = TabSheet1
    Align = alClient
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'Window Info'
      object memoWindowInfo: TMemo
        Left = 0
        Top = 0
        Width = 912
        Height = 544
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
        Width = 912
        Height = 544
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
    object TabSheet3: TTabSheet
      Caption = 'History'
      ImageIndex = 2
      object memoHistory: TMemo
        Left = 0
        Top = 0
        Width = 912
        Height = 544
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
    Top = 631
    Width = 920
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
    Left = 56
    Top = 88
  end
end
