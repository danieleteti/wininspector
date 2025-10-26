unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, System.StrUtils, Winapi.PsApi, Vcl.ComCtrls,
  System.DateUtils, System.Rtti, System.TypInfo, Winapi.TlHelp32,
  System.Generics.Collections;

type
  TfrmMain = class(TForm)
    timerUpdate: TTimer;
    StatusBar1: TStatusBar;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    memoWindowInfo: TMemo;
    memoDLLInfo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure timerUpdateTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    function GetWindowClassName(Handle: HWND): string;
    function GetWindowTitle(Handle: HWND): string;
    function GetProcessInfo(Handle: HWND): string;
    function GetWindowStyleInfo(Handle: HWND): string;
    function GetWindowDimensions(Handle: HWND): string;
    function GetMonitorInfo(MousePos: TPoint): string;
    function IsWindowVisible(Handle: HWND): Boolean;
    function GetVCLComponentInfo(Handle: HWND): string;
    function GetDLLInfo(Handle: HWND): string;
    function IsDelphiVCLWindow(const ClassName: string): Boolean;
    function ExtractDelphiVersionFromBPL(const BPLName: string): string;
    function GetFileVersionInfo(const FileName: string): string;
    function GetRealModulePath(ProcessHandle: THandle; ModuleHandle: HMODULE): string;
    procedure UpdateWindowInfo;
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption := 'WinInspector - The TDWinInfo Legacy Continues';
  StatusBar1.Panels[0].Text := 'Copyright 2001 - ' + YearOf(Date).ToString +
    ' by Daniele Teti - https://github.com/danieleteti/wininspector';

  // Setup tabs
  TabSheet1.Caption := 'Window Info';
  TabSheet2.Caption := 'DLL/Libraries';

  // Setup memo Window Info
  memoWindowInfo.Clear;
  memoWindowInfo.Font.Name := 'Consolas';
  memoWindowInfo.Font.Size := 10;
  memoWindowInfo.ScrollBars := ssVertical;
  memoWindowInfo.ReadOnly := True;
  memoWindowInfo.Align := alClient;
  memoWindowInfo.Parent := TabSheet1;

  // Setup memo DLL Info
  memoDLLInfo.Clear;
  memoDLLInfo.Font.Name := 'Consolas';
  memoDLLInfo.Font.Size := 10;
  memoDLLInfo.ScrollBars := ssVertical;
  memoDLLInfo.ReadOnly := True;
  memoDLLInfo.Align := alClient;
  memoDLLInfo.Parent := TabSheet2;

  // Setup del timer
  timerUpdate.Interval := 500; // 500ms come richiesto
  timerUpdate.Enabled := True;

  // Dimensioni form più ragionevoli
  Width := 700;
  Height := 600;
  Position := poScreenCenter;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  timerUpdate.Enabled := False;
end;

procedure TfrmMain.timerUpdateTimer(Sender: TObject);
begin
  UpdateWindowInfo;
end;

function TfrmMain.GetWindowClassName(Handle: HWND): string;
var
  Buffer: PChar;
  Len: Integer;
begin
  Result := '<Unknown>';

  if (Handle = 0) or not IsWindow(Handle) then
  begin
    Result := '<Invalid handle>';
    Exit;
  end;

  try
    // Alloca buffer di dimensione fissa per il nome classe
    GetMem(Buffer, 256 * SizeOf(Char));
    try
      FillChar(Buffer^, 256 * SizeOf(Char), 0);

      Len := GetClassName(Handle, Buffer, 256);
      if Len > 0 then
        Result := string(Buffer);
    finally
      FreeMem(Buffer);
    end;
  except
    Result := '<Error reading class>';
  end;
end;

function TfrmMain.GetWindowTitle(Handle: HWND): string;
var
  Len: Integer;
  Buffer: PChar;
begin
  Result := '<No title>';

  if (Handle = 0) or not IsWindow(Handle) then
  begin
    Result := '<Invalid handle>';
    Exit;
  end;

  try
    // Ottieni prima la lunghezza necessaria
    Len := GetWindowTextLength(Handle);
    if Len <= 0 then
      Exit;

    // Alloca buffer dinamico
    GetMem(Buffer, (Len + 1) * SizeOf(Char));
    try
      FillChar(Buffer^, (Len + 1) * SizeOf(Char), 0);

      // Ottieni il testo
      if GetWindowText(Handle, Buffer, Len + 1) > 0 then
        Result := string(Buffer);
    finally
      FreeMem(Buffer);
    end;
  except
    Result := '<Error reading title>';
  end;
end;

function TfrmMain.GetProcessInfo(Handle: HWND): string;
var
  ProcessId, ThreadId: DWORD;
  ProcessHandle: THandle;
  Buffer: PChar;
  Len: DWORD;
  FileName: string;
begin
  if (Handle = 0) or not IsWindow(Handle) then
  begin
    Result := 'Invalid handle';
    Exit;
  end;

  ThreadId := GetWindowThreadProcessId(Handle, @ProcessId);

  try
    ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, ProcessId);
    try
      if ProcessHandle <> 0 then
      begin
        GetMem(Buffer, (MAX_PATH + 1) * SizeOf(Char));
        try
          FillChar(Buffer^, (MAX_PATH + 1) * SizeOf(Char), 0);

          Len := GetModuleFileNameEx(ProcessHandle, 0, Buffer, MAX_PATH);
          if Len > 0 then
          begin
            FileName := string(Buffer);
            Result := Format('PID: %d, TID: %d, EXE: %s', [ProcessId, ThreadId, ExtractFileName(FileName)]);
          end
          else
            Result := Format('PID: %d, TID: %d, EXE: <Not accessible>', [ProcessId, ThreadId]);
        finally
          FreeMem(Buffer);
        end;
      end
      else
        Result := Format('PID: %d, TID: %d, EXE: <Not accessible>', [ProcessId, ThreadId]);
    finally
      if ProcessHandle <> 0 then
        CloseHandle(ProcessHandle);
    end;
  except
    Result := Format('PID: %d, TID: %d, EXE: <Error>', [ProcessId, ThreadId]);
  end;
end;

function TfrmMain.GetWindowStyleInfo(Handle: HWND): string;
var
  Style, ExStyle: DWORD;
  StyleInfo: TStringList;
begin
  // Controlla se l'handle è valido
  if (Handle = 0) or not IsWindow(Handle) then
  begin
    Result := 'Invalid window handle';
    Exit;
  end;

  try
    Style := GetWindowLongPtr(Handle, GWL_STYLE);
    ExStyle := GetWindowLongPtr(Handle, GWL_EXSTYLE);
  except
    Result := 'Error reading window styles';
    Exit;
  end;

  StyleInfo := TStringList.Create;
  try
    // Stili principali
    if (Style and WS_VISIBLE) <> 0 then StyleInfo.Add('VISIBLE');
    if (Style and WS_CHILD) <> 0 then StyleInfo.Add('CHILD');
    if (Style and WS_POPUP) <> 0 then StyleInfo.Add('POPUP');
    if (Style and WS_MINIMIZE) <> 0 then StyleInfo.Add('MINIMIZED');
    if (Style and WS_MAXIMIZE) <> 0 then StyleInfo.Add('MAXIMIZED');
    if (Style and WS_DISABLED) <> 0 then StyleInfo.Add('DISABLED');

    // Stili estesi
    if (ExStyle and WS_EX_TOPMOST) <> 0 then StyleInfo.Add('TOPMOST');
    if (ExStyle and WS_EX_TOOLWINDOW) <> 0 then StyleInfo.Add('TOOLWINDOW');
    if (ExStyle and WS_EX_TRANSPARENT) <> 0 then StyleInfo.Add('TRANSPARENT');
    if (ExStyle and WS_EX_LAYERED) <> 0 then StyleInfo.Add('LAYERED');

    if StyleInfo.Count > 0 then
      Result := Format('Style: 0x%x, ExStyle: 0x%x [%s]', [Style, ExStyle, StyleInfo.CommaText])
    else
      Result := Format('Style: 0x%x, ExStyle: 0x%x', [Style, ExStyle]);
  finally
    StyleInfo.Free;
  end;
end;

function TfrmMain.GetWindowDimensions(Handle: HWND): string;
var
  Rect: TRect;
begin
  if (Handle = 0) or not IsWindow(Handle) then
  begin
    Result := 'Invalid handle';
    Exit;
  end;

  if GetWindowRect(Handle, Rect) then
    Result := Format('Pos: (%d,%d), Size: %dx%d',
      [Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top])
  else
    Result := 'Dimensions not available';
end;

function TfrmMain.GetMonitorInfo(MousePos: TPoint): string;
var
  ScreenWidth, ScreenHeight: Integer;
  WorkAreaRect: TRect;
  MonitorCount: Integer;
  InfoList: TStringList;
begin
  InfoList := TStringList.Create;
  try
    // Informazioni base schermo
    ScreenWidth := GetSystemMetrics(SM_CXSCREEN);
    ScreenHeight := GetSystemMetrics(SM_CYSCREEN);
    MonitorCount := GetSystemMetrics(SM_CMONITORS);

    // Area di lavoro (esclude taskbar)
    SystemParametersInfo(SPI_GETWORKAREA, 0, @WorkAreaRect, 0);

    InfoList.Add(Format('Screen resolution: %dx%d', [ScreenWidth, ScreenHeight]));
    InfoList.Add(Format('Number of monitors: %d', [MonitorCount]));
    InfoList.Add(Format('Work area: (%d,%d) - (%d,%d)', [
      WorkAreaRect.Left, WorkAreaRect.Top,
      WorkAreaRect.Right, WorkAreaRect.Bottom]));
    InfoList.Add(Format('Mouse on screen: (%d,%d)', [MousePos.X, MousePos.Y]));

    // Informazioni aggiuntive sul display se multi-monitor
    if MonitorCount > 1 then
    begin
      InfoList.Add(Format('Virtual desktop: %dx%d', [
        GetSystemMetrics(SM_CXVIRTUALSCREEN),
        GetSystemMetrics(SM_CYVIRTUALSCREEN)]));
      InfoList.Add(Format('Desktop offset: (%d,%d)', [
        GetSystemMetrics(SM_XVIRTUALSCREEN),
        GetSystemMetrics(SM_YVIRTUALSCREEN)]));
    end;

    Result := InfoList.Text;
  finally
    InfoList.Free;
  end;
end;

function TfrmMain.IsWindowVisible(Handle: HWND): Boolean;
begin
  if (Handle = 0) or not IsWindow(Handle) then
    Result := False
  else
    Result := Winapi.Windows.IsWindowVisible(Handle);
end;

function TfrmMain.IsDelphiVCLWindow(const ClassName: string): Boolean;
begin
  // Identifica classi VCL comuni di Delphi
  Result := (Pos('T', ClassName) = 1) and
            ((Pos('Form', ClassName) > 0) or
             (Pos('Button', ClassName) > 0) or
             (Pos('Edit', ClassName) > 0) or
             (Pos('Panel', ClassName) > 0) or
             (Pos('Label', ClassName) > 0) or
             (Pos('Memo', ClassName) > 0) or
             (Pos('Grid', ClassName) > 0) or
             (Pos('List', ClassName) > 0) or
             (Pos('Tree', ClassName) > 0) or
             (Pos('Tab', ClassName) > 0) or
             (Pos('Combo', ClassName) > 0) or
             (Pos('Check', ClassName) > 0) or
             (Pos('Radio', ClassName) > 0) or
             (Pos('Group', ClassName) > 0) or
             (Pos('Page', ClassName) > 0) or
             (Pos('Splitter', ClassName) > 0) or
             (Pos('Status', ClassName) > 0) or
             (Pos('Tool', ClassName) > 0) or
             (Pos('Action', ClassName) > 0));
end;

function TfrmMain.GetVCLComponentInfo(Handle: HWND): string;
var
  ClassName: string;
  Info: TStringList;
  WindowText: string;
  Style: DWORD;
  IsEnabled: Boolean;
  IsReadOnly: Boolean;
  ParentClassName: string;
  ParentHandle: HWND;
begin
  Result := '';
  ClassName := GetWindowClassName(Handle);

  if not IsDelphiVCLWindow(ClassName) then
    Exit;

  Info := TStringList.Create;
  try
    Info.Add('🎨  VCL/DELPHI COMPONENT DETECTED:');
    Info.Add(Format('   Component Type: %s', [ClassName]));

    // Ottieni testo del controllo
    WindowText := GetWindowTitle(Handle);
    if (WindowText <> '<No title>') and (WindowText <> '<Invalid handle>') then
      Info.Add(Format('   Text/Caption: "%s"', [WindowText]));

    // Analizza gli stili per proprietà comuni
    try
      Style := GetWindowLongPtr(Handle, GWL_STYLE);
      if Style = 0 then
      begin
        var Err := GetLastError;
        if Err <> 0 then
        begin
          Info.Add('   (Cannot read window styles - access denied)');
          Result := Info.Text;
          Exit;
        end;
      end;

      IsEnabled := (Style and WS_DISABLED) = 0;
      Info.Add(Format('   Enabled: %s', [BoolToStr(IsEnabled, True)]));
    except
      Info.Add('   (Error reading window properties)');
      Result := Info.Text;
      Exit;
    end;

    // Identifica proprietà specifiche per tipo
    if (Pos('Edit', ClassName) > 0) or (Pos('Memo', ClassName) > 0) then
    begin
      IsReadOnly := (Style and ES_READONLY) <> 0;
      Info.Add(Format('   ReadOnly: %s', [BoolToStr(IsReadOnly, True)]));

      if (Style and ES_MULTILINE) <> 0 then
        Info.Add('   MultiLine: True');

      if (Style and ES_PASSWORD) <> 0 then
        Info.Add('   PasswordChar: ****');
    end
    else if Pos('Button', ClassName) > 0 then
    begin
      if (Style and BS_DEFPUSHBUTTON) <> 0 then
        Info.Add('   Default: True');
    end
    else if Pos('Check', ClassName) > 0 then
    begin
      try
        var CheckState := SendMessage(Handle, BM_GETCHECK, 0, 0);
        case CheckState of
          BST_UNCHECKED: Info.Add('   Checked: False');
          BST_CHECKED: Info.Add('   Checked: True');
          BST_INDETERMINATE: Info.Add('   State: Indeterminate');
        end;
      except
        // Ignora errori SendMessage
      end;
    end
    else if Pos('Radio', ClassName) > 0 then
    begin
      try
        var RadioState := SendMessage(Handle, BM_GETCHECK, 0, 0);
        Info.Add(Format('   Checked: %s', [BoolToStr(RadioState = BST_CHECKED, True)]));
      except
        // Ignora errori SendMessage
      end;
    end
    else if Pos('List', ClassName) > 0 then
    begin
      try
        var ItemCount := SendMessage(Handle, LB_GETCOUNT, 0, 0);
        if ItemCount >= 0 then
        begin
          Info.Add(Format('   Items.Count: %d', [ItemCount]));
          var SelIndex := SendMessage(Handle, LB_GETCURSEL, 0, 0);
          if SelIndex >= 0 then
            Info.Add(Format('   ItemIndex: %d', [SelIndex]));
        end;
      except
        // Ignora errori SendMessage
      end;
    end
    else if Pos('Combo', ClassName) > 0 then
    begin
      try
        var ItemCount := SendMessage(Handle, CB_GETCOUNT, 0, 0);
        if ItemCount >= 0 then
        begin
          Info.Add(Format('   Items.Count: %d', [ItemCount]));
          var SelIndex := SendMessage(Handle, CB_GETCURSEL, 0, 0);
          if SelIndex >= 0 then
            Info.Add(Format('   ItemIndex: %d', [SelIndex]));
        end;
      except
        // Ignora errori SendMessage
      end;
    end;

    // Analizza il parent per contesto
    ParentHandle := GetParent(Handle);
    if ParentHandle <> 0 then
    begin
      ParentClassName := GetWindowClassName(ParentHandle);
      if Pos('Panel', ParentClassName) > 0 then
        Info.Add(Format('   Parent Container: %s', [ParentClassName]))
      else if Pos('Group', ParentClassName) > 0 then
        Info.Add(Format('   Parent Container: %s (GroupBox)', [ParentClassName]));
    end;

    // Informazioni aggiuntive
    Info.Add('   ℹ️  VCL component properties extracted via Windows API');

    Result := Info.Text;
  finally
    Info.Free;
  end;
end;

function TfrmMain.ExtractDelphiVersionFromBPL(const BPLName: string): string;
var
  FileName: string;
begin
  FileName := UpperCase(ExtractFileName(BPLName));

  // Pattern: rtl280.bpl = Delphi 11, vcl270.bpl = Delphi 10.4, etc.
  if Pos('370', FileName) > 0 then Result := 'Delphi 13 Florence'
  else if Pos('290', FileName) > 0 then Result := 'Delphi 12 Athens'
  else if Pos('280', FileName) > 0 then Result := 'Delphi 11 Alexandria'
  else if Pos('270', FileName) > 0 then Result := 'Delphi 10.4 Sydney'
  else if Pos('260', FileName) > 0 then Result := 'Delphi 10.3 Rio'
  else if Pos('250', FileName) > 0 then Result := 'Delphi 10.2 Tokyo'
  else if Pos('240', FileName) > 0 then Result := 'Delphi 10.1 Berlin'
  else if Pos('230', FileName) > 0 then Result := 'Delphi 10 Seattle'
  else if Pos('220', FileName) > 0 then Result := 'Delphi XE8'
  else if Pos('210', FileName) > 0 then Result := 'Delphi XE7'
  else if Pos('200', FileName) > 0 then Result := 'Delphi XE6'
  else if Pos('190', FileName) > 0 then Result := 'Delphi XE5'
  else if Pos('180', FileName) > 0 then Result := 'Delphi XE4'
  else if Pos('170', FileName) > 0 then Result := 'Delphi XE3'
  else if Pos('160', FileName) > 0 then Result := 'Delphi XE2'
  else Result := 'Unknown Delphi Version';
end;

function TfrmMain.GetFileVersionInfo(const FileName: string): string;
var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
begin
  Result := '';
  if not FileExists(FileName) then
    Exit;

  VerInfoSize := GetFileVersionInfoSize(PChar(FileName), Dummy);
  if VerInfoSize > 0 then
  begin
    GetMem(VerInfo, VerInfoSize);
    try
      if Winapi.Windows.GetFileVersionInfo(PChar(FileName), 0, VerInfoSize, VerInfo) then
      begin
        if VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize) then
        begin
          Result := Format('%d.%d.%d.%d',
            [HiWord(VerValue^.dwFileVersionMS), LoWord(VerValue^.dwFileVersionMS),
             HiWord(VerValue^.dwFileVersionLS), LoWord(VerValue^.dwFileVersionLS)]);
        end;
      end;
    finally
      FreeMem(VerInfo);
    end;
  end;
end;

function TfrmMain.GetRealModulePath(ProcessHandle: THandle; ModuleHandle: HMODULE): string;
var
  Buffer: array[0..MAX_PATH] of Char;
  Len: DWORD;
begin
  Result := '';
  if ProcessHandle = 0 then
    Exit;

  try
    FillChar(Buffer, SizeOf(Buffer), 0);

    // GetModuleFileNameEx ritorna il path reale, non quello rediretto
    Len := GetModuleFileNameEx(ProcessHandle, ModuleHandle, Buffer, MAX_PATH);
    if Len > 0 then
      Result := string(Buffer);
  except
    Result := ''; // In caso di errore, ritorna stringa vuota
  end;
end;

function TfrmMain.GetDLLInfo(Handle: HWND): string;
var
  ProcessId: DWORD;
  ProcessHandle: THandle;
  SnapshotHandle: THandle;
  ModuleEntry: TModuleEntry32;
  Info: TStringList;
  DelphiVersion: string;
  VersionInfo: string;
  BPLList: TStringList;
  DLLList: TStringList;
  RealPath: string;
begin
  Result := '';

  if (Handle = 0) or not IsWindow(Handle) then
    Exit;

  GetWindowThreadProcessId(Handle, @ProcessId);

  // Apri il processo per GetModuleFileNameEx
  ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, ProcessId);
  if ProcessHandle = 0 then
    Exit;

  Info := TStringList.Create;
  BPLList := TStringList.Create;
  DLLList := TStringList.Create;
  try
    Info.Add('📚  LOADED LIBRARIES:');

    // Crea snapshot dei moduli del processo
    SnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, ProcessId);
    if SnapshotHandle <> INVALID_HANDLE_VALUE then
    begin
      try
        ModuleEntry.dwSize := SizeOf(TModuleEntry32);

        if Module32First(SnapshotHandle, ModuleEntry) then
        begin
          repeat
            try
              var ModuleName := UpperCase(ModuleEntry.szModule);
              var ModuleExt := UpperCase(ExtractFileExt(ModuleEntry.szExePath));

              // Ottieni il path REALE (non rediretto da WOW64)
              RealPath := GetRealModulePath(ProcessHandle, ModuleEntry.hModule);
              if RealPath = '' then
                RealPath := string(ModuleEntry.szExePath); // Fallback al path originale

              // Identifica BPL Delphi o exe principale Delphi
              var IsBPL := (ModuleExt = '.BPL') or
                           (Pos('RTL', ModuleName) > 0) or
                           (Pos('VCL', ModuleName) > 0) or
                           (Pos('FMX', ModuleName) > 0);

              if IsBPL then
              begin
                DelphiVersion := ExtractDelphiVersionFromBPL(ModuleEntry.szModule);
                VersionInfo := GetFileVersionInfo(RealPath);
                if VersionInfo <> '' then
                  BPLList.Add(Format('%s (v%s) - %s', [RealPath, VersionInfo, DelphiVersion]))
                else
                  BPLList.Add(Format('%s - %s', [RealPath, DelphiVersion]));
              end
              else
              begin
                // DLL normali - path completo REALE
                VersionInfo := GetFileVersionInfo(RealPath);
                if VersionInfo <> '' then
                  DLLList.Add(Format('%s (v%s)', [RealPath, VersionInfo]))
                else
                  DLLList.Add(Format('%s', [RealPath]));
              end;
            except
              // Ignora errori su singoli moduli e continua con il prossimo
            end;
          until not Module32Next(SnapshotHandle, ModuleEntry);
        end;
      finally
        CloseHandle(SnapshotHandle);
      end;
    end;

    // Ordina le liste alfabeticamente per path completo
    BPLList.Sort;
    DLLList.Sort;

    // Output BPL packages
    if BPLList.Count > 0 then
    begin
      Info.Add('🎯 Delphi/C++Builder Packages (BPL):');
      Info.Add('');
      Info.AddStrings(BPLList);
      Info.Add('');
    end;

    // Output DLL complete
    if DLLList.Count > 0 then
    begin
      Info.Add(Format('📦 Standard DLLs (%d total):', [DLLList.Count]));
      Info.Add('');
      Info.AddStrings(DLLList);
    end;

    Result := Info.Text;
  finally
    Info.Free;
    BPLList.Free;
    DLLList.Free;
    if ProcessHandle <> 0 then
      CloseHandle(ProcessHandle);
  end;
end;

procedure TfrmMain.UpdateWindowInfo;
var
  MousePos: TPoint;
  WindowHandle, ParentHandle, RootHandle: HWND;
  Info: TStringList;
  IsResponding: Boolean;
  MonitorInfoLines: TStringList;
  i: Integer;
begin
  // Ottieni posizione mouse
  GetCursorPos(MousePos);

  // Trova finestra sotto il cursore
  WindowHandle := WindowFromPoint(MousePos);

  Info := TStringList.Create;
  MonitorInfoLines := TStringList.Create;
  try
    // Evita flickering durante l'aggiornamento - TAB 1: Window Info
    memoWindowInfo.Lines.BeginUpdate;
    try
      // Pulisci esplicitamente il memo prima di aggiornare
      memoWindowInfo.Lines.Clear;

      Info.Add(Format('🖱️  MOUSE POSITION: (%d, %d)', [MousePos.X, MousePos.Y]));
      Info.Add('');

      if WindowHandle <> 0 then
      begin
        Info.Add('🪟  CURRENT WINDOW:');
        Info.Add(Format('   Handle: 0x%x (%d)', [WindowHandle, WindowHandle]));
        Info.Add(Format('   Class: %s', [GetWindowClassName(WindowHandle)]));
        Info.Add(Format('   Title: %s', [GetWindowTitle(WindowHandle)]));
        Info.Add(Format('   %s', [GetWindowDimensions(WindowHandle)]));
        Info.Add(Format('   Visible: %s', [IfThen(IsWindowVisible(WindowHandle), 'Yes', 'No')]));
        Info.Add(Format('   %s', [GetProcessInfo(WindowHandle)]));
        Info.Add(Format('   %s', [GetWindowStyleInfo(WindowHandle)]));

        // Verifica se la finestra risponde
        IsResponding := SendMessageTimeout(WindowHandle, WM_NULL, 0, 0,
          SMTO_ABORTIFHUNG or SMTO_BLOCK, 100, nil) <> 0;
        Info.Add(Format('   Responds: %s', [IfThen(IsResponding, 'Yes', 'No')]));

        // Informazioni sulla finestra parent
        ParentHandle := GetParent(WindowHandle);
        if ParentHandle <> 0 then
        begin
          Info.Add('');
          Info.Add('👨‍👦  PARENT WINDOW:');
          Info.Add(Format('   Handle: 0x%x (%d)', [ParentHandle, ParentHandle]));
          Info.Add(Format('   Class: %s', [GetWindowClassName(ParentHandle)]));
          Info.Add(Format('   Title: %s', [GetWindowTitle(ParentHandle)]));
        end;

        // Finestra root (top-level)
        RootHandle := GetAncestor(WindowHandle, GA_ROOT);
        if (RootHandle <> 0) and (RootHandle <> WindowHandle) and (RootHandle <> ParentHandle) then
        begin
          Info.Add('');
          Info.Add('🏠  ROOT WINDOW:');
          Info.Add(Format('   Handle: 0x%x (%d)', [RootHandle, RootHandle]));
          Info.Add(Format('   Class: %s', [GetWindowClassName(RootHandle)]));
          Info.Add(Format('   Title: %s', [GetWindowTitle(RootHandle)]));
        end;
      end
      else
      begin
        Info.Add('❌ No window found under cursor');
      end;

      // Sezione VCL Component Inspector
      var VCLInfo := GetVCLComponentInfo(WindowHandle);
      if VCLInfo <> '' then
      begin
        Info.Add('');
        Info.Add(VCLInfo);
      end;

      // Sezione monitor alla fine
      Info.Add('');
      Info.Add('🖥️  MONITOR:');

      // Gestisci correttamente le righe multiple del monitor
      MonitorInfoLines.Text := GetMonitorInfo(MousePos);
      for i := 0 to MonitorInfoLines.Count - 1 do
        Info.Add('   ' + MonitorInfoLines[i]);

      memoWindowInfo.Lines.Assign(Info);
    finally
      memoWindowInfo.Lines.EndUpdate;
    end;

    // TAB 2: DLL/Library Info - aggiorna SOLO se il tab è attivo
    if (WindowHandle <> 0) and (PageControl1.ActivePage = TabSheet2) then
    begin
      memoDLLInfo.Lines.BeginUpdate;
      try
        memoDLLInfo.Lines.Clear;
        var DLLInfo := GetDLLInfo(WindowHandle);
        if DLLInfo <> '' then
          memoDLLInfo.Lines.Text := DLLInfo
        else
          memoDLLInfo.Lines.Text := 'No DLL information available';
      finally
        memoDLLInfo.Lines.EndUpdate;
      end;
    end;
  finally
    Info.Free;
    MonitorInfoLines.Free;
  end;
end;

end.
