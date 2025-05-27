unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, System.StrUtils, Winapi.PsApi, Vcl.ComCtrls,
  System.DateUtils;

type
  TfrmMain = class(TForm)
    memoInfo: TMemo;
    timerUpdate: TTimer;
    StatusBar1: TStatusBar;
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

  // Setup del memo
  memoInfo.Clear;
  memoInfo.Font.Name := 'Consolas';
  memoInfo.Font.Size := 12;  // Font più grande
  memoInfo.ScrollBars := ssVertical;
  memoInfo.ReadOnly := True;
  memoInfo.Align := alClient;

  // Setup del timer
  timerUpdate.Interval := 500; // 500ms come richiesto
  timerUpdate.Enabled := True;

  // Dimensioni form
  Width := 600;
  Height := 800;
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
    // Evita flickering durante l'aggiornamento
    memoInfo.Lines.BeginUpdate;
    try
      // Pulisci esplicitamente il memo prima di aggiornare
      memoInfo.Lines.Clear;

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

      // Sezione monitor alla fine
      Info.Add('');
      Info.Add('🖥️  MONITOR:');

      // Gestisci correttamente le righe multiple del monitor
      MonitorInfoLines.Text := GetMonitorInfo(MousePos);
      for i := 0 to MonitorInfoLines.Count - 1 do
        Info.Add('   ' + MonitorInfoLines[i]);

      memoInfo.Lines.Assign(Info);
    finally
      memoInfo.Lines.EndUpdate;
    end;
  finally
    Info.Free;
    MonitorInfoLines.Free;
  end;
end;

end.
