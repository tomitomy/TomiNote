unit fImport;

{$mode objfpc}{$H+}

// String constants and string literals require this option
// 字符串常量和字符串字面量需要此选项
{$codepage UTF8}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls;

type

  { TformImport }

  TformImport = class(TForm)
    radgImportFrom      : TRadioGroup;
    radgImportTo        : TRadioGroup;
    chkbIncludeFileExt  : TCheckBox;
    chkbIncludeEntryDir : TCheckBox;

    bttnOK              : TButton;
    bttnCancel          : TButton;
    lablSpace           : TLabel;

    openDlg1            : TOpenDialog;
    seldDlg1            : TSelectDirectoryDialog;

    { Event 事件 }

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormActivate(Sender: TObject);

    procedure bttnOKClick(Sender: TObject);
    procedure bttnCancelClick(Sender: TObject);

    procedure radgImportFromSelectionChanged(Sender: TObject);

  private

    { Function 功能 }

    procedure SetWindowRect;
    procedure SetWindowFont;

    procedure LoadSettings;
    procedure SaveSettings;

    procedure SwapOkCancel;
    procedure OKEvent;
    procedure CancelEvent;

  end;

var
  formImport: TformImport;

implementation

uses
  fMain, uConfig, uTreedb, uResources;

{$R *.lfm}

{ TformImport }

{ ============================================================ }
{ Function 功能                                                }
{ ============================================================ }

procedure TformImport.SetWindowRect;
begin
  if Config.KeepImportFormRect then begin
    BoundsRect := Config.ImportFormRect;
    Position := poDesigned;
  end else begin
    Position := poDesktopCenter;
  end;
end;

procedure TformImport.SetWindowFont;
begin
  if Screen.Fonts.IndexOf(Config.WindowFontName) <> -1 then
    Font.Name := Config.WindowFontName
  else
    Font.Name := 'default';
  Font.Size := Config.WindowFontSize;
end;

procedure TformImport.LoadSettings;
begin
  chkbIncludeFileExt.Checked  := Config.ImportFileExt;
  chkbIncludeEntryDir.Checked := Config.ImportRootDir;

  // These code will change the state of other controls, so put at the last.
  // 这些代码会改变其它控件的状态，所以放在最后
  radgImportFrom.ItemIndex := Config.ImportFrom;
  radgImportTo.ItemIndex := Config.ImportTo;
end;

procedure TformImport.SaveSettings;
begin
  Config.ImportFrom    := radgImportFrom.ItemIndex;
  Config.ImportTo      := radgImportTo.ItemIndex;
  Config.ImportFileExt := chkbIncludeFileExt.Checked;
  Config.ImportRootDir := chkbIncludeEntryDir.Checked;
end;

procedure TformImport.SwapOkCancel;
begin
  if Config.SwapOKCancel then begin
    bttnOK.Caption     := Res_CaptionCancel;
    bttnOK.Cancel      := True;

    bttnCancel.Caption := Res_CaptionImport;
  end else begin
    bttnOK.Caption     := Res_CaptionImport;

    bttnCancel.Caption := Res_CaptionCancel;
    bttnCancel.Cancel  := True;
  end;
end;

procedure TformImport.OKEvent;
var
  FromPath: string;
  IncludeEntryDir: boolean;
  IncludeFileExt: boolean;
  Mode: TNodeAttachMode;
begin
  // Prevent user from closing the database after opening the Import dialog
  // 防止用户打开导入对话框后关闭数据库
  if not formMain.DBActive then begin
    Close;
    Exit;
  end;

  Mode := naInsert;
  case radgImportTo.ItemIndex of
    1: Mode := naAddChildFirst;
    2: Mode := naInsertBehind;
    3: Mode := naAddChild;
  end;

  IncludeEntryDir := chkbIncludeEntryDir.Enabled and chkbIncludeEntryDir.Checked;
  IncludeFileExt  := chkbIncludeFileExt.Enabled and chkbIncludeFileExt.Checked;

  if Config.LastImportDir = '' then
    Config.LastImportDir := ConcatPaths([AppDir, 'data']);
  openDlg1.InitialDir := Config.LastImportDir;
  seldDlg1.InitialDir := Config.LastImportDir;

  case radgImportFrom.ItemIndex of
    0: begin
      openDlg1.Filter := Res_DialogFilterTXT;
      if not openDlg1.Execute then Exit;
      Hide;
      // Prevent user from modifying the note after opening the Import dialog.
      // 防止用户在打开导入对话框后又修改笔记内容
      formMain.SubmitNote;
      FromPath := openDlg1.FileName;
      formMain.ImportFile(FromPath, IncludeFileExt, Mode).Selected := True;
      Config.LastImportDir := ExtractFileDir(openDlg1.FileName);
      Close;
    end;
    1: begin
      if not seldDlg1.Execute then Exit;
      Hide;
      // Prevent user from modifying the note after opening the Import dialog.
      // 防止用户在打开导入对话框后又修改笔记内容
      formMain.SubmitNote;
      FromPath := seldDlg1.FileName;
      formMain.ImportDir(FromPath, IncludeEntryDir, IncludeFileExt, Mode).Selected := True;
      Config.LastImportDir := seldDlg1.FileName;
      Close;
    end;
    2: begin
      openDlg1.Filter := Res_DialogFilterTDB;
      if not openDlg1.Execute then Exit;
      Hide;
      // Prevent user from modifying the note after opening the Import dialog.
      // 防止用户在打开导入对话框后又修改笔记内容
      formMain.SubmitNote;
      FromPath := openDlg1.FileName;
      formMain.ImportDB(FromPath, Mode);
      Config.LastImportDir := ExtractFileDir(openDlg1.FileName);
      Close;
    end;
  end;
end;

procedure TformImport.CancelEvent;
begin
  Close;
end;

{ ============================================================ }
{ Event 事件                                                   }
{ ============================================================ }

procedure TformImport.FormCreate(Sender: TObject);
begin
  SetWindowRect;
  SetWindowFont;

  LoadSettings;
  SwapOkCancel;
end;

procedure TformImport.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Config.ImportFormRect := BoundsRect;
  SaveSettings;
  CloseAction := caFree;
  formImport := nil;
end;

procedure TformImport.FormActivate(Sender: TObject);
begin
  formMain.actnCutUpdate(Sender);
end;

procedure TformImport.bttnOKClick(Sender: TObject);
begin
  if Config.SwapOKCancel then CancelEvent else OKEvent;
end;

procedure TformImport.bttnCancelClick(Sender: TObject);
begin
  if Config.SwapOKCancel then OKEvent else CancelEvent;
end;

procedure TformImport.radgImportFromSelectionChanged(Sender: TObject);
begin
  chkbIncludeFileExt.Enabled := radgImportFrom.ItemIndex <> 2;
  chkbIncludeEntryDir.Enabled := radgImportFrom.ItemIndex = 1;
end;

end.
