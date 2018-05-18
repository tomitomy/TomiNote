unit fExport;

{$mode objfpc}{$H+}

// String constants and string literals require this option
// 字符串常量和字符串字面量需要此选项
{$codepage UTF8}

interface

uses
  Classes, SysUtils, Forms, ComCtrls, ExtCtrls, StdCtrls, Dialogs;

type

  { TformExport }

  TformExport = class(TForm)
    radgExportTo     : TRadioGroup;
    radgExportFrom   : TRadioGroup;

    chkbAddFileExt   : TCheckBox;
    chkbAddSeparator : TCheckBox;
    editFileExt      : TEdit;
    editSeparator    : TEdit;

    bttnOK           : TButton;
    bttnCancel       : TButton;
    lablSpace        : TLabel;

    saveDlg1         : TSaveDialog;
    seldDlg1         : TSelectDirectoryDialog;

    { Event 事件 }

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormActivate(Sender: TObject);

    procedure bttnOKClick(Sender: TObject);
    procedure bttnCancelClick(Sender: TObject);

    procedure radgExportToSelectionChanged(Sender: TObject);
    procedure chkbAddFileExtChange(Sender: TObject);
    procedure chkbAddSeparatorChange(Sender: TObject);

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
  formExport: TformExport;

implementation

{$R *.lfm}

uses
  fMain, uConfig, uTreeDB, uSearch, uResources;

{ TformExport }

{ ============================================================ }
{ Function 功能                                                }
{ ============================================================ }

procedure TformExport.SetWindowRect;
begin
  if Config.KeepExportFormRect then begin
    BoundsRect := Config.ExportFormRect;
    Position := poDesigned;
  end else begin
    Position := poDesktopCenter;
  end;
end;

procedure TformExport.SetWindowFont;
begin
  if Screen.Fonts.IndexOf(Config.WindowFontName) <> -1 then
    Font.Name := Config.WindowFontName
  else
    Font.Name := 'default';
  Font.Size := Config.WindowFontSize;
end;

procedure TformExport.LoadSettings;
begin
  chkbAddFileExt.Checked    := Config.ExportAddFileExt;
  chkbAddSeparator.Checked  := Config.ExportAddSeparator;

  editFileExt.Enabled       := chkbAddFileExt.Checked;
  editFileExt.Text          := Config.ExportFileExt;

  editSeparator.Enabled     := chkbAddSeparator.Checked;
  editSeparator.Text        := Config.ExportSeparator;

  // These code will change the state of other controls, so put at the last.
  // 这些代码会改变其它控件的状态，所以放在最后
  radgExportTo.ItemIndex    := Config.ExportTo;
  radgExportFrom.ItemIndex  := Config.ExportFrom;
end;

procedure TformExport.SaveSettings;
begin
  Config.ExportTo           := radgExportTo.ItemIndex;
  Config.ExportFrom         := radgExportFrom.ItemIndex;
  Config.ExportAddFileExt   := chkbAddFileExt.Checked;
  Config.ExportAddSeparator := chkbAddSeparator.Checked;

  editFileExt.Text   := StringReplace(editFileExt.Text, #13, '', 0, False);
  editFileExt.Text   := StringReplace(editFileExt.Text, #10, '\n', 0, False);
  editSeparator.Text := StringReplace(editSeparator.Text, #13, '', 0, False);
  editSeparator.Text := StringReplace(editSeparator.Text, #10, '\n', 0, False);

  Config.ExportFileExt      := editFileExt.Text;
  Config.ExportSeparator    := editSeparator.Text;
end;

procedure TformExport.SwapOkCancel;
begin
  if Config.SwapOKCancel then begin
    bttnOK.Caption     := Res_CaptionCancel;
    bttnOK.Cancel      := True;

    bttnCancel.Caption := Res_CaptionExport;
  end else begin
    bttnOK.Caption     := Res_CaptionExport;

    bttnCancel.Caption := Res_CaptionCancel;
    bttnCancel.Cancel  := True;
  end;
end;

procedure TformExport.OKEvent;
var
  ToPath: string;
  Depth: integer;
  FileExt: string;
  Separator: string;
begin
  // Prevent user from closing the database after opening the Import dialog.
  // 防止用户打开导入对话框后关闭数据库
  if not formMain.DBActive then begin
    Close;
    Exit;
  end;

  // Empty tree | 空树
  if formMain.trevTree.Selected = nil then Exit;

  formMain.SubmitNote;

  FileExt := '';
  if chkbAddFileExt.Enabled and chkbAddFileExt.Checked then
    FileExt := editFileExt.Text;

  Separator := '';
  if chkbAddSeparator.Enabled and chkbAddSeparator.Checked then
    Separator := editSeparator.Text;

  if Config.LastExportDir = '' then
    Config.LastExportDir := ConcatPaths([AppDir, 'data']);
  saveDlg1.InitialDir := Config.LastExportDir;
  seldDlg1.InitialDir := Config.LastExportDir;

  Depth := 1;
  case radgExportFrom.ItemIndex of
    0: // Export single node | 导出单个节点
    begin
      saveDlg1.FileName := formMain.trevTree.Selected.Text;
    end;
    1: // Export branch | 导出分支
    begin
      Depth := AllDepth;
      saveDlg1.FileName := formMain.trevTree.Selected.Text;
    end;
    2: // Export whole tree | 导出整棵树
    begin
      Depth := WholeTree;
      saveDlg1.FileName := ChangeFileExt(ExtractFileName(formMain.DBName), '') + '_export';
    end;
  end;

  if radgExportTo.ItemIndex = 2 then
    saveDlg1.FileName := ChangeFileExt(saveDlg1.FileName, DBFileExt)
  else
    saveDlg1.FileName := ChangeFileExt(saveDlg1.FileName, FileExt);

  case radgExportTo.ItemIndex of
    0: begin // Export to file | 导出到文件
        saveDlg1.Filter := Res_DialogFilterTxt;
        if not saveDlg1.Execute then Exit;
        Hide;
        // Prevent user from modifying the note after opening the Export dialog.
        // 防止用户在打开导出对话框后又修改笔记内容
        formMain.SubmitNote;
        ToPath := saveDlg1.FileName;
        formMain.ExportToFile(ToPath, Separator, Depth);
        Config.LastExportDir := ExtractFileDir(saveDlg1.FileName);
        Close;
      end;
    1: begin // Export to irectory | 导出到目录
        if not seldDlg1.Execute then Exit;
        Hide;
        // Prevent user from modifying the note after opening the Export dialog.
        // 防止用户在打开导出对话框后又修改笔记内容
        formMain.SubmitNote;
        ToPath := seldDlg1.FileName;
        formMain.ExportToDir(ToPath, FileExt, Depth);
        Config.LastExportDir := seldDlg1.FileName;
        Close;
      end;
    2: begin // Export to database | 导出到数据库
        saveDlg1.Filter := Res_DialogFilterTDB;
        if not saveDlg1.Execute then Exit;
        Hide;
        // Prevent user from modifying the note after opening the Export dialog.
        // 防止用户在打开导出对话框后又修改笔记内容
        formMain.SubmitNote;
        ToPath := saveDlg1.FileName;
        formMain.ExportToDB(ToPath, Depth);
        Config.LastExportDir := ExtractFileDir(saveDlg1.FileName);
        Close;
      end;
  end;
end;

procedure TformExport.CancelEvent;
begin
  Close;
end;

{ ============================================================ }
{ Event 事件                                                   }
{ ============================================================ }

procedure TformExport.FormCreate(Sender: TObject);
begin
  SetWindowRect;
  SetWindowFont;

  LoadSettings;
  SwapOkCancel;
end;

procedure TformExport.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Config.ExportFormRect := BoundsRect;
  SaveSettings;
  CloseAction := caFree;
  formExport := nil;
end;

procedure TformExport.FormActivate(Sender: TObject);
begin
  formMain.actnCutUpdate(Sender);
end;

procedure TformExport.bttnOKClick(Sender: TObject);
begin
  if Config.SwapOKCancel then CancelEvent else OKEvent;
end;

procedure TformExport.bttnCancelClick(Sender: TObject);
begin
  if Config.SwapOKCancel then OKEvent else CancelEvent;
end;

procedure TformExport.radgExportToSelectionChanged(Sender: TObject);
begin
  if radgExportTo.ItemIndex <> 2 then begin
    chkbAddFileExt.Enabled := True;
    editFileExt.Enabled := chkbAddFileExt.Checked;
  end else if chkbAddFileExt.Enabled then begin
    chkbAddFileExt.Enabled := False;
    editFileExt.Enabled := False;
  end;

  if (radgExportTo.ItemIndex = 0) and (radgExportFrom.ItemIndex <> 0) then begin
    chkbAddSeparator.Enabled := True;
    editSeparator.Enabled := chkbAddSeparator.Checked;
  end else if chkbAddSeparator.Enabled then begin
    chkbAddSeparator.Enabled := False;
    editSeparator.Enabled := False;
  end;
end;

procedure TformExport.chkbAddFileExtChange(Sender: TObject);
begin
  editFileExt.Enabled := chkbAddFileExt.Checked;
end;

procedure TformExport.chkbAddSeparatorChange(Sender: TObject);
begin
  editSeparator.Enabled := chkbAddSeparator.Checked;
end;

end.
