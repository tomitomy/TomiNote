unit fOptions;

{$mode objfpc}{$H+}

// 字符串常量和字符串字面量需要此选项
// string constants and string literals require this option
{$codepage UTF8}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls;

type

  { TformOptions }

  TformOptions = class(TForm)
    lstbTabs                 : TListBox;
    pagcMain                 : TPageControl;
    tabsGeneral              : TTabSheet;
    tabsLayout               : TTabSheet;
    tabsTheme                : TTabSheet;
    tabsOther                : TTabSheet;

    bttnOK                   : TButton;
    bttnCancel               : TButton;

    { General 常规 }

    chkbLoadLastFile         : TCheckBox;
    chkbSelectLastNode       : TCheckBox;

    lablAutoSaveInterval     : TLabel;
    editAutoSaveInterval     : TEdit;
    lablAutoBackupInterval   : TLabel;
    editAutoBackupInterval   : TEdit;
    lablAutoBackupCount      : TLabel;
    editAutoBackupCount      : TEdit;

    lablHistoryMaxSize       : TLabel;
    editHistoryMaxSize       : TEdit;
    lablHistoryMinCount      : TLabel;
    editHistoryMinCount      : TEdit;

    chkbDiscardHistory       : TCheckBox;


    { Layout 界面 }

    chkgFullWindowHideBars   : TCheckGroup;

    chkbTreeBarAutoSize      : TCheckBox;
    chkbInfoBarAutoSize      : TCheckBox;
    chkbRecyBarAutoSize      : TCheckBox;

    editTreeBarPercent       : TEdit;
    editInfoBarPercent       : TEdit;
    editRecyBarPercent       : TEdit;

    chkgRememberWindowSize   : TCheckGroup;

    chkbSwapOKCancel         : TCheckBox;
    chkbRemoveMenuBarItem    : TCheckBox;

    { Theme 主题 }

    lablWindowFontName       : TLabel;
    lablTreeBarFontName      : TLabel;
    lablNoteBarFontName      : TLabel;
    lablInfoBarFontName      : TLabel;
    combWindowFontName       : TComboBox;
    combTreeBarFontName      : TComboBox;
    combNoteBarFontName      : TComboBox;
    combInfoBarFontName      : TComboBox;
    lablWindowFontSize       : TLabel;
    lablTreeBarFontSize      : TLabel;
    lablNoteBarFontSize      : TLabel;
    lablInfoBarFontSize      : TLabel;
    editWindowFontSize       : TEdit;
    editTreeBarFontSize      : TEdit;
    editNoteBarFontSize      : TEdit;
    editInfoBarFontSize      : TEdit;

    lablExpandSignSize       : TLabel;
    editExpandSignSize       : TEdit;

    grpbBrightTheme          : TGroupBox;
    lablBrightForeColor      : TLabel;
    lablBrightBackColor      : TLabel;
    clrbBrightForeColor      : TColorButton;
    clrbBrightBackColor      : TColorButton;
    bttnDefBrightTheme       : TButton;

    grpbDarkTheme            : TGroupBox;
    lablDarkForeColor        : TLabel;
    lablDarkBackColor        : TLabel;
    clrbDarkForeColor        : TColorButton;
    clrbDarkBackColor        : TColorButton;
    bttnDefDarkTheme         : TButton;

    { Other }

    editRecentCountLimit     : TEdit;
    editSearchCountLimit     : TEdit;
    lablRecentCountLimit     : TLabel;
    lablSearchCountLimit     : TLabel;

    { Event 事件 }

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

    procedure bttnOKClick(Sender: TObject);
    procedure bttnCancelClick(Sender: TObject);

    procedure lstbTabsSelectionChange(Sender: TObject; User: Boolean);

    procedure chkbInfoBarAutoSizeChange(Sender: TObject);
    procedure chkbRecyBarAutoSizeChange(Sender: TObject);
    procedure chkbTreeBarAutoSizeChange(Sender: TObject);

    procedure bttnDefDarkThemeClick(Sender: TObject);
    procedure bttnDefBrightThemeClick(Sender: TObject);

    // Based 0 | 从 0 开始
    procedure editMouseWheel0(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    // Based 1 | 从 1 开始
    procedure editMouseWheel1(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

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
  formOptions              : TformOptions;

implementation

uses
  uResources, uConfig, fMain;

{$R *.lfm}

{ TformOptions }

{ ============================================================ }
{ Function 功能                                                }
{ ============================================================ }

procedure TformOptions.SetWindowRect;
begin
  if Config.KeepOptionsFormRect then begin
    BoundsRect := Config.OptionsFormRect;
    Position := poDesigned;
  end else begin
    Position := poDesktopCenter;
  end;
end;

procedure TformOptions.SetWindowFont;
begin
  if Screen.Fonts.IndexOf(Config.WindowFontName) <> -1 then
    Font.Name := Config.WindowFontName
  else
    Font.Name := 'default';
  Font.Size := Config.WindowFontSize;
end;

procedure TformOptions.LoadSettings;
var
  HistorySize: Double;
  HistorySizeStr: string;
begin
  pagcMain.ShowTabs           := False;
  lstbTabs.ItemIndex          := 0;

  { General 常规 }

  chkbLoadLastFile.Checked    := Config.LoadLastFile;
  chkbSelectLastNode.Checked  := Config.SelectLastNode;

  editAutoSaveInterval.Text   := IntToStr(Config.AutoSaveInterval);
  editAutoBackupInterval.Text := IntToStr(Config.AutoBackupInterval);
  editAutoBackupCount.Text    := IntToStr(Config.AutoBackupCount);

  editHistoryMaxSize.Text     := IntToStr(Config.HistoryMaxSize);
  editHistoryMinCount.Text    := IntToStr(Config.HistoryMinCount);

  HistorySize := formMain.GetHistorySize;
  if HistorySize < 1024 then begin
    HistorySizeStr := Format('%.0f B', [HistorySize]);
  end else if HistorySize < 1024 * 1024 then begin
    HistorySize := HistorySize / 1024;
    HistorySizeStr := Format('%.1f KB', [HistorySize]);
  end else begin
    HistorySize := HistorySize / (1024 * 1024);
    HistorySizeStr := Format('%.1f MB', [HistorySize]);
  end;

  chkbDiscardHistory.Caption := Format(chkbDiscardHistory.Caption, [HistorySizeStr]);

  { Layout 界面 }

  chkgFullWindowHideBars.Checked[0] := Config.FullWindowHideMenuBar;
  chkgFullWindowHideBars.Checked[1] := Config.FullWindowHideToolBar;
  chkgFullWindowHideBars.Checked[2] := Config.FullWindowHideStatBar;
  chkgFullWindowHideBars.Checked[3] := Config.FullWindowHideTreeBar;
  chkgFullWindowHideBars.Checked[4] := Config.FullWindowHideRecyBar;
  chkgFullWindowHideBars.Checked[5] := Config.FullWindowHideInfoBar;

  chkbTreeBarAutoSize.Checked := Config.TreeBarAutoSize;
  editTreeBarPercent.Enabled  := chkbTreeBarAutoSize.Checked;
  editTreeBarPercent.Text     := Config.TreeBarPercent.ToString;

  chkbRecyBarAutoSize.Checked := Config.RecyBarAutoSize;
  editRecyBarPercent.Enabled  := chkbRecyBarAutoSize.Checked;
  editRecyBarPercent.Text     := Config.RecyBarPercent.ToString;

  chkbInfoBarAutoSize.Checked := Config.InfoBarAutoSize;
  editInfoBarPercent.Enabled  := chkbInfoBarAutoSize.Checked;
  editInfoBarPercent.Text     := Config.InfoBarPercent.ToString;

  chkgRememberWindowSize.Checked[0] := Config.KeepMainFormRect;
  chkgRememberWindowSize.Checked[1] := Config.KeepSearchFormRect;
  chkgRememberWindowSize.Checked[2] := Config.KeepImportFormRect;
  chkgRememberWindowSize.Checked[3] := Config.KeepExportFormRect;
  chkgRememberWindowSize.Checked[4] := Config.KeepOptionsFormRect;

  chkbSwapOKCancel.Checked      := Config.SwapOKCancel;
  chkbRemoveMenuBarItem.Checked := Config.RemoveMenuBarItem;

  { Theme 主题 }

  combWindowFontName.Items  := Screen.Fonts;
  combTreeBarFontName.Items := Screen.Fonts;
  combNoteBarFontName.Items := Screen.Fonts;
  combInfoBarFontName.Items := Screen.Fonts;

  combWindowFontName.ItemIndex  := combWindowFontName.Items.IndexOf(Config.WindowFontName);
  combTreeBarFontName.ItemIndex := combTreeBarFontName.Items.IndexOf(Config.TreeBarFontName);
  combNoteBarFontName.ItemIndex := combNoteBarFontName.Items.IndexOf(Config.NoteBarFontName);
  combInfoBarFontName.ItemIndex := combInfoBarFontName.Items.IndexOf(Config.InfoBarFontName);

  editWindowFontSize.Text  := Config.WindowFontSize.ToString;
  editTreeBarFontSize.Text := Config.TreeBarFontSize.ToString;
  editNoteBarFontSize.Text := Config.NoteBarFontSize.ToString;
  editInfoBarFontSize.Text := Config.InfoBarFontSize.ToString;

  editExpandSignSize.Text := Config.ExpandSignSize.ToString;

  clrbBrightForeColor.ButtonColor := Config.BrightForeColor;
  clrbBrightBackColor.ButtonColor := Config.BrightBackColor;

  clrbDarkForeColor.ButtonColor := Config.DarkForeColor;
  clrbDarkBackColor.ButtonColor := Config.DarkBackColor;

  editSearchCountLimit.Text := IntToStr(Config.SearchCountLimit);
  editRecentCountLimit.Text := IntToStr(Config.RecentCountLimit);
end;

procedure TformOptions.SaveSettings;
begin

  { General 常规 }

  Config.LoadLastFile := chkbLoadLastFile.Checked;
  Config.SelectLastNode := chkbSelectLastNode.Checked;

  Config.AutoSaveInterval := StrToIntDef(editAutoSaveInterval.Text, 0);
  Config.AutoBackupInterval := StrToIntDef(editAutoBackupInterval.Text, 0);
  Config.AutoBackupCount := StrToIntDef(editAutoBackupCount.Text, 0);

  Config.HistoryMaxSize := StrToIntDef(editHistoryMaxSize.Text, 0);
  Config.HistoryMinCount := StrToIntDef(editHistoryMinCount.Text, 0);

  if chkbDiscardHistory.Checked then formMain.DiscardHistory(False);

  { Layout 界面}

  Config.FullWindowHideMenuBar := chkgFullWindowHideBars.Checked[0];
  Config.FullWindowHideToolBar := chkgFullWindowHideBars.Checked[1];
  Config.FullWindowHideStatBar := chkgFullWindowHideBars.Checked[2];
  Config.FullWindowHideTreeBar := chkgFullWindowHideBars.Checked[3];
  Config.FullWindowHideRecyBar := chkgFullWindowHideBars.Checked[4];
  Config.FullWindowHideInfoBar := chkgFullWindowHideBars.Checked[5];

  Config.TreeBarAutoSize := chkbTreeBarAutoSize.Checked;
  Config.RecyBarAutoSize := chkbRecyBarAutoSize.Checked;
  Config.InfoBarAutoSize := chkbInfoBarAutoSize.Checked;

  Config.TreeBarPercent := StrToIntDef(editTreeBarPercent.Text, DefTreeBarPercent);
  Config.RecyBarPercent := StrToIntDef(editRecyBarPercent.Text, DefRecyBarPercent);
  Config.InfoBarPercent := StrToIntDef(editInfoBarPercent.Text, DefInfoBarPercent);

  Config.KeepMainFormRect    := chkgRememberWindowSize.Checked[0];
  Config.KeepSearchFormRect  := chkgRememberWindowSize.Checked[1];
  Config.KeepImportFormRect  := chkgRememberWindowSize.Checked[2];
  Config.KeepExportFormRect  := chkgRememberWindowSize.Checked[3];
  Config.KeepOptionsFormRect := chkgRememberWindowSize.Checked[4];

  Config.SwapOKCancel      := chkbSwapOKCancel.Checked;
  Config.RemoveMenuBarItem := chkbRemoveMenuBarItem.Checked;

  { Theme 主题 }

  Config.WindowFontName  := combWindowFontName.Text;
  Config.TreeBarFontName := combTreeBarFontName.Text;
  Config.NoteBarFontName := combNoteBarFontName.Text;
  Config.InfoBarFontName := combInfoBarFontName.Text;

  Config.WindowFontSize  := StrToIntDef(editWindowFontSize.Text, 0);
  Config.TreeBarFontSize := StrToIntDef(editTreeBarFontSize.Text, DefFontSize);
  Config.NoteBarFontSize := StrToIntDef(editNoteBarFontSize.Text, DefFontSize);
  Config.InfoBarFontSize := StrToIntDef(editInfoBarFontSize.Text, DefFontSize);

  Config.ExpandSignSize  := StrToIntDef(editExpandSignSize.Text, 0);

  Config.BrightForeColor := clrbBrightForeColor.ButtonColor;
  Config.BrightBackColor := clrbBrightBackColor.ButtonColor;

  Config.DarkForeColor := clrbDarkForeColor.ButtonColor;
  Config.DarkBackColor := clrbDarkBackColor.ButtonColor;

  Config.SearchCountLimit := StrToIntDef(editSearchCountLimit.Text, DefSearchCountLimit);
  Config.RecentCountLimit := StrToIntDef(editRecentCountLimit.Text, DefRecentCountLimit);
end;

procedure TformOptions.SwapOkCancel;
begin
  if Config.SwapOKCancel then begin
    bttnOK.Caption     := Res_CaptionCancel;
    bttnOK.Cancel      := True;

    bttnCancel.Caption := Res_CaptionOK;
  end else begin
    bttnOK.Caption     := Res_CaptionOK;

    bttnCancel.Caption := Res_CaptionCancel;
    bttnCancel.Cancel  := True;
  end;
end;

procedure TformOptions.OKEvent;
begin
  Hide;
  SaveSettings;
  formMain.LoadControlState;
  Close;
end;

procedure TformOptions.CancelEvent;
begin
  Close;
end;

{ ============================================================ }
{ Event 事件                                                   }
{ ============================================================ }

procedure TformOptions.FormCreate(Sender: TObject);
begin
  SetWindowRect;
  SetWindowFont;

  SwapOkCancel;
  LoadSettings;
end;

procedure TformOptions.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Config.OptionsFormRect := BoundsRect;
  CloseAction := caFree;
  formOptions := nil;
end;

procedure TformOptions.bttnOKClick(Sender: TObject);
begin
  if Config.SwapOKCancel then CancelEvent else OKEvent;
end;

procedure TformOptions.bttnCancelClick(Sender: TObject);
begin
  if Config.SwapOKCancel then OKEvent else CancelEvent;
end;

{ ListBox 列表框 }

procedure TformOptions.lstbTabsSelectionChange(Sender: TObject; User: Boolean);
begin
  if pagcMain.PageCount > lstbTabs.ItemIndex then
    pagcMain.ActivePageIndex := lstbTabs.ItemIndex;
end;

{ CheckBox 复选框 }

procedure TformOptions.chkbTreeBarAutoSizeChange(Sender: TObject);
begin
  editTreeBarPercent.Enabled := chkbTreeBarAutoSize.Checked;
end;

procedure TformOptions.chkbRecyBarAutoSizeChange(Sender: TObject);
begin
  editRecyBarPercent.Enabled := chkbRecyBarAutoSize.Checked;
end;

procedure TformOptions.chkbInfoBarAutoSizeChange(Sender: TObject);
begin
  editInfoBarPercent.Enabled := chkbInfoBarAutoSize.Checked;
end;

{ Button 按钮 }

procedure TformOptions.bttnDefBrightThemeClick(Sender: TObject);
begin
  clrbBrightForeColor.ButtonColor := DefBrightForeColor;
  clrbBrightBackColor.ButtonColor := DefBrightBackColor;
end;

procedure TformOptions.bttnDefDarkThemeClick(Sender: TObject);
begin
  clrbDarkForeColor.ButtonColor  := DefDarkForeColor;
  clrbDarkBackColor.ButtonColor  := DefDarkBackColor;
end;

{ Edit 编辑框 }

// Based 0 | 从 0 开始
procedure TformOptions.editMouseWheel0(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  Num: Integer;
begin
  Num := StrToIntDef((Sender as TEdit).Text, 0);
  if WheelDelta > 0 then Inc(Num) else Dec(Num);
  if Num < 0 then Num := 0;
  (Sender as TEdit).Text := IntToStr(Num);
end;

// Based 1 | 从 1 开始
procedure TformOptions.editMouseWheel1(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  Num: Integer;
begin
  Num := StrToIntDef((Sender as TEdit).Text, 1);
  if WheelDelta > 0 then Inc(Num) else Dec(Num);
  if Num < 1 then Num := 1;
  (Sender as TEdit).Text := IntToStr(Num);
end;

end.
