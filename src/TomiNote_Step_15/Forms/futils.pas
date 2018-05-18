unit fUtils;

{$mode objfpc}{$H+}

// String constants and string literals require this option
// 字符串常量和字符串字面量需要此选项
{$codepage UTF8}

interface

uses
  Classes, SysUtils, Forms, ComCtrls, StdCtrls, ExtCtrls, Types, LCLType,
  Controls, Menus, ActnList, Clipbrd, uHistory;

type

  { TformUtils }

  TformUtils = class(TForm)

    pgctMain                 : TPageControl;
    tabsSort                 : TTabSheet;
    tabsSplit                : TTabSheet;
    tabsRename               : TTabSheet;

    bttnOK                   : TButton;
    bttnCancel               : TButton;

    { Sort }

    radgSortDirection        : TRadioGroup;
    radgSortOf               : TRadioGroup;

    { Split }

    lablSeparatorSp          : TLabel;
    combSeparatorSp          : TComboBox;

    lablTitleSp              : TLabel;
    combTitleSp              : TComboBox;

    chkbIgnoreCaseSp         : TCheckBox;
    chkbMultiLineSp          : TCheckBox;
    chkbNonGreedySp          : TCheckBox;

    chkbIncludeSeparatorSp   : TCheckBox;

    chkbAddPreNumSp          : TCheckBox;
    editPreNumLenSp          : TEdit;

    { Rename }

    lablSearchRn             : TLabel;
    combSearchRn             : TComboBox;

    lablReplaceRn            : TLabel;
    combReplaceRn            : TComboBox;

    chkbIgnoreCaseRn         : TCheckBox;
    chkbMultiLineRn          : TCheckBox;
    chkbNonGreedyRn          : TCheckBox;

    radgSearchInRn           : TRadioGroup;

    chkbAddPreNumRn          : TCheckBox;
    editPreNumLenRn          : TEdit;

    chkbSpecifyDepthRn       : TCheckBox;
    editDepthRn              : TEdit;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormActivate(Sender: TObject);

    procedure bttnOKClick(Sender: TObject);
    procedure bttnCancelClick(Sender: TObject);

    procedure chkbAddPreNumSpChange(Sender: TObject);
    procedure combSeparatorSpCloseUp(Sender: TObject);
    procedure combTitleSpCloseUp(Sender: TObject);

    procedure combReplaceRnCloseUp(Sender: TObject);
    procedure combSearchRnCloseUp(Sender: TObject);
    procedure chkbAddPreNumRnChange(Sender: TObject);
    procedure chkbSpecifyDepthRnChange(Sender: TObject);

    procedure editMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

  private
    procedure SetWindowRect;
    procedure SetWindowFont;

    procedure LoadSettings;
    procedure SaveSettings;

    procedure SwapOkCancel;
    procedure OKEvent;
    procedure CancelEvent;

    procedure PerformSort;
    procedure PerformSplit;
    procedure PerformRename;

    procedure SaveComboBoxData(var ComboBox: TComboBox; var ConfigItem: TStringList);
    function  CheckDepth: Boolean;
  end;

var
  formUtils: TformUtils;

implementation

uses
  uConfig, uCommon, uResources, uTreeDB, fMain;

{$R *.lfm}

{ TformUtils }

{ ============================================================ }
{ Function 功能                                                }
{ ============================================================ }

procedure TformUtils.SetWindowRect;
begin
  if Config.KeepUtilsFormRect then begin
    BoundsRect := Config.UtilsFormRect;
    Position := poDesigned;
  end else begin
    Position := poDesktopCenter;
  end;
end;

procedure TformUtils.SetWindowFont;
begin
  if Screen.Fonts.IndexOf(Config.WindowFontName) <> -1 then
    Font.Name := Config.WindowFontName
  else
    Font.Name := 'default';
  Font.Size := Config.WindowFontSize;
end;

procedure TformUtils.LoadSettings;
begin
  if (Config.UtilsTabIndex < 0) or (Config.UtilsTabIndex >= pgctMain.PageCount) then
    Config.UtilsTabIndex := 0;

  pgctMain.TabIndex             := Config.UtilsTabIndex;

  { Sort }

  radgSortDirection.ItemIndex   := Config.SortDirection;
  radgSortOf.ItemIndex          := Config.SortOf;

  { Split }

  combSeparatorSp.Text          := Config.SplitSeparator;
  combTitleSp.Text              := Config.SplitTitle;

  combSeparatorSp.Items         := Config.RecentSeparator;
  combTitleSp.Items             := Config.RecentTitle;

  chkbIgnoreCaseSp.Checked      := Config.IgnoreCaseSp;
  chkbMultiLineSp.Checked       := Config.MultiLineSp;
  chkbNonGreedySp.Checked       := Config.NonGreedySp;

  chkbIncludeSeparatorSp.Checked := Config.IncludeSeparator;

  chkbAddPreNumSp.Checked       := Config.AddPreNumSp;
  editPreNumLenSP.Enabled       := chkbAddPreNumSP.Checked;
  editPreNumLenSp.Text          := IntToStr(Config.PreNumLenSp);

  { Rename }

  combSearchRn.Text             := Config.SearchTextRn;
  combReplaceRn.Text            := Config.ReplaceTextRn;

  combSearchRn.Items            := Config.RecentSearchRn;
  combReplaceRn.Items           := Config.RecentReplaceRn;

  chkbIgnoreCaseRn.Checked      := Config.IgnoreCaseRn;
  chkbMultiLineRn.Checked       := Config.MultiLineRn;
  chkbNonGreedyRn.Checked       := Config.NonGreedyRn;

  radgSearchInRn.ItemIndex      := Config.SearchIn;

  chkbSpecifyDepthRn.Checked    := Config.SpecifyDepthRn;
  editDepthRn.Enabled           := chkbSpecifyDepthRn.Checked;
  editDepthRn.Text              := IntToStr(Config.DepthRn);

  chkbAddPreNumRn.Checked       := Config.AddPreNumRn;
  editPreNumLenRn.Enabled       := chkbAddPreNumRn.Checked;
  editPreNumLenRn.Text          := IntToStr(Config.PreNumLenRn);

  editPreNumLenSp.OnMouseWheel  := @editMouseWheel;
  editPreNumLenRn.OnMouseWheel  := @editMouseWheel;
  editDepthRn.OnMouseWheel      := @editMouseWheel;
end;

procedure TformUtils.SaveSettings;
begin
  Config.UtilsTabIndex          := pgctMain.TabIndex;

  { Sort }

  Config.SortDirection          := radgSortDirection.ItemIndex;
  Config.SortOf                 := radgSortOf.ItemIndex;

  { Split }

  combSeparatorSp.Text          := Escape(combSeparatorSp.Text);
  combTitleSp.Text              := Escape(combTitleSp.Text);

  Config.SplitSeparator         := combSeparatorSp.Text;
  Config.SplitTitle             := combTitleSp.Text;

  Config.IgnoreCaseSp           := chkbIgnoreCaseSp.Checked;
  Config.MultiLineSp            := chkbMultiLineSp.Checked;
  Config.NonGreedySp            := chkbNonGreedySp.Checked;

  Config.IncludeSeparator       := chkbIncludeSeparatorSp.Checked;

  Config.AddPreNumSp            := chkbAddPreNumSp.Checked;
  Config.PreNumLenSp            := StrToIntDef(editPreNumLenSp.Text, 3);

  { Rename }

  Config.SearchTextRn           := combSearchRn.Text;
  Config.ReplaceTextRn          := combReplaceRn.Text;

  Config.IgnoreCaseRn           := chkbIgnoreCaseRn.Checked;
  Config.MultiLineRn            := chkbMultiLineRn.Checked;
  Config.NonGreedyRn            := chkbNonGreedyRn.Checked;

  Config.SearchIn               := radgSearchInRn.ItemIndex;

  Config.SpecifyDepthRn         := chkbSpecifyDepthRn.Checked;
  Config.DepthRn                := StrToIntDef(editDepthRn.Text, 0);

  Config.AddPreNumRn            := chkbAddPreNumRn.Checked;
  Config.PreNumLenRn            := StrToIntDef(editPreNumLenRn.Text, 3);
end;

procedure TformUtils.SaveComboBoxData(var ComboBox: TComboBox;
  var ConfigItem: TStringList);
var
  Index: Integer;
begin
  if ComboBox.Text <> '' then begin
    ComboBox.Text := Escape(ComboBox.Text);
    Index := ConfigItem.IndexOf(ComboBox.Text);
    if Index >= 0 then
      ConfigItem.Delete(Index)
    else while ConfigItem.Count >= Config.RecentCountLimit do
      ConfigItem.Delete(ConfigItem.Count - 1);

    ConfigItem.Insert(0, ComboBox.Text);
  end;
end;

function TformUtils.CheckDepth: Boolean;
var
  Depth: Integer;
begin
  Result := True;
  if pgctMain.ActivePageIndex <> 2 then Exit;

  if chkbSpecifyDepthRn.Checked then
    Depth := StrToIntDef(editDepthRn.Text, 0)
  else
    Depth := AllDepth;

  if (combSearchRn.Text = '') or (Depth = 0) or (Depth > 1) and
  (Application.MessageBox(PChar(Res_RenameWarning), PChar(AppTitle),
  MB_YESNO + MB_ICONQUESTION) <> ID_YES) then
    Result := False;
end;

procedure TformUtils.SwapOkCancel;
begin
  if Config.SwapOKCancel then begin
    bttnOK.Caption     := Res_CaptionCancel;
    // bttnOK.Cancel      := True;

    bttnCancel.Caption := Res_CaptionExecute;
  end else begin
    bttnOK.Caption     := Res_CaptionExecute;

    bttnCancel.Caption := Res_CaptionCancel;
    // bttnCancel.Cancel  := True;
  end;
end;

procedure TformUtils.OKEvent;
begin
  // Prevent user from closing the database after opening the Utils dialog
  // 防止用户打开工具对话框后关闭数据库
  if not formMain.DBActive then begin
    Close;
    Exit;
  end;

  if not CheckDepth then Exit;

  // Prevent user from modifying the note after opening the Utils dialog.
  // 防止用户在打开工具对话框后又修改笔记内容
  formMain.SubmitNote;

  SaveComboBoxData(combSeparatorSp, Config.RecentSeparator);
  SaveComboBoxData(combTitleSp,     Config.RecentTitle);
  SaveComboBoxData(combSearchRn,    Config.RecentSearchRn);
  SaveComboBoxData(combReplaceRn,   Config.RecentReplaceRn);

  Hide;

  case pgctMain.ActivePageIndex of
    0: PerformSort;
    1: PerformSplit;
    2: PerformRename;
  end;

  Close;
end;

procedure TformUtils.CancelEvent;
begin
  Close;
end;

procedure TformUtils.PerformSort;
begin
  formMain.SortNode(radgSortOf.ItemIndex = 0, radgSortDirection.ItemIndex = 1);
end;

procedure TformUtils.PerformSplit;
var
  HeadStr, Separator, Title: string;
  IncludeSeparator: boolean;
  PreNumLen: integer;
begin
  Separator := RegExprUnEscape(combSeparatorSp.Text);
  if Separator = '' then Exit;

  HeadStr := '';
  if chkbIgnoreCaseSp.Checked then HeadStr := HeadStr + '(?i)'   else HeadStr := HeadStr + '(?-i)';
  if chkbMultiLineSp.Checked  then HeadStr := HeadStr + '(?m-s)' else HeadStr := HeadStr + '(?s-m)';
  if chkbNonGreedySp.Checked  then HeadStr := HeadStr + '(?-g)'  else HeadStr := HeadStr + '(?g)';

  Separator := HeadStr + Separator;

  Title     := RegExprUnEscape(combTitleSp.Text);
  if Title <> '' then Title := HeadStr + Title;

  IncludeSeparator := chkbIncludeSeparatorSp.Checked;

  if chkbAddPreNumSp.Checked then
    PreNumLen := StrToIntDef(editPreNumLenSp.Text, 0)
  else
    PreNumLen := 0;

  formMain.SplitNote(Separator, Title, IncludeSeparator, PreNumLen);
end;

procedure TformUtils.PerformRename;
var
  HeadStr, SearchText, NameText: string;
  SearchInNote: boolean;
  Depth, PreNumLen: Integer;
begin
  SearchText := RegExprUnEscape(combSearchRn.Text);
  if SearchText = '' then Exit;

  NameText := RegExprUnEscape(combReplaceRn.Text);
  if NameText = '' then Exit;

  HeadStr := '';
  if chkbIgnoreCaseRn.Checked then HeadStr := HeadStr + '(?i)'   else HeadStr := HeadStr + '(?-i)';
  if chkbMultiLineRn.Checked  then HeadStr := HeadStr + '(?m-s)' else HeadStr := HeadStr + '(?s-m)';
  if chkbNonGreedyRn.Checked  then HeadStr := HeadStr + '(?-g)'  else HeadStr := HeadStr + '(?g)';

  SearchText := HeadStr + SearchText;

  SearchInNote := radgSearchInRn.ItemIndex = 1;

  if chkbAddPreNumRn.Checked then
    PreNumLen := StrToIntDef(editPreNumLenRn.Text, 0)
  else
    PreNumLen := 0;

  if chkbSpecifyDepthRn.Checked then
    Depth := StrToIntDef(editDepthRn.Text, 1)
  else
    Depth := AllDepth;

  formMain.RenameNodes(SearchText, NameText, SearchInNote, PreNumLen, Depth);
end;

{ ============================================================ }
{ Event 事件                                                   }
{ ============================================================ }

{ Form }

procedure TformUtils.FormCreate(Sender: TObject);
begin
  SetWindowRect;
  SetWindowFont;

  LoadSettings;
  SwapOkCancel;
end;

procedure TformUtils.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Config.UtilsFormRect     := BoundsRect;
  SaveSettings;

  CloseAction              := caFree;
  formUtils                := nil;
end;

procedure TformUtils.FormActivate(Sender: TObject);
begin
  formMain.actnCutUpdate(Sender);
end;

procedure TformUtils.bttnOKClick(Sender: TObject);
begin
  if Config.SwapOKCancel then CancelEvent else OKEvent;
end;

procedure TformUtils.bttnCancelClick(Sender: TObject);
begin
  if Config.SwapOKCancel then OKEvent else CancelEvent;
end;

{ Split }

procedure TformUtils.combSeparatorSpCloseUp(Sender: TObject);
var
  Index: Integer;
begin
  Index := combSeparatorSp.ItemIndex;
  if IsKeyDown(VK_SHIFT) and (Index >= 0) then begin
    Config.RecentSeparator.Delete(Index);
    combSeparatorSp.Items.Delete(Index);
  end;
end;

procedure TformUtils.combTitleSpCloseUp(Sender: TObject);
var
  Index: Integer;
begin
  Index := combTitleSp.ItemIndex;
  if IsKeyDown(VK_SHIFT) and (Index >= 0) then begin
    Config.RecentTitle.Delete(Index);
    combTitleSp.Items.Delete(Index);
  end;
end;

procedure TformUtils.chkbAddPreNumSpChange(Sender: TObject);
begin
  editPreNumLenSp.Enabled := chkbAddPreNumSp.Checked;
end;

{ Rename }

procedure TformUtils.combSearchRnCloseUp(Sender: TObject);
var
  Index: Integer;
begin
  Index := combSearchRn.ItemIndex;
  if IsKeyDown(VK_SHIFT) and (Index >= 0) then begin
    Config.RecentSearchRn.Delete(Index);
    combSearchRn.Items.Delete(Index);
  end;
end;

procedure TformUtils.combReplaceRnCloseUp(Sender: TObject);
var
  Index: Integer;
begin
  Index := combReplaceRn.ItemIndex;
  if IsKeyDown(VK_SHIFT) and (Index >= 0) then begin
    Config.RecentReplaceRn.Delete(Index);
    combReplaceRn.Items.Delete(Index);
  end;
end;

procedure TformUtils.chkbAddPreNumRnChange(Sender: TObject);
begin
  editPreNumLenRn.Enabled := chkbAddPreNumRn.Checked;
end;

procedure TformUtils.chkbSpecifyDepthRnChange(Sender: TObject);
begin
  editDepthRn.Enabled := chkbSpecifyDepthRn.Checked;
end;

procedure TformUtils.editMouseWheel(Sender: TObject; Shift: TShiftState;
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

