unit fSearch;

{$mode objfpc}{$H+}

// 字符串常量和字符串字面量需要此选项
// string constants and string literals require this option
{$codepage UTF8}

interface

uses
  Classes, SysUtils, Forms, ComCtrls, ExtCtrls, StdCtrls, LCLType, Graphics,
  ActnList;

type

  { TformSearch }

  TformSearch = class(TForm)
    radgSearchFrom     : TRadioGroup;

    combSearch         : TComboBox;
    chkbDoReplace      : TCheckBox;
    combReplace        : TComboBox;

    chkbSearchInName   : TCheckBox;
    chkbSearchInNote   : TCheckBox;
    chkbIgnoreCase     : TCheckBox;

    chkbNameFilter     : TCheckBox;
    editNameFilter     : TEdit;

    chkbUseRegexpr     : TCheckBox;
    chkbMultiLine      : TCheckBox;
    chkbNonGreedy      : TCheckBox;

    bttnOK             : TButton;
    bttnCancel         : TButton;
    lablSpace          : TLabel;

    { Event 事件 }

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormActivate(Sender: TObject);

    procedure bttnOKClick(Sender: TObject);
    procedure bttnCancelClick(Sender: TObject);

    procedure chkbDoReplaceChange(Sender: TObject);
    procedure combSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure combReplaceKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure chkbUseRegexprChange(Sender: TObject);
    procedure chkbNameFilterChange(Sender: TObject);

    procedure combReplaceCloseUp(Sender: TObject);
    procedure combSearchCloseUp(Sender: TObject);

  private

    { Function 功能 }

    procedure SetWindowRect;
    procedure SetWindowFont;

    procedure LoadSettings;
    procedure SaveSettings;

    procedure SaveComboBoxData(var ComboBox: TComboBox;
      var ConfigItem: TStringList);

    procedure SwapOkCancel;
    procedure OKEvent;
    procedure CancelEvent;

  end;

var
  formSearch: TformSearch;

implementation

uses
  fMain, uConfig, uTreeDB, uCommon, uResources;

{$R *.lfm}

{ TformSearch }

{ ============================================================ }
{ Function 功能                                                }
{ ============================================================ }

procedure TformSearch.SetWindowRect;
begin
  if Config.KeepSearchFormRect then begin
    BoundsRect := Config.SearchFormRect;
    Position := poDesigned;
  end else begin
    Position := poDesktopCenter;
  end;
end;

procedure TformSearch.SetWindowFont;
begin
  if Screen.Fonts.IndexOf(Config.WindowFontName) <> -1 then
    Font.Name := Config.WindowFontName
  else
    Font.Name := 'default';
  Font.Size := Config.WindowFontSize;
end;

procedure TformSearch.LoadSettings;
begin
  radgSearchFrom.ItemIndex   := Config.SearchFrom;

  combSearch.Items           := Config.RecentSearch;
  combSearch.Text            := Config.SearchText;
  combSearch.SelectAll;

  combReplace.Items          := Config.RecentReplace;
  combReplace.Text           := Config.ReplaceText;
  combReplace.Enabled        := chkbDoReplace.Checked;

  chkbSearchInName.Checked   := Config.SearchInName;
  chkbSearchInNote.Checked   := Config.SearchInNote;
  chkbIgnoreCase.Checked     := Config.SearchIgnoreCase;

  chkbNameFilter.Checked     := Config.UseNameFilter;
  editNameFilter.Enabled     := chkbNameFilter.Checked;
  editNameFilter.Text        := Config.NameFilter;

  chkbUseRegexpr.Checked     := Config.SearchUseRegExpr;
  chkbMultiLine.Enabled      := chkbUseRegexpr.Checked;
  chkbNonGreedy.Enabled      := chkbUseRegexpr.Checked;
  chkbMultiLine.Checked      := Config.SearchMultiLineMode;
  chkbNonGreedy.Checked      := Config.SearchNonGreedyMode;

  // These code will change the state of other controls, so put at the last.
  // 这些代码会改变其它控件的状态，所以放在最后

  // To avoid misoperation, this option is not auto selected by default
  // 为了避免误操作，此选项默认不自动选择
  // chkbDoReplace.Checked      := Config.DoReplace;
end;

procedure TformSearch.SaveSettings;
begin
  Config.SearchFrom          := radgSearchFrom.ItemIndex;

  Config.DoReplace           := chkbDoReplace.Checked;
  Config.SearchInName        := chkbSearchInName.Checked;
  Config.SearchInNote        := chkbSearchInNote.Checked;
  Config.SearchIgnoreCase    := chkbIgnoreCase.Checked;
  Config.SearchUseRegExpr    := chkbUseRegexpr.Checked;
  Config.SearchMultiLineMode := chkbMultiLine.Checked;
  Config.SearchNonGreedyMode := chkbNonGreedy.Checked;

  Config.SearchText          := Escape(combSearch.Text);
  Config.ReplaceText         := Escape(combReplace.Text);

  // Save the Search Text | 保存搜索内容
  SaveComboBoxData(combSearch, Config.RecentSearch);

  // Save the Replace Text | 保存替换内容
  SaveComboBoxData(combReplace, Config.RecentReplace);

  Config.UseNameFilter := chkbNameFilter.Checked;
  Config.NameFilter    := editNameFilter.Text;
end;

procedure TformSearch.SaveComboBoxData(var ComboBox: TComboBox;
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

procedure TformSearch.SwapOkCancel;
begin
  if Config.SwapOKCancel then begin
    bttnOK.Caption     := Res_CaptionCancel;
    bttnOK.Cancel      := True;

    bttnCancel.Caption := Res_CaptionSearch;
  end else begin
    bttnOK.Caption     := Res_CaptionSearch;

    bttnCancel.Caption := Res_CaptionCancel;
    bttnCancel.Cancel  := True;
  end;
end;

procedure TformSearch.OKEvent;
var
  Depth         : Integer;
  IgnoreCase    : boolean;
  MultiLineMode : boolean;
  NonGreedyMode : boolean;
  HeadStr       : string;
  SearchText    : string;
  ReplaceText   : string;
  Filter        : string;
  ShowWarning   : Boolean;
begin
  if not formMain.DBActive then begin
    Close;
    Exit;
  end;

  SearchText := combSearch.Text;
  if SearchText = '' then Exit;

  ShowWarning := chkbDoReplace.Checked and ((radgSearchFrom.ItemIndex = 2) or
  (radgSearchFrom.ItemIndex = 1) and (formMain.ActiveNode.Count > 0));

  if ShowWarning and
    (Application.MessageBox(PChar(Res_MultiReplaceTip), PChar(AppTitle), MB_YESNO + MB_ICONQUESTION) <> IDYES) then
    Exit;

  ReplaceText   := combReplace.Text;
  IgnoreCase    := chkbIgnoreCase.Checked;
  MultiLineMode := chkbMultiLine.Enabled and chkbMultiLine.Checked;
  NonGreedyMode := chkbNonGreedy.Enabled and chkbNonGreedy.Checked;

  Hide;

  if chkbUseRegExpr.Checked then begin
    HeadStr := '';
    if IgnoreCase    then HeadStr := HeadStr + '(?i)'   else HeadStr := HeadStr + '(?-i)';
    if MultiLineMode then HeadStr := HeadStr + '(?m-s)' else HeadStr := HeadStr + '(?s-m)';
    if NonGreedyMode then HeadStr := HeadStr + '(?-g)'  else HeadStr := HeadStr + '(?g)';
    SearchText  := HeadStr + RegExprUnEscape(SearchText);
    ReplaceText := RegExprUnEscape(ReplaceText);
  end else begin
    SearchText  := UnEscape(SearchText);
    ReplaceText := UnEscape(ReplaceText);
  end;

  Depth := AllDepth;       // Selected Branch | 所选分支
  case radgSearchFrom.ItemIndex of
    0: Depth := 1;         // Selected Node | 所选节点
    2: Depth := WholeTree; // Whole Tree | 整棵树
  end;

  // Prevent user from modifying the note after opening the Search dialog.
  // 防止用户在打开搜索对话框后又修改笔记内容
  formMain.SubmitNote;

  if chkbNameFilter.Checked then
    Filter := editNameFilter.Text
  else
    Filter := '';

  if chkbDoReplace.Checked then begin
    if chkbUseRegExpr.Checked then
      formMain.RegExprReplace(SearchText, ReplaceText, chkbSearchInName.Checked, chkbSearchInNote.Checked, Depth, Filter)
    else
      formMain.Replace(SearchText, ReplaceText, chkbSearchInName.Checked, chkbSearchInNote.Checked, Depth, IgnoreCase, Filter);
  end else begin
    if chkbUseRegExpr.Checked then
      formMain.RegExprSearch(SearchText, chkbSearchInName.Checked, chkbSearchInNote.Checked, Depth, Filter)
    else
      formMain.Search(SearchText, chkbSearchInName.Checked, chkbSearchInNote.Checked, Depth, IgnoreCase, Filter);
  end;

  Close;
end;

procedure TformSearch.CancelEvent;
begin
  Close;
end;

{ ============================================================ }
{ Event 事件                                                   }
{ ============================================================ }

procedure TformSearch.FormCreate(Sender: TObject);
begin
  SetWindowRect;
  SetWindowFont;

  SwapOkCancel;

  LoadSettings;
end;

procedure TformSearch.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Config.SearchFormRect := BoundsRect;
  SaveSettings;
  CloseAction := caFree;
  formSearch  := nil;
end;

procedure TformSearch.FormActivate(Sender: TObject);
begin
  formMain.actnCutUpdate(Sender);
end;

procedure TformSearch.bttnOKClick(Sender: TObject);
begin
  if Config.SwapOKCancel then CancelEvent else OKEvent;
end;

procedure TformSearch.bttnCancelClick(Sender: TObject);
begin
  if Config.SwapOKCancel then OKEvent else CancelEvent;
end;

procedure TformSearch.chkbDoReplaceChange(Sender: TObject);
begin
  combReplace.Enabled := chkbDoReplace.Checked;

  if chkbDoReplace.Checked then begin
    if Config.SwapOKCancel then begin
      bttnCancel.Font.Color := clRed;
      bttnCancel.Caption := Res_CaptionReplace;
    end else begin
      bttnOK.Font.Color := clRed;
      bttnOK.Caption     := Res_CaptionReplace;
    end;
  end else begin
    if Config.SwapOKCancel then begin
      bttnCancel.Font.Color := clDefault;
      bttnCancel.Caption := Res_CaptionSearch;
    end else begin
      bttnOK.Font.Color := clDefault;
      bttnOK.Caption     := Res_CaptionSearch;
    end;
  end;
end;

procedure TformSearch.combSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    if ssCtrl in Shift then
      OKEvent
    else if combReplace.Enabled then
      combReplace.SetFocus;
end;

procedure TformSearch.combReplaceKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (ssCtrl in Shift) then
    OKEvent;
end;

procedure TformSearch.chkbUseRegexprChange(Sender: TObject);
begin
  chkbMultiLine.Enabled := chkbUseRegexpr.Checked;
  chkbNonGreedy.Enabled := chkbUseRegexpr.Checked;
end;

procedure TformSearch.chkbNameFilterChange(Sender: TObject);
begin
  editNameFilter.Enabled := chkbNameFIlter.Checked;
end;

procedure TformSearch.combSearchCloseUp(Sender: TObject);
begin
  if IsKeyDown(VK_SHIFT) and (combSearch.ItemIndex >= 0) then begin
    Config.RecentSearch.Delete(combSearch.ItemIndex);
    combSearch.Items.Delete(combSearch.ItemIndex);
  end;
end;

procedure TformSearch.combReplaceCloseUp(Sender: TObject);
begin
  if IsKeyDown(VK_SHIFT) and (combReplace.ItemIndex >= 0) then begin
    Config.RecentReplace.Delete(combReplace.ItemIndex);
    combReplace.Items.Delete(combReplace.ItemIndex);
  end;
end;

end.
