unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, uSearch, uCommon;

type

  { TformMain }

  TformMain = class(TForm)
    panlSearch               : TPanel;
    lablSearch               : TLabel;
    memoSearch               : TMemo;
    spltSearch               : TSplitter;
    lstbSearchResult         : TListBox;

    spltMain                 : TSplitter;

    panlReplace              : TPanel;
    lablReplace              : TLabel;
    memoReplace              : TMemo;
    spltReplace              : TSplitter;
    lstbReplaceResult        : TListBox;

    panlBottom               : TPanel;
    editSearch               : TEdit;
    editReplace              : TEdit;
    bttnSearch               : TButton;
    bttnReplace              : TButton;
    chkbIgnoreCase           : TCheckBox;
    chkbRegExpr              : TCheckBox;

    procedure FormResize(Sender: TObject);

    procedure bttnSearchClick(Sender: TObject);
    procedure bttnReplaceClick(Sender: TObject);

    procedure editSearchEnter(Sender: TObject);
    procedure editReplaceEnter(Sender: TObject);

    procedure lstbReplaceResultMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lstbSearchResultMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    procedure Search;
    procedure RegExprSearch;
    procedure Replace;
    procedure RegExprReplace;
  public

  end;

var
  formMain: TformMain;

implementation

uses
  lazUTF8;

{$R *.lfm}

{ TformMain }

procedure TformMain.FormResize(Sender: TObject);
begin
  panlSearch.Width := (Width - 16) div 2 - 2;
  spltMain.Left := Width;

  lstbSearchResult.Height := panlSearch.Height * 30 div 100;
  spltSearch.Top := 0;

  lstbReplaceResult.Height := lstbSearchResult.Height;
  spltReplace.Top := 0;

  editSearch.Width := (panlBottom.Width - 420) div 2;
  editReplace.Width := editSearch.Width;
end;

procedure TformMain.bttnSearchClick(Sender: TObject);
begin
  lstbSearchResult.Clear;
  if chkbRegExpr.Checked then RegExprSearch else Search;
end;

procedure TformMain.bttnReplaceClick(Sender: TObject);
begin
  lstbReplaceResult.Clear;
  if chkbRegExpr.Checked then RegExprReplace else Replace;
end;

procedure TformMain.editSearchEnter(Sender: TObject);
begin
  if (editSearch.Tag = 0) and (editSearch.Text = 'Search For') then begin
    editSearch.Text := '';
    editSearch.Tag := 1;
  end;
end;

procedure TformMain.editReplaceEnter(Sender: TObject);
begin
  if (editReplace.Tag = 0) and (editReplace.Text = 'Replace As') then begin
    editReplace.Text := '';
    editReplace.Tag := 1;
  end;
end;

procedure TformMain.lstbSearchResultMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Str: string;
  Index: Integer;
begin
  if lstbSearchResult.ItemIndex >= 0 then begin
    Str := lstbSearchResult.Items[lstbSearchResult.ItemIndex];
    Index := Pos('|', Str);
    memoSearch.SelStart := StrToIntDef(Copy(Str, 1, Index - 1), 0) - 1;
    memoSearch.SelLength := StrToIntDef(Copy(Str, Index + 1), 0);
  end;
  memoSearch.SetFocus;
end;

procedure TformMain.lstbReplaceResultMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Str: string;
  Index: Integer;
begin
  if lstbReplaceResult.ItemIndex >= 0 then begin
    Str := lstbReplaceResult.Items[lstbReplaceResult.ItemIndex];
    Index := Pos('|', Str);
    memoReplace.SelStart := StrToIntDef(Copy(Str, 1, Index - 1), 0) - 1;
    memoReplace.SelLength := StrToIntDef(Copy(Str, Index + 1), 0);
  end;
  memoReplace.SetFocus;
end;

procedure TformMain.Search;
var
  SearchText: string;
  i: Integer;
  Matches: TSizeIntArray;
  Index, Len: SizeInt;
begin
  SearchText := UnEscape(editSearch.Text);
  i := UTF8FindMatches(memoSearch.Text, SearchText, Matches, 0, chkbIgnoreCase.Checked);
  for i := 0 to Length(Matches) - 1 do begin
    Index := Matches[i];
    Len   := Length(SearchText);
    lstbSearchResult.Items.Add(IntToStr(Index) + '|' + IntToStr(Len));
  end;
  lablSearch.Caption := 'Search From - ' + IntToStr(i);
end;

procedure TformMain.RegExprSearch;
var
  SearchText: string;
  i: Integer;
  Matches: TSizeIntArray;
  Index, Len: SizeInt;
begin
  // 将正则表达式中的转义字符（\r \n \t \xFF）转换为特殊字符（#13 #10 #9 Char），
  // 不处理 \\。正则表达式自己会处理 \\，所以这里要用专用的反转义函数
  // convert the escaped characters in the regular expression (\r \n \t \xFF) to
  // special characters (#13 #10 #9 Char), it doesn't deal with \\. the regular
  // expression itself will deal with \\, so here we use a special unescape function
  SearchText := RegExprUnEscape(editSearch.Text);
  i := RegExprFindMatches(memoSearch.Text, SearchText, Matches, 0, chkbIgnoreCase.Checked);
  for i := 0 to Length(Matches) div 2 - 1 do begin
    Index := Matches[i*2];
    Len   := Matches[i*2+1];
    lstbSearchResult.Items.Add(IntToStr(Index) + '|' + IntToStr(Len));
  end;
  lablSearch.Caption := 'Search From - ' + IntToStr(i);
end;

procedure TformMain.Replace;
var
  SearchText, ReplaceText: string;
  i: Integer;
  Matches: TSizeIntArray;
  Index, Len: SizeInt;
begin
  SearchText  := UnEscape(editSearch.Text);
  ReplaceText := UnEscape(editReplace.Text);
  memoReplace.Text := UTF8ReplaceMatches(memoSearch.Text, SearchText,
    ReplaceText, Matches, 0, chkbIgnoreCase.Checked);
  for i := 0 to Length(Matches) - 1 do begin
    Index := Matches[i];
    Len   := Length(ReplaceText);
    lstbReplaceResult.Items.Add(IntToStr(Index) + '|' + IntToStr(Len));
  end;
  lablReplace.Caption := 'Replace Result - ' + IntToStr(i);
end;

procedure TformMain.RegExprReplace;
var
  SearchText, ReplaceText: string;
  i: Integer;
  Matches: TSizeIntArray;
  Index, Len: SizeInt;
begin
  SearchText  := RegExprUnEscape(editSearch.Text);
  ReplaceText := RegExprUnEscape(editReplace.Text);
  memoReplace.Text := RegExprReplaceMatches(memoSearch.Text, SearchText,
    ReplaceText, Matches, 0, chkbIgnoreCase.Checked);
  for i := 0 to Length(Matches) div 2 - 1 do begin
    Index := Matches[i*2];
    Len   := Matches[i*2+1];
    lstbReplaceResult.Items.Add(IntToStr(Index) + '|' + IntToStr(Len));
  end;
  lablReplace.Caption := 'Replace Result - ' + IntToStr(i);
end;

end.

