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
    tabsScript               : TTabSheet;

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

    { Script }

    trevScript               : TTreeView;
    memoScript               : TMemo;
    chkbNameFilterSc         : TCheckBox;
    editNameFilterSc         : TEdit;
    bttnEditScript           : TButton;

    actlMain                 : TActionList;
    actnRename               : TAction;
    actnAddToFront           : TAction;
    actnAddToBehind          : TAction;
    actnAddToChildFirst      : TAction;
    actnAddToChildLast       : TAction;
    actnDeleteNode           : TAction;
    actnCut                  : TAction;
    actnCopy                 : TAction;
    actnPaste                : TAction;
    actnSelectAll            : TAction;
    actnDeleteText           : TAction;
    actnUndo                 : TAction;
    actnRedo                 : TAction;
    actnEditScript           : TAction;

    menuTree                 : TPopupMenu;
    pmiCut                   : TMenuItem;
    pmiCopy                  : TMenuItem;
    pmiPaste                 : TMenuItem;
    pmiSelectAll             : TMenuItem;
    pmiDeleteText            : TMenuItem;
    pmiUndo                  : TMenuItem;
    pmiRedo                  : TMenuItem;
    pmiSeparatorA01          : TMenuItem;
    pmiSeparatorA02          : TMenuItem;

    menuScript               : TPopupMenu;
    pmiRename                : TMenuItem;
    pmiAddToFront            : TMenuItem;
    pmiAddToBehind           : TMenuItem;
    pmiAddToChildFirst       : TMenuItem;
    pmiAddToChildLast        : TMenuItem;
    pmiDeleteNode            : TMenuItem;
    pmiSeparatorB01          : TMenuItem;

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

    procedure actnEditScriptExecute(Sender: TObject);
    procedure actnEditScriptUpdate(Sender: TObject);

    procedure actnRenameUpdate(Sender: TObject);
    procedure actnTextExecute(Sender: TObject);
    procedure actnTextUpdate(Sender: TObject);
    procedure memoScriptKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);

    procedure chkbNameFilterScChange(Sender: TObject);

    procedure actnRenameExecute(Sender: TObject);
    procedure treeEdited(Sender: TObject; Node: TTreeNode; var S: string);

    procedure actnAddNodeExecute(Sender: TObject);
    procedure actnDeleteNodeExecute(Sender: TObject);

    procedure treeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure treeDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure treeDragDrop(Sender, Source: TObject; X, Y: Integer);

    procedure ScriptMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

  private

    FHistory                 : THistory;
    FScriptChanged           : Boolean;

    procedure InitializeControls;

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
    procedure PerformScript;

    procedure SaveComboBoxData(var ComboBox: TComboBox; var ConfigItem: TStringList);
    function  CheckDepth: Boolean;

    procedure LoadScript;
    procedure ShowScriptEditor;
    procedure HideScriptEditor;

    function  GetNodeID(Node: TTreeNode): Integer; inline;
    procedure SetNodeID(Node: TTreeNode; ID: Integer); inline;

    procedure LoadTree(Tree: TTreeView);
    function  LoadNode(Tree: TTreeView; ID: Integer; ToNode: TTreeNode;
      Mode: TNodeAttachMode): TTreeNode;
    procedure LoadSubNodes(Tree: TTreeView; Node: TTreeNode);

    function AddNode(Tree: TTreeView; NodeName, NodeNote: string;
      ToNode: TTreeNode; Mode: TNodeAttachMode): TTreeNode;
    function DeleteNode(Node: TTreeNode): TTreeNode;

    function MoveNode(ToTree: TTreeView; Node, ToNode: TTreeNode;
      Mode: TNodeAttachMode): TTreeNode;
    function CopyNode(ToTree: TTreeView; Node, ToNode: TTreeNode;
      Mode: TNodeAttachMode): TTreeNode;

  end;

var
  formUtils: TformUtils;

implementation

uses
  uConfig, uCommon, uResources, uTreeDB, fMain;

var
  ScriptDB                 : TTreeDB;       // Alias | 别名
  FDragMode                : TNodeDragMode; // Alias | 别名

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

procedure TformUtils.InitializeControls;
var
  i: Integer;
begin
  trevScript.ReadOnly := True;
  memoScript.ReadOnly := True;
  memoScript.Hide;

  actnCut.OnUpdate := nil;
  for i := 0 to actlMain.ActionCount - 1 do begin
    (actlMain.Actions[i] as TAction).Enabled := False;
  end;

  actnEditScript.Caption := Res_CaptionEditScript;
  if (formMain.DBName <> ScriptFile) then
    LoadScript;

{$ifdef MSWINDOWS}
  memoScript.OnKeyDown := @memoScriptKeyDown;
{$endif}
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

  { Script }

  chkbNameFilterSc.Checked      := Config.UseNameFilterSc;
  editNameFilterSc.Enabled      := chkbNameFilterSc.Checked;
  editNameFilterSc.Text         := Config.NameFilterSc;

  trevScript.Options            := trevScript.Options - [tvoThemedDraw];
  trevScript.Font.Name          := formMain.trevTree.Font.Name;
  trevScript.Font.Size          := formMain.trevTree.Font.Size;
  trevScript.ExpandSignSize     := formMain.trevTree.ExpandSignSize;

  memoScript.Font.Name          := formMain.memoNote.Font.Name;
  memoScript.Font.Size          := formMain.trevTree.Font.Size;
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

  { Script }

  Config.UseNameFilterSc        := chkbNameFilterSc.Checked;
  Config.NameFilterSc           := editNameFilterSc.Text;

  if trevScript.Selected = nil then
    Config.LastScriptID           := 0
  else
    Config.LastScriptID           := GetNodeID(trevScript.Selected);
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

  case pgctMain.ActivePageIndex of
    0: PerformSort;
    1: PerformSplit;
    2: PerformRename;
    3: PerformScript;
  end;
end;

procedure TformUtils.CancelEvent;
begin
  Close;
end;

procedure TformUtils.PerformSort;
begin
  Hide;
  formMain.SortNode(radgSortOf.ItemIndex = 0, radgSortDirection.ItemIndex = 1);
  Close;
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

  Hide;
  formMain.SplitNote(Separator, Title, IncludeSeparator, PreNumLen);
  Close;
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

  Hide;
  formMain.RenameNodes(SearchText, NameText, SearchInNote, PreNumLen, Depth);
  Close;
end;

procedure TformUtils.PerformScript;
var
  ScriptID, Depth: Integer;
  Filter: string;
begin
  if trevScript.Selected = nil then Exit;

  ScriptID := GetNodeID(trevScript.Selected);

  Depth := AllDepth;

  if chkbNameFilterSc.Checked then
    Filter := editNameFilterSc.Text
  else
    Filter := '';

  if Depth = 0 then Exit;

  if (formMain.ActiveNode.Count > 0) and
  (Application.MessageBox(PChar(Res_MultiReplaceTip), PChar(AppTitle),
  MB_YESNO+MB_ICONQUESTION) <> IDYES) then
    Exit;

  Hide;
  HideScriptEditor;
  formMain.ExecuteScript(ScriptID, Depth, Filter, False);
  Close;
end;

{ ============================================================ }
{ Event 事件                                                   }
{ ============================================================ }

{ Form }

procedure TformUtils.FormCreate(Sender: TObject);
begin
  ScriptDB := formMain.ScriptDB;

  FScriptChanged := False;

  FHistory := THistory.Create(memoScript);

  SetWindowRect;
  SetWindowFont;

  LoadSettings;
  SwapOkCancel;

  InitializeControls;
end;

procedure TformUtils.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Config.UtilsFormRect     := BoundsRect;

  HideScriptEditor;
  ScriptDB.SaveDB;
  FHistory.Free;

  SaveSettings;

  CloseAction              := caFree;
  formUtils                := nil;

  if FScriptChanged then begin
    formMain.UnLoadScriptMenu;
    formMain.LoadScriptMenu;
  end;
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

{ Script }

procedure TformUtils.LoadScript;
var
  Node: TTreeNode;
begin
  if formMain.DBName = ScriptFile then Exit;

  LoadTree(trevScript);

  // Select the last node | 选择上次的节点
  for Node in trevScript.Items do
    if GetNodeID(Node) = Config.LastScriptID then begin
      Node.Selected := True;
      Break;
    end;
end;

procedure TformUtils.actnEditScriptExecute(Sender: TObject);
begin
  if tabsScript.Tag = 0 then
    ShowScriptEditor
  else
    HideScriptEditor;

  if tabsScript.Tag = 0 then
    actnEditScript.Caption := Res_CaptionEditScript
  else
    actnEditScript.Caption := Res_CaptionEndEdit;
end;

procedure TformUtils.actnEditScriptUpdate(Sender: TObject);
var
  AEnabled: Boolean;
begin
  AEnabled := (formMain.DBName <> ScriptFile) and
    (pgctMain.ActivePage = tabsScript) and
    (trevScript.Selected <> nil);
  actnEditScript .Enabled := AEnabled;

  // The update of actnEditScript is very timely, because it is bound to a visible button,
  // it is a good guarantee that the status of Undo and Redo is correct
  // actnEditScript 的更新非常及时，可以很好的保证 Undo 和 Redo 的状态正确
  AEnabled := AEnabled and memoScript.Visible;
  actnUndo       .Enabled := AEnabled and FHistory.CanUndo;
  actnRedo       .Enabled := AEnabled and FHistory.CanRedo;
end;

procedure TformUtils.ShowScriptEditor;
var
  ID: Integer;
begin
  if (trevScript.Selected = nil) or (tabsScript.Tag = 1) then Exit;
  tabsScript.Tag := 1;

  memoScript.ReadOnly := False;
  memoScript.Show;
  trevScript.Hide;
  actnCut.OnUpdate := @actnTextUpdate;
  for ID := 0 to actlMain.ActionCount - 1 do begin
    (actlMain.Actions[ID] as TAction).Enabled := True;
  end;

  ID := GetNodeID(trevScript.Selected);
  memoScript.Text := ScriptDB.GetNote(ID);
  // becareful the #13#10 in Windows TMemo and the #10 in database.
  // 小心 Windows TMemo 中的 #13#10 和数据库中的 #10。
  FHistory.SetHistoryData(nil, memoScript.Text);
end;

procedure TformUtils.HideScriptEditor;
var
  ID: Integer;
begin
  if (trevScript.Selected = nil) or (tabsScript.Tag = 0) then Exit;
  tabsScript.Tag := 0;

  memoScript.ReadOnly := True;
  memoScript.Hide;
  trevScript.Show;
  actnCut.OnUpdate := nil;
  for ID := 0 to actlMain.ActionCount - 1 do begin
    (actlMain.Actions[ID] as TAction).Enabled := False;
  end;

  ID := GetNodeID(trevScript.Selected);
  ScriptDB.SetNote(ID, ToLF(memoScript.Text));
  FHistory.SetHistoryData(nil, '');
end;

procedure TformUtils.chkbNameFilterScChange(Sender: TObject);
begin
  editNameFilterSc.Enabled := chkbNameFilterSc.Checked;
end;

procedure TformUtils.actnRenameUpdate(Sender: TObject);
var
  AEnabled: Boolean;
begin
  AEnabled := (formMain.DBName <> ScriptFile) and (pgctMain.ActivePage = tabsScript) and
    trevScript.Visible;
  actnAddToFront      .Enabled := AEnabled;
  actnAddToBehind     .Enabled := AEnabled;
  actnAddToChildFirst .Enabled := AEnabled;
  actnAddToChildlast  .Enabled := AEnabled;
  AEnabled := AEnabled and (trevScript.Selected <> nil);
  actnRename          .Enabled := AEnabled;
  actnDeleteNode      .Enabled := AEnabled;
end;

procedure TformUtils.actnTextExecute(Sender: TObject);
begin
  if actnEditScript.Enabled and (pgctMain.ActivePage = tabsScript) and memoScript.Visible then
    case (Sender as TAction).Name of
      'actnCut'        : memoScript.CutToClipboard;
      'actnCopy'       : memoScript.CopyToClipboard;
      'actnPaste'      : FHistory.PasteText;
      'actnSelectAll'  : memoScript.SelectAll;
      'actnDeleteText' : FHistory.DeleteText;
      'actnUndo'       : FHistory.Undo;
      'actnRedo'       : FHistory.Redo;
    end;
end;

procedure TformUtils.actnTextUpdate(Sender: TObject);
var
  AEnabled: Boolean;
begin
  AEnabled := actnEditScript.Enabled and (pgctMain.ActivePage = tabsScript) and memoScript.Visible;
  actnPaste      .Enabled := AEnabled and ClipBoard.HasFormat(CF_TEXT);
  actnSelectAll  .Enabled := AEnabled and memoScript.Focused;

  AEnabled := AEnabled and (memoScript.SelLength > 0);
  actnCut        .Enabled := AEnabled;
  actnCopy       .Enabled := AEnabled;
  actnDeleteText .Enabled := AEnabled;
end;

procedure TformUtils.memoScriptKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Disable Windows default Undo Redo shortcut.
  // 禁用 Windows 默认的 Undo Redo 快捷键
  if (ssCtrl in Shift) then begin
    case Key of
      90: begin  // Z
        // Disable Ctrl+Z, but pass Ctrl+Shift+Z
        // 禁用 Ctrl+Z，但放过 Ctrl+Shift+Z
        if ssShift in Shift then begin
          if not actnRedo.Enabled then
            Key := 0;
        end else if not actnUndo.Enabled then  // Disable Ctrl+Z | 禁用 Ctrl+Z
          Key := 0;
      end;
      89: begin  // Y
        if not actnRedo.Enabled then           // Disable Ctrl+Y | 禁用 Ctrl+Y
          Key := 0;
      end;
    end;
  end;
end;

procedure TformUtils.ScriptMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  Size: Integer;
begin
  if not (ssCtrl in Shift) then Exit;

  if ssShift in Shift then Size := 10 else Size := 1;
  if WheelDelta < 0 then Size := -Size;

  if (Sender as TControl).Font.Size = 0 then
    Size := DefFontSize + Size
  else
    Size := (Sender as TControl).Font.Size + Size;

  if Size <= 0 then Size := 1;

  case (Sender as TControl).Name of
    'trevScript': trevScript.Font.Size := Size;
    'memoScript': memoScript.Font.Size := Size;
  end;
end;

{ ============================================================ }
{ Node Data 节点数据                                           }
{ ============================================================ }

function TformUtils.GetNodeID(Node: TTreeNode): Integer; inline;
begin
  Result := Integer(Node.Data);
end;

procedure TformUtils.SetNodeID(Node: TTreeNode; ID: Integer); inline;
begin
  Node.Data := Pointer(ID);
end;

{ ============================================================ }
{ Load Node 加载节点                                           }
{ ============================================================ }

procedure TformUtils.LoadTree(Tree: TTreeView);
var
  ID: Integer;
  NewNode: TTreeNode;
begin
  for ID in ScriptDB.GetChildren(RootID) do begin
    NewNode := Tree.Items.AddChild(nil, ScriptDB.GetName(ID));
    SetNodeID(NewNode, ID);
    LoadSubNodes(Tree, NewNode);
  end;
end;

function TformUtils.LoadNode(Tree: TTreeView; ID: Integer; ToNode: TTreeNode;
  Mode: TNodeAttachMode): TTreeNode;
begin
  if ToNode = nil then Mode := naAdd;
  Result := Tree.Items.AddNode(nil, ToNode, ScriptDB.GetName(ID), nil, Mode);
  SetNodeID(Result, ID);
  LoadSubNodes(Tree, Result);
end;

procedure TformUtils.LoadSubNodes(Tree: TTreeView; Node: TTreeNode);
var
  NewNode: TTreeNode;

  procedure DoLoadSubNodes(ANode: TTreeNode);
  var
    ID: Integer;
  begin
    ID := GetNodeID(ANode);
    for ID in ScriptDB.GetChildren(ID) do begin
      NewNode := Tree.Items.AddChild(ANode, ScriptDB.GetName(ID));
      SetNodeID(NewNode, ID);
      DoLoadSubNodes(NewNode);
    end;
  end;

begin
  DoLoadSubNodes(Node);
end;

{ ============================================================ }
{ Add Node 添加节点                                            }
{ ============================================================ }

procedure TformUtils.actnAddNodeExecute(Sender: TObject);
var
  Mode: TNodeAttachMode;
  NewNode: TTreeNode;
begin
  if not trevScript.Visible then Exit;
  Mode := naInsert;
  case (Sender as TAction).Name of
    // 'actnAddToFront':      Mode := naInsert;
    'actnAddToBehind':     Mode := naInsertBehind;
    'actnAddToChildFirst': Mode := naAddChildFirst;
    'actnAddToChildLast':  Mode := naAddChild;
  end;
  NewNode := AddNode(trevScript, '', '', trevScript.Selected, Mode);
  NewNode.Selected := True;
  // Auto enter the "Rename" state | 自动进入“重命名”状态
  actnRenameExecute(nil);
end;

function TformUtils.AddNode(Tree: TTreeView; NodeName, NodeNote: string;
  ToNode: TTreeNode; Mode: TNodeAttachMode): TTreeNode;
var
  ID, ToID: Integer;
begin
  if NodeName = '' then NodeName := Res_UnNamedNode;
  if ToNode <> nil then
    ToID := GetNodeID(ToNode)
  else
    ToID := RootID;

  // Add node in database | 在数据库中添加节点
  ID := ScriptDB.AddNode(NodeName, NodeNote, ToID, Mode);

  // Add node in TreeView | 在 TreeView 中添加节点
  if ToNode = nil then Mode := naAdd;
  Result := Tree.Items.AddNode(nil, ToNode, NodeName, nil, Mode);
  SetNodeID(Result, ID);

  FScriptChanged := True;
end;

{ ============================================================ }
{ Delete Node 删除节点                                         }
{ ============================================================ }

procedure TformUtils.actnDeleteNodeExecute(Sender: TObject);
begin
  if (not trevScript.Visible) or (trevScript.Selected = nil) then Exit;
  if (Application.MessageBox(PChar(Res_DeleteNodeTip), PChar(AppTitle),
  MB_YESNO + MB_ICONQUESTION) <> IDYES) then
    Exit;
  DeleteNode(trevScript.Selected);
end;

function TformUtils.DeleteNode(Node: TTreeNode): TTreeNode;
begin
  Result := nil;
  if Node = nil then Exit;

  // Auto select the next node | 自动选中下一个节点
  Result := Node.GetNextSibling;
  if Result = nil then Result := Node.GetPrevSibling;
  if Result = nil then Result := Node.Parent;
  if Result <> nil then Result.Selected := True;

  // Remove node from database | 从数据库中删除节点
  ScriptDB.DeleteNode(GetNodeID(Node));

  // Remove node from TreeView | 从 TreeView 中删除节点
  Node.Delete;

  FScriptChanged := True;
end;

{ ============================================================ }
{ Rename Node 重命名节点                                       }
{ ============================================================ }

procedure TformUtils.actnRenameExecute(Sender: TObject);
begin
  if trevScript.Visible and (trevScript.Selected <> nil) then
    trevScript.Selected.EditText;
end;

procedure TformUtils.treeEdited(Sender: TObject; Node: TTreeNode; var S: string);
begin
  if S <> Node.Text then begin
    ScriptDB.SetName(GetNodeID(Node), S);
    FScriptChanged := True;
  end;
end;

{ ============================================================ }
{ Move Node 移动节点                                           }
{ ============================================================ }

function TformUtils.MoveNode(ToTree: TTreeView; Node, ToNode: TTreeNode;
  Mode: TNodeAttachMode): TTreeNode;
var
  ID, ToID: Integer;
begin
  Result := Node;
  if Node = nil then Exit;
  ID := GetNodeID(Node);

  if ToNode <> nil then begin
    // Not allowed to move into descendant node | 不允许移入子孙节点
    if ToNode.HasAsParent(Node) then Exit;

    ToID := GetNodeID(ToNode);
  end else
    ToID := RootID;

  // In this case, Mode must be naAdd, otherwise TreeView.Items.AddNode() will go wrong
  // 这种情况下，Mode 必须是 naAdd，否则 TreeView.Items.AddNode() 会出错
  if ToNode = nil then Mode := naAdd;

  // Mve node in database(No "move to descendant node" check turned on in the database)
  // 在数据库中移动节点（数据库中没有开启“移入子孙节点”的检查）
  if not ScriptDB.MoveNode(ID, ToID, Mode) then Exit;

  // Move node in TreeView | 在 TreeView 中移动节点
  if Node.TreeView <> ToTree then begin
    Result := LoadNode(ToTree, ID, ToNode, Mode);
    Node.Delete;
    FScriptChanged := True;
  end else if Node <> ToNode then begin
    Node.MoveTo(ToNode, Mode);
    FScriptChanged := True;
  end;
end;

{ ============================================================ }
{ Drag And Drop Node 拖拽节点                                  }
{ ============================================================ }

procedure TformUtils.treeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // ssCtrl means start drag using Move mode, ssAlt means start drag using Copy mode
  // ssCtrl 表示以移动模式开始拖动，ssAlt 表示以复制模式开始拖动
  if ((ssCtrl in Shift) or (ssAlt in Shift)) and (Button = mbLeft) and
    (htOnItem in (Sender as TTreeView).GetHitTestInfoAt(X, Y)) then
  begin
    (Sender as TTreeView).BeginDrag(False);
    FDragMode := dmMove;
    // Only trevTree is allowed to copy node | 只有 trevTree 可以复制节点
    if ssAlt in Shift then
      FDragMode := dmCopy
  end;
end;

procedure TformUtils.treeDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = trevScript);
end;

procedure TformUtils.treeDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Node, ToNode: TTreeNode;
  Mode: TNodeAttachMode;
  IntoChildren: Boolean;
begin
  Node := (Source as TTreeView).Selected;

  ToNode := (Sender as TTreeView).GetNodeAt(X, Y);
  Mode := naInsertBehind;

  // Check if the node is moved to child nodes.
  // If the Ctrl or Alt key is still hold down while dropping, move the node to the target node's sibling.
  // If the Ctrl or Alt key is release while dropping, move the node to the target node's child nodes.
  // 判断是否移入目标节点的子节点中
  // 如果在 Drop 的时候仍然按住 Ctrl 或 Alt 键不放，则将节点移动到目标节点的兄弟节点中
  // 如果在 Drop 的时候没有按住 Ctrl 或 Alt 键，则将节点移动到目标节点的子节点中
  IntoChildren := not (IsKeyDown(VK_CONTROL) or IsKeyDown(VK_MENU));

  // Move up, move to the front of target. Move down, move to the behind of target.
  // 向上移动，则移动到目标之前，向下移动，则移动到目标之后
  if (Sender = Source) and (ToNode <> nil) and (Node.AbsoluteIndex > ToNode.AbsoluteIndex) then
    Mode := naInsert;

  // If the Shift key is hold down while dropping, change the move mode to "insert to front".
  // 如果在 Drop 的时候按住 Shift 键不放，则将移动方式改为移动到目标之前
  if IsKeyDown(VK_SHIFT) then Mode := naInsert;

  // If move to child nodes, correct the move mode
  // 如果是移入子节点，则修正移动方式
  if IntoChildren then
    if Mode = naInsert then
      Mode := naAddChildFirst
    else
      Mode := naAddChild;

  if FDragMode = dmCopy then
    CopyNode(Sender as TTreeView, Node, ToNode, Mode).Selected := True
  else if Node <> ToNode then
    MoveNode(Sender as TTreeView, Node, ToNode, Mode).Selected := True;

  (Sender as TTreeView).SetFocus;
end;

{ ============================================================ }
{ Copy Node 复制节点                                           }
{ ============================================================ }

function TformUtils.CopyNode(ToTree: TTreeView; Node, ToNode: TTreeNode;
  Mode: TNodeAttachMode): TTreeNode;
var
  ID, ToID, NewID: Integer;
begin
  Result := nil;
  if Node = nil then Exit;
  ID := GetNodeID(Node);

  if ToNode <> nil then
    ToID := GetNodeID(ToNode)
  else
    ToID := RootID;

  // Copy the node in database | 在数据库中复制节点
  NewID := ScriptDB.CopyNode(ID, ToID, Mode);

  // Copy the node in TreeView | 在 TreeView 中复制节点
  Result := LoadNode(ToTree, NewID, ToNode, Mode);

  FScriptChanged := True;
end;

end.

