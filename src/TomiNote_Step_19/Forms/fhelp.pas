unit fhelp;

{$mode objfpc}{$H+}

// String constants and string literals require this option
// 字符串常量和字符串字面量需要此选项
{$codepage UTF8}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, LCLType, uTreeDB, LCLTranslator;

type

  { TformHelp }

  TformHelp = class(TForm)
    panlPlain                : TPanel;
    panlTree                 : TPanel;

    memoPlain                : TMemo;

    memoTree                 : TMemo;
    spltTree                 : TSplitter;
    trevTree                 : TTreeView;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormActivate(Sender: TObject);
  private
    FTreeDB: TTreeDB;

    procedure InitializeControls;
    procedure SplitNote(Note, Splitter, TitleSearch, TitleReplace: string);

    procedure treeSelectionChanged(Sender: TObject);
    procedure MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint; var Handled: Boolean);

    function  GetNodeID(Node: TTreeNode): Integer;
    procedure SetNodeID(Node: TTreeNode; ID: Integer);
    function  AddNode(Tree: TTreeView; NodeName, NodeNote: string;
      ToNode: TTreeNode; Mode: TNodeAttachMode): TTreeNode;
  public
    IsAbout: Boolean;
  end;

var
  formHelp: TformHelp;

implementation

uses
  fMain, uConfig, uCommon, RegExpr, uResources;

{$R *.lfm}

{ TformHelp }

procedure TformHelp.InitializeControls;
begin
  panlPlain.Align          := alClient;
  panlTree.Align           := alClient;

  // Remove the tvoThemedDraw option to modify TreeView's color
  // 去掉 tvoThemedDraw 选项以修改 TreeView 的颜色
  trevTree.Options         := trevTree.Options - [tvoThemedDraw];

  trevTree.Width           := Width * 30 div 100;

  memoPlain.ReadOnly       := True;
  trevTree .ReadOnly       := True;
  memoTree .ReadOnly       := True;

  memoPlain.BorderStyle    := bsNone;
  trevTree .BorderStyle    := bsNone;
  memoTree .BorderStyle    := bsNone;

  memoPlain.Font.Name      := formMain.memoNote.Font.Name;
  trevTree .Font.Name      := formMain.trevTree.Font.Name;
  memoTree .Font.Name      := formMain.memoNote.Font.Name;

  memoPlain.Font.Size      := formMain.memoNote.Font.Size;
  trevTree .Font.Size      := formMain.trevTree.Font.Size;
  memoTree .Font.Size      := formMain.memoNote.Font.Size;

  if Config.ActiveTheme = DarkThemeID then begin
    memoPlain.Font.Color     := Config.DarkForeColor;
    trevTree .Font.Color     := Config.DarkForeColor;
    memoTree .Font.Color     := Config.DarkForeColor;

    panlPlain.Color          := Config.DarkBackColor;
    panlTree.Color           := Config.DarkBackColor;
    memoPlain.Color          := Config.DarkBackColor;
    trevTree .Color          := Config.DarkBackColor;
    memoTree .Color          := Config.DarkBackColor;
  end else begin
    memoPlain.Font.Color     := Config.BrightForeColor;
    trevTree .Font.Color     := Config.BrightForeColor;
    memoTree .Font.Color     := Config.BrightForeColor;

    panlPlain.Color          := Config.BrightBackColor;
    panlTree.Color           := Config.BrightBackColor;
    memoPlain.Color          := Config.BrightBackColor;
    trevTree .Color          := Config.BrightBackColor;
    memoTree .Color          := Config.BrightBackColor;
  end;

  trevTree                 .OnSelectionChanged      := @treeSelectionChanged;
  memoPlain                .OnMouseWheel            := @MouseWheel;
  trevTree                 .OnMouseWheel            := @MouseWheel;
  memoTree                 .OnMouseWheel            := @MouseWheel;
end;

procedure TformHelp.FormCreate(Sender: TObject);
begin
  FTreeDB := nil;
  IsAbout := False;
  InitializeControls;
end;

procedure TformHelp.FormShow(Sender: TObject);
var
  i, MaxWidth: Integer;
begin
  panlPlain.Show;
  panlTree.Hide;
  if IsAbout then begin
    Width := 640; Height := 480;
    Caption := Res_AboutTitle;
    memoPlain.Text := Res_AboutContent;
  end else begin
    Width := 800; Height := 600;
    Caption := Res_HelpTitle;
    if IsKeyDown(VK_SHIFT) then begin
      // Load About content | 加载关于内容
      memoPlain.Text := Res_HelpContent;
    end else begin
      panlPlain.Hide;
      panlTree.Show;
      FTreeDB := TTreeDB.Create;
      FTreeDB.OpenDB(':memory:');
      // Load Help content | 加载帮助内容
      SplitNote(Res_HelpContent, '(?m)^\[ --- .*? --- \]$', '(?m)^\[ --- (.*?) --- \]$', '$1');
      if trevTree.Items.Count > 0 then begin
        trevTree.Items[0].Selected := True;
        // Adjust TreeView Width | 调整 TreeView 宽度
        MaxWidth := 0;
        for i := 0 to trevTree.Items.Count - 1 do begin
          if MaxWidth < trevTree.Items[i].DisplayTextRight then
            MaxWidth := trevTree.Items[i].DisplayTextRight;
        end;
        MaxWidth := MaxWidth + trevTree.ExpandSignSize + 16;
        if MaxWidth > trevTree.Width then
          trevTree.Width := MaxWidth;
      end;
    end;
  end;
  Left := (Screen.Width - Width) div 2;
  Top  := (Screen.Height - Height) div 2;
end;

procedure TformHelp.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if FTreeDB <> nil then begin
    FTreeDB.CloseDB(False, False);
    FTreeDB.Free;
  end;
  CloseAction := caFree;
  formHelp := nil;
end;

procedure TformHelp.FormActivate(Sender: TObject);
begin
  formMain.actnCutUpdate(Sender);
end;

procedure TformHelp.treeSelectionChanged(Sender: TObject);
begin
  if trevTree.Selected = nil then
    memoTree.Text := ''
  else
    memoTree.Text := FTreeDB.GetNote(GetNodeID(trevTree.Selected));
end;

procedure TformHelp.MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
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
    'memoPlain': memoPlain.Font.Size := Size;
    'trevTree' : trevTree .Font.Size := Size;
    'memoTree' : memoTree .Font.Size := Size;
  end;
end;

{ ============================================================ }
{ Node Data 节点数据                                           }
{ ============================================================ }

function TformHelp.GetNodeID(Node: TTreeNode): Integer; inline;
begin
  Result := Integer(Node.Data);
end;

procedure TformHelp.SetNodeID(Node: TTreeNode; ID: Integer); inline;
begin
  Node.Data := Pointer(ID);
end;

{ ============================================================ }
{ Add Node 添加节点                                            }
{ ============================================================ }

function TformHelp.AddNode(Tree: TTreeView; NodeName, NodeNote: string;
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
  ID := FTreeDB.AddNode(NodeName, NodeNote, ToID, Mode);

  // Add node in TreeView | 在 TreeView 中添加节点
  if ToNode = nil then Mode := naAdd;
  Result := Tree.Items.AddNode(nil, ToNode, NodeName, nil, Mode);
  SetNodeID(Result, ID);
end;

procedure TformHelp.SplitNote(Note, Splitter, TitleSearch, TitleReplace: string);
var
  SubNoteStart, SubNoteLen: Integer;
  SubExpr: TRegExpr;
  SubNote, SubTitle: string;

  procedure SplitOneNote;
  begin
    // Get the child Note | 获取子笔记
    SubNote := Copy(Note, SubNoteStart, SubNoteLen);
    SubNote := TrimBlank(SubNote);
    if SubNote = '' then Exit;
    SubNote := Format(#10'%s'#10#10#10, [SubNote]);

    // Get the child name | 获取子名称
    if SubExpr.Exec(SubNote) then
      SubTitle := RegExpr.ReplaceRegExpr(TitleSearch, SubExpr.Match[0], TitleReplace, True)
    else
      SubTitle := Res_UnnamedNode;

    AddNode(trevTree, SubTitle, SubNote, nil, naAddChild);
  end;

var
  Expr: TRegExpr;
  Found: Boolean;
  PrevMatchPos: SizeInt;
begin
  if (Splitter = '') or (TitleSearch = '') then Exit;

  Expr := TRegExpr.Create(Splitter);

  SubExpr := TRegExpr.Create(TitleSearch);

  SubNoteStart := 1;
  PrevMatchPos := 1;

  // Find the separator | 查找分隔符
  Found := Expr.Exec(Note);
  while Found do begin
    // Get the length of the child Note | 获取子笔记的长度
    SubNoteLen := UTF8PosToBytePos(PChar(Note) + SubNoteStart - 1,
      Length(Note) - SubNoteStart + 1, Expr.MatchPos[0] - PrevMatchPos + 1) - 1;

    // Create child node | 创建子节点
    SplitOneNote;

    // Calculate the position of the next child Note | 计算下一个子笔记的位置

    SubNoteStart := SubNoteStart + SubNoteLen;
    PrevMatchPos := Expr.MatchPos[0];

    // Find the next separator | 查找下一个分隔符
    Found := Expr.ExecNext;
  end;
  // Handle the content behind the last separator
  // 处理最后一个分隔符之后的内容
  SubNoteLen := Length(Note) - SubNoteStart + 1;
  SplitOneNote;

  SubExpr.Free;
  Expr.Free;
end;

end.

