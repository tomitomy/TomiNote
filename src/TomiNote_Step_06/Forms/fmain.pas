unit fMain;

{$mode objfpc}{$H+}

// String constants and string literals require this option
// 字符串常量和字符串字面量需要此选项
{$codepage UTF8}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, Menus, ActnList, uTreeDB;

type

  PNodeData = ^TNodeData;

  TNodeData = record
    ID                       : Integer;
    // Other fields have not been implemented
    // 其它字段尚未实现
  end;

  TNodeDragMode = (dmMove, dmCopy);

  { TformMain }

  TformMain = class(TForm)
    openDlg1                 : TOpenDialog;
    saveDlg1                 : TSaveDialog;

    panlTree                 : TPanel;
    trevTree                 : TTreeView;
    spltTree                 : TSplitter;
    trevRecy                 : TTreeView;

    panlBottom               : TPanel;
    bttnNew                  : TButton;
    bttnOpen                 : TButton;
    bttnSave                 : TButton;
    bttnClose                : TButton;

    statMain                 : TStatusBar;

    actlMain                 : TActionList;
    actnNew                  : TAction;
    actnOpen                 : TAction;
    actnSave                 : TAction;
    actnClose                : TAction;
    actnRename               : TAction;
    actnAddToFront           : TAction;
    actnAddToBehind          : TAction;
    actnAddToChildFirst      : TAction;
    actnAddToChildLast       : TAction;
    actnDeleteNode           : TAction;
    actnRecycleNode          : TAction;
    actnRestoreNode          : TAction;
    actnEmptyRecycler        : TAction;
    actnMoveUp               : TAction;
    actnMoveDown             : TAction;
    actnMoveLeft             : TAction;
    actnMoveRight            : TAction;

    menuTree                 : TPopupMenu;
    pmiRename                : TMenuItem;
    pmiAddToFront            : TMenuItem;
    pmiAddToBehind           : TMenuItem;
    pmiAddToChildFirst       : TMenuItem;
    pmiAddToChildLast        : TMenuItem;
    pmiDeleteNode            : TMenuItem;
    pmiRecycleNode           : TMenuItem;
    pmiMoveUp                : TMenuItem;
    pmiMoveDown              : TMenuItem;
    pmiMoveLeft              : TMenuItem;
    pmiMoveRight             : TMenuItem;
    pmiSeparatorA01          : TMenuItem;
    pmiSeparatorA02          : TMenuItem;
    pmiSeparatorA03          : TMenuItem;

    menuRecy                 : TPopupMenu;
    pmiRenameRecy            : TMenuItem;
    pmiRestoreNode           : TMenuItem;
    pmiDeleteNodeRecy        : TMenuItem;
    pmiEmptyRecycler         : TMenuItem;
    pmiMoveUpRecy            : TMenuItem;
    pmiMoveDownRecy          : TMenuItem;
    pmiMoveLeftRecy          : TMenuItem;
    pmiMoveRightRecy         : TMenuItem;
    pmiSeparatorB01          : TMenuItem;
    pmiSeparatorB02          : TMenuItem;

    { Form 窗体 }

    procedure FormCreate(Sender: TObject);

  private

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure InitializeControls;

    { Control State 控件状态 }

    procedure actnRenameUpdate(Sender: TObject);

  private

    { Database File 数据库文件 }

    FTreeDB                  : TTreeDB;
    FDBName                  : string;
    FDBActive                : Boolean;
    FDBChanged               : Boolean;

    procedure SetDBActive(AValue: Boolean);
    procedure SetDBChanged(AValue: Boolean);

  published

    property DBActive: Boolean read FDBActive write SetDBActive;
    property DBChanged: Boolean read FDBChanged write SetDBChanged;

  private

    procedure actnNewExecute(Sender: TObject);
    procedure actnOpenExecute(Sender: TObject);
    procedure actnSaveExecute(Sender: TObject);
    procedure actnCloseExecute(Sender: TObject);

    function OpenDB(FileName: string): Boolean;
    function SaveDB: Boolean;
    function CloseDB(Save: Boolean): Boolean;

    { Load Node 加载节点 }

    procedure LoadTree(Tree: TTreeView);
    function  LoadNode(Tree: TTreeView; ID: Integer; ToNode: TTreeNode;
      Mode: TNodeAttachMode): TTreeNode;
    procedure LoadSubNodes(Tree: TTreeView; Node: TTreeNode);

    { UnLoad Node 卸载节点 }

    procedure UnloadTree(Tree: TTreeView);
    procedure UnLoadNode(Tree: TTreeView; Node: TTreeNode);
    procedure UnLoadSubNodes(Tree: TTreeView; Node: TTreeNode);

    { Node Data 节点数据 }

    function  GetNodeID(Node: TTreeNode): Integer; inline;
    procedure SetNodeID(Node: TTreeNode; AID: Integer);
    procedure FreeNodeData(Node: TTreeNode);

    { Add Node 添加节点 }

    procedure actnAddNodeExecute(Sender: TObject);
    function  AddNode(Tree: TTreeView; NodeName, NodeNote: string;
      ToNode: TTreeNode; Mode: TNodeAttachMode): TTreeNode;

    { Delete Node 删除节点 }

    procedure actnDeleteNodeExecute(Sender: TObject);
    function  DeleteNode(Node: TTreeNode): TTreeNode;

    { Rename Node 重命名节点 }

    procedure actnRenameExecute(Sender: TObject);
    procedure treeEdited(Sender: TObject; Node: TTreeNode; var S: string);

  private

    { Activate Node 激活节点 }

    FActiveTree              : TTreeView;
    FActiveNode              : TTreeNode;

    procedure SetActiveNode(AValue: TTreeNode);

  published

    property ActiveNode: TTreeNode read FActiveNode write SetActiveNode;

  private

    procedure treeSelectionChanged(Sender: TObject);
    procedure treeEnter(Sender: TObject);

    { Recycle And Restore Node 回收和恢复节点 }

    procedure actnRecycleNodeExecute(Sender: TObject);
    procedure actnRestoreNodeExecute(Sender: TObject);
    procedure actnEmptyRecyclerExecute(Sender: TObject);

    function  RecycleNode(Node: TTreeNode): TTreeNode;
    function  RestoreNode(Node, ToNode: TTreeNode): TTreeNode;
    procedure EmptyRecycler;

    { Move Node 移动节点 }

    procedure actnMoveUpExecute(Sender: TObject);
    procedure actnMoveDownExecute(Sender: TObject);
    procedure actnMoveLeftExecute(Sender: TObject);
    procedure actnMoveRightExecute(Sender: TObject);

    function  MoveNode(ToTree: TTreeView; Node, ToNode: TTreeNode;
      Mode: TNodeAttachMode): TTreeNode;

    procedure MoveNodeUp(Node: TTreeNode);
    procedure MoveNodeDown(Node: TTreeNode);
    procedure MoveNodeLeft(Node: TTreeNode);
    procedure MoveNodeRight(Node: TTreeNode);

  private

    { Drag And Drop Node 拖拽节点 }

    FDragMode                : TNodeDragMode;

    procedure treeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure treeDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure treeDragDrop(Sender, Source: TObject; X, Y: Integer);

    { Copy Node 复制节点 }

    function CopyNode(ToTree: TTreeView; Node, ToNode: TTreeNode;
      Mode: TNodeAttachMode): TTreeNode;

  end;

var
  formMain                 : TformMain;

implementation

uses
  uResources, LCLType, uCommon;

{$R *.lfm}

{$I fMainTreeDB.inc}

procedure TformMain.FormCreate(Sender: TObject);
begin
  InitializeControls;

  FTreeDB                  := TTreeDB.Create;

  FDBName                  := '';
  FDBActive                := False;
  FDBChanged               := False;

  FActiveTree              := nil;
  FActiveNode              := nil;
end;

procedure TformMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  actnCloseExecute(Sender);
  if FDBActive then CloseAction := caNone;
end;

procedure TformMain.FormDestroy(Sender: TObject);
begin
  FTreeDB.Free;
end;

procedure TformMain.FormResize(Sender: TObject);
begin
  trevRecy.Height := panlTree.Height * 30 div 100;
end;

procedure TformMain.InitializeControls;
begin
  Caption := Format('%s v%s', [AppTitle, AppVersion]);

  Width := 800; Height := 600;

  formMain                 .OnClose                 := @FormClose;
  formMain                 .OnDestroy               := @FormDestroy;
  formMain                 .OnResize                := @FormResize;

  openDlg1.InitialDir := ConcatPaths([AppDir, 'data']);
  saveDlg1.InitialDir := ConcatPaths([AppDir, 'data']);

  openDlg1.Filter := Res_DialogFilterTDB;
  saveDlg1.Filter := Res_DialogFilterTDB;

  actnNew                  .OnExecute               := @actnNewExecute;
  actnOpen                 .OnExecute               := @actnOpenExecute;
  actnSave                 .OnExecute               := @actnSaveExecute;
  actnClose                .OnExecute               := @actnCloseExecute;

  actnAddToFront           .OnExecute               := @actnAddNodeExecute;
  actnAddToBehind          .OnExecute               := @actnAddNodeExecute;
  actnAddToChildFirst      .OnExecute               := @actnAddNodeExecute;
  actnAddToChildLast       .OnExecute               := @actnAddNodeExecute;
  actnDeleteNode           .OnExecute               := @actnDeleteNodeExecute;
  actnRecycleNode          .OnExecute               := @actnRecycleNodeExecute;
  actnRestoreNode          .OnExecute               := @actnRestoreNodeExecute;
  actnEmptyRecycler        .OnExecute               := @actnEmptyRecyclerExecute;

  actnRename               .OnExecute               := @actnRenameExecute;
  trevTree                 .OnEdited                := @treeEdited;
  trevRecy                 .OnEdited                := @treeEdited;

  actnMoveUp               .OnExecute               := @actnMoveUpExecute;
  actnMoveDown             .OnExecute               := @actnMoveDownExecute;
  actnMoveLeft             .OnExecute               := @actnMoveLeftExecute;
  actnMoveRight            .OnExecute               := @actnMoveRightExecute;

  actnRename               .OnUpdate                := @actnRenameUpdate;

  trevTree.ReadOnly := True;
  trevTree.RightClickSelect := True;
  // Must be empty before database opened | 打开数据库前必须为空
  trevTree.Items.Clear;

  trevRecy.ReadOnly := True;
  trevRecy.RightClickSelect := True;
  // Must be empty before database opened | 打开数据库前必须为空
  trevRecy.Items.Clear;

  trevTree                 .OnSelectionChanged      := @treeSelectionChanged;
  trevRecy                 .OnSelectionChanged      := @treeSelectionChanged;
  trevTree                 .OnEnter                 := @treeEnter;
  trevRecy                 .OnEnter                 := @treeEnter;

  trevTree                 .OnMouseDown             := @treeMouseDown;
  trevTree                 .OnDragOver              := @treeDragOver;
  trevTree                 .OnDragDrop              := @treeDragDrop;

  trevRecy                 .OnMouseDown             := @treeMouseDown;
  trevRecy                 .OnDragOver              := @treeDragOver;
  trevRecy                 .OnDragDrop              := @treeDragDrop;

  trevTree.Enabled := False;
  trevRecy.Enabled := False;
end;

end.

