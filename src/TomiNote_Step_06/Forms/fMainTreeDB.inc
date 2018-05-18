
{ ============================================================ }
{ Control State 控件状态                                       }
{ ============================================================ }

procedure TformMain.actnRenameUpdate(Sender: TObject);
var
  AEnabled: Boolean;
begin
  AEnabled := (trevTree.Selected <> nil) and (FActiveTree = trevTree);
  actnRecycleNode          .Enabled := AEnabled;

  AEnabled := FActiveNode <> nil;
  actnRename               .Enabled := AEnabled;
  actnDeleteNode           .Enabled := AEnabled;
  actnMoveUp               .Enabled := AEnabled;
  actnMoveDown             .Enabled := AEnabled;
  actnMoveLeft             .Enabled := AEnabled;
  actnMoveRight            .Enabled := AEnabled;

  AEnabled := FDBActive and (FActiveTree = trevTree);
  actnAddToFront           .Enabled := AEnabled;
  actnAddToBehind          .Enabled := AEnabled;
  actnAddToChildFirst      .Enabled := AEnabled;
  actnAddToChildLast       .Enabled := AEnabled;

  actnRestoreNode          .ENabled := trevRecy.Selected <> nil;
  actnEmptyRecycler        .Enabled := trevRecy.Items.Count > 0;
end;

{ ============================================================ }
{ Database File 数据库文件                                     }
{ ============================================================ }

procedure TformMain.SetDBActive(AValue: Boolean);
begin
  if FDBActive = AValue then Exit;
  FDBActive := AValue;

  actnClose                .Enabled := FDBActive;
end;

procedure TformMain.SetDBChanged(AValue: Boolean);
begin
  if FDBChanged = AValue then Exit;
  FDBChanged := AValue;

  actnSave                 .Enabled := FDBChanged;
end;

procedure TformMain.actnNewExecute(Sender: TObject);
var
  FileName: string;
begin
  if FDBName <> '' then
    saveDlg1.InitialDir := ExtractFileDir(FDBName);
  saveDlg1.FileName := '';

  if not saveDlg1.Execute then Exit;

  FileName := saveDlg1.FileName;

  // Add file extension | 添加文件扩展名
  if ExtractFileExt(FileName) = '' then
    FileName := ChangeFileExt(FileName, DBFileExt);

  // Overwrite File warning | “文件覆盖”警告
  if FileExists(FileName) and
    (Application.MessageBox(PChar(Res_OverwriteFileTip), PChar(AppTitle),
    MB_YESNO + MB_ICONINFORMATION) <> IDYES) then
    Exit;

  actnCloseExecute(Sender);
  if FDBActive then Exit;

  // Do overwrite file | 执行文件覆盖
  if FileExists(FileName) then DeleteFile(FileName);
  if FileExists(FileName) then begin
    Application.MessageBox(PChar(Res_OverwriteFileFail), PChar(AppTitle), MB_OK + MB_ICONERROR);
    Exit;
  end;

  if not FTreeDB.OpenDB(FileName) then begin
    Application.MessageBox(PChar(Res_CreateDBFail), PChar(AppTitle), MB_OK + MB_ICONERROR);
    Exit;
  end;

  // Enter active state | 进入活动状态
  FDBName := FileName;
  DBActive := True;
  DBChanged := False;
  trevTree.Enabled := True;
  trevRecy.Enabled := True;

  Caption := Format('%s v%s - [%s]', [AppTitle, AppVersion, ExtractFileName(FDBName)]);
end;

procedure TformMain.actnOpenExecute(Sender: TObject);
var
  FileName: string;
begin
  if FDBName <> '' then
    openDlg1.InitialDir := ExtractFileDir(FDBName);
  openDlg1.FileName := '';

  if not openDlg1.Execute then Exit;

  FileName := openDlg1.FileName;

  if not FileExists(FileName) then Exit;

  actnCloseExecute(Sender);
  if FDBActive then Exit;

  OpenDB(FileName);
end;

function TformMain.OpenDB(FileName: string): Boolean;
begin
  Result := False;

  if FDBActive then Exit;

  if not FileExists(FileName) then Exit;

  if not FTreeDB.OpenDB(FileName) then begin
    Application.MessageBox(PChar(Res_OpenDBFail), PChar(AppTitle), MB_OK + MB_ICONERROR);
    Exit;
  end;

  if FTreeDB.Config.DBVersion <> DatabaseVersion then begin
    FTreeDB.CloseDB(False, False);
    Application.MessageBox(PChar(Res_DBVersionError), PChar(AppTitle), MB_OK + MB_ICONERROR);
    Exit;
  end;

  LoadTree(trevTree);
  LoadTree(trevRecy);

  // Enter active state | 进入活动状态
  FDBName := FileName;
  DBActive := True;
  DBChanged := False;
  trevTree.Enabled := True;
  trevRecy.Enabled := True;

  FActiveTree := nil;
  treeEnter(trevTree);

  Caption := Format('%s v%s - [%s]', [AppTitle, AppVersion, ExtractFileName(FDBName)]);

  Result := True;
end;

procedure TformMain.actnSaveExecute(Sender: TObject);
begin
  SaveDB;
end;

function TformMain.SaveDB: Boolean;
begin
  Result := False;

  if not FDBChanged then Exit;

  if not FTreeDB.SaveDB then begin
    Application.MessageBox(PChar(Res_SaveDBFail), PChar(AppTitle), MB_OK + MB_ICONERROR);
    Exit;
  end;

  DBChanged := False;
  Result := True;
end;

procedure TformMain.actnCloseExecute(Sender: TObject);
var
  Save: Boolean;
begin
  Save := False;
  if FDBChanged then begin
    case Application.MessageBox(PChar(Res_SaveDataTip), PChar(AppTitle),
    MB_YESNOCANCEL + MB_ICONQUESTION) of
      IDYes    : Save := True;
      IDCancel : Exit;
    end;
  end;

  CloseDB(Save);
end;

function TformMain.CloseDB(Save: Boolean): Boolean;
begin
  Result := False;

  if not FDBActive then Exit;
  if not FDBChanged then Save := False;

  if not FTreeDB.CloseDB(Save) then begin
    Application.MessageBox(PChar(Res_CloseDBFail), PChar(AppTitle), MB_OK + MB_ICONERROR);
    Exit;
  end;

  // Enter inactive state | 进入非活动状态
  FDBName := '';
  DBActive := False;
  DBChanged := False;
  // Defocused TreeView | 移除 TreeView 的焦点
  trevTree.Enabled := False;
  trevRecy.Enabled := False;

  UnloadTree(trevTree);
  UnloadTree(trevRecy);

  Caption := Format('%s v%s', [AppTitle, AppVersion]);

  Result := True;
end;

{ ============================================================ }
{ Load Node 加载节点                                           }
{ ============================================================ }

procedure TformMain.LoadTree(Tree: TTreeView);
var
  ID: Integer;
  NewNode: TTreeNode;
begin
  if Tree = trevTree then ID := RootID else ID := RecyclerID;

  for ID in FTreeDB.GetChildren(ID) do begin
    NewNode := Tree.Items.AddChild(nil, FTreeDB.GetName(ID));
    SetNodeID(NewNode, ID);

    LoadSubNodes(Tree, NewNode);
  end;
end;

function TformMain.LoadNode(Tree: TTreeView; ID: Integer; ToNode: TTreeNode;
  Mode: TNodeAttachMode): TTreeNode;
begin
  if ToNode = nil then Mode := naAdd;

  Result := Tree.Items.AddNode(nil, ToNode, FTreeDB.GetName(ID), nil, Mode);
  SetNodeID(Result, ID);

  LoadSubNodes(Tree, Result);
end;

procedure TformMain.LoadSubNodes(Tree: TTreeView; Node: TTreeNode);
var
  NewNode: TTreeNode;

  procedure DoLoadSubNodes(ANode: TTreeNode);
  var
    ID: Integer;
  begin
    ID := GetNodeID(ANode);
    for ID in FTreeDB.GetChildren(ID) do begin
      NewNode := Tree.Items.AddChild(ANode, FTreeDB.GetName(ID));
      SetNodeID(NewNode, ID);
      DoLoadSubNodes(NewNode);
    end;
  end;

begin
  DoLoadSubNodes(Node);
end;

{ ============================================================ }
{ UnLoad Node 卸载节点                                         }
{ ============================================================ }

procedure TformMain.UnloadTree(Tree: TTreeView);
begin
  UnLoadSubNodes(Tree, nil);
end;

procedure TformMain.UnLoadNode(Tree: TTreeView; Node: TTreeNode);
begin
  UnLoadSubNodes(Tree, Node);
  if Node <> nil then begin
    if FActiveNode = Node then ActiveNode := nil;
    FreeNodeData(Node);
    Node.Delete;
  end;
end;

procedure TformMain.UnLoadSubNodes(Tree: TTreeView; Node: TTreeNode);
var
  StartNode: TTreeNode;
  EndNode: TTreeNode;
begin
  StartNode := nil;
  EndNode := nil;

  // Get the range of nodes that need to be traversed
  // 获取需要遍历的节点范围
  if Node = nil then begin
    if Tree.Items.Count > 0 then
      StartNode := Tree.Items[0];
  end else begin
    if Node.Count > 0 then begin
      StartNode := Node.Items[0];
      EndNode := Node.GetNextSkipChildren;
    end;
  end;

  // Traverse nodes and clear their Data info
  // 遍历节点并清除其 Data 信息
  while StartNode <> EndNode do begin
    if FActiveNode = StartNode then ActiveNode := nil;
    FreeNodeData(StartNode);
    StartNode := StartNode.GetNext;
  end;

  if Node = nil then
    Tree.Items.Clear
  else
    Node.DeleteChildren;
end;

{ ============================================================ }
{ Node Data 节点数据                                           }
{ ============================================================ }

function TformMain.GetNodeID(Node: TTreeNode): Integer; inline;
begin
  Result := PNodeData(Node.Data)^.ID;
end;

procedure TformMain.SetNodeID(Node: TTreeNode; AID: Integer);
begin
  if Node.Data = nil then Node.Data := new(PNodeData);
  PNodeData(Node.Data)^.ID := AID;
end;

procedure TformMain.FreeNodeData(Node: TTreeNode);
begin
  if Node <> nil then begin
    dispose(PNodeData(Node.Data));
  end;
end;

{ ============================================================ }
{ Add Node 添加节点                                            }
{ ============================================================ }

procedure TformMain.actnAddNodeExecute(Sender: TObject);
var
  Mode: TNodeAttachMode;
  NewNode: TTreeNode;
begin
  Mode := naInsert;
  case (Sender as TAction).Name of
    // 'actnAddToFront':      Mode := naInsert;
    'actnAddToBehind':     Mode := naInsertBehind;
    'actnAddToChildFirst': Mode := naAddChildFirst;
    'actnAddToChildLast':  Mode := naAddChild;
  end;
  NewNode := AddNode(trevTree, '', '', trevTree.Selected, Mode);
  NewNode.Selected := True;
  // Auto enter the "Rename" state | 自动进入“重命名”状态
  actnRenameExecute(nil);
end;

function TformMain.AddNode(Tree: TTreeView; NodeName, NodeNote: string;
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

  DBChanged := True;
end;

{ ============================================================ }
{ Delete Node 删除节点                                         }
{ ============================================================ }

procedure TformMain.actnDeleteNodeExecute(Sender: TObject);
begin
  if (FActiveNode <> nil) and
  (Application.MessageBox(PChar(Res_DeleteNodeTip), PChar(AppTitle),
  MB_YESNO+MB_ICONQUESTION) = IDYES) then
    DeleteNode(FActiveNode);
end;

function TformMain.DeleteNode(Node: TTreeNode): TTreeNode;
begin
  Result := nil;
  if Node = nil then Exit;

  // Auto select the next node | 自动选中下一个节点
  Result := Node.GetNextSibling;
  if Result = nil then Result := Node.GetPrevSibling;
  if Result = nil then Result := Node.Parent;
  if Result <> nil then Result.Selected := True;

  // Remove node from database | 从数据库中删除节点
  FTreeDB.DeleteNode(GetNodeID(Node));

  // Remove node from TreeView | 从 TreeView 中删除节点
  UnLoadNode(Node.TreeView as TTreeView, Node);

  DBChanged := True;
end;

{ ============================================================ }
{ Rename Node 重命名节点                                       }
{ ============================================================ }

procedure TformMain.actnRenameExecute(Sender: TObject);
begin
  if FActiveNode <> nil then
    FActiveNode.EditText;
end;

procedure TformMain.treeEdited(Sender: TObject; Node: TTreeNode; var S: string);
begin
  if S <> Node.Text then begin
    FTreeDB.SetName(GetNodeID(Node), S);
    DBChanged := True;
  end;
end;

{ ============================================================ }
{ Activate Node 激活节点                                       }
{ ============================================================ }

procedure TformMain.SetActiveNode(AValue: TTreeNode);
begin
  if FActiveNode = AValue then Exit;
  FActiveNode := AValue;
end;

procedure TformMain.treeSelectionChanged(Sender: TObject);
begin
  ActiveNode := (Sender as TTreeView).Selected;
  if FActiveNode <> nil then
    statMain.Panels[2].Text := Format(Res_ChildNodesInfo, [FActiveNode.Count])
  else
    statMain.Panels[2].Text := '';
end;

procedure TformMain.treeEnter(Sender: TObject);
begin
  if FActiveTree <> Sender then begin
    FActiveTree := (Sender as TTreeView);

    if (FActiveTree.Selected = nil) and (FActiveTree.Items.Count > 0) then
      FActiveTree.Items[0].Selected := True
    else
      treeSelectionChanged(Sender);
  end;
end;

{ ============================================================ }
{ Recycle And Restore Node 回收和恢复节点                      }
{ ============================================================ }

procedure TformMain.actnRecycleNodeExecute(Sender: TObject);
begin
  RecycleNode(trevTree.Selected);
end;

function TformMain.RecycleNode(Node: TTreeNode): TTreeNode;
begin
  Result := nil;
  if Node = nil then Exit;

  // Auto select the next node | 自动选中下一个节点
  Result := Node.GetNextSibling;
  if Result = nil then Result := Node.GetPrevSibling;
  if Result = nil then Result := Node.Parent;
  if Result <> nil then Result.Selected := True;

  MoveNode(trevRecy, Node, nil, naAddChild);
end;

procedure TformMain.actnRestoreNodeExecute(Sender: TObject);
begin
  RestoreNode(trevRecy.Selected, trevTree.Selected);
end;

function TformMain.RestoreNode(Node, ToNode: TTreeNode): TTreeNode;
begin
  Result := nil;
  if Node = nil then Exit;

  // Auto select the next node | 自动选中下一个节点
  Result := Node.GetNextSibling;
  if Result = nil then Result := Node.GetPrevSibling;
  if Result = nil then Result := Node.Parent;
  if Result <> nil then Result.Selected := True;

  MoveNode(trevTree, Node, ToNode, naInsertBehind);
end;

procedure TformMain.actnEmptyRecyclerExecute(Sender: TObject);
begin
  if (Application.MessageBox(PChar(Res_EmptyRecyclerTip), PChar(AppTitle),
  MB_YESNO+MB_ICONQUESTION) = IDYES) then
    EmptyRecycler;
end;

procedure TformMain.EmptyRecycler;
begin
  if trevRecy.Items.Count = 0 then Exit;

  FTreeDB.EmptyRecycler;
  UnLoadTree(trevRecy);

  DBChanged := True;
end;

{ ============================================================ }
{ Move Node 移动节点                                           }
{ ============================================================ }

function TformMain.MoveNode(ToTree: TTreeView; Node, ToNode: TTreeNode;
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
  end else if ToTree = trevTree then
    ToID := RootID
  else
    ToID := RecyclerID;

  // In this case, Mode must be naAdd, otherwise TreeView.Items.AddNode() will go wrong
  // 这种情况下，Mode 必须是 naAdd，否则 TreeView.Items.AddNode() 会出错
  if ToNode = nil then Mode := naAdd;

  // Mve node in database(No "move to descendant node" check turned on in the database)
  // 在数据库中移动节点（数据库中没有开启“移入子孙节点”的检查）
  if not FTreeDB.MoveNode(ID, ToID, Mode) then Exit;

  // Move node in TreeView | 在 TreeView 中移动节点
  if Node.TreeView <> ToTree then begin
    Result := LoadNode(ToTree, ID, ToNode, Mode);
    UnLoadNode(TTreeView(Node.TreeView), Node);
    DBChanged := True;
  end else if Node <> ToNode then begin
    Node.MoveTo(ToNode, Mode);
    DBChanged := True;
  end;
end;

procedure TformMain.actnMoveUpExecute(Sender: TObject);
begin
  MoveNodeUp(FActiveNode);
end;

procedure TformMain.MoveNodeUp(Node: TTreeNode);
var
  ToNode: TTreeNode;
  Mode: TNodeAttachMode;
begin
  if Node = nil then Exit;
  Node.Expanded := False;

  FTreeDB.MoveNodeUp(GetNodeID(Node));

  ToNode := Node.GetPrevSibling;
  Mode := naInsert;
  // Circular move | 循环移动
  if ToNode = nil then begin
    // ToNode will not be nil | ToNode 不会是 nil
    ToNode := Node.GetLastSibling;
    Mode := naInsertBehind;
  end;

  if Node <> ToNode then begin
    Node.MoveTo(ToNode, Mode);
    DBChanged := True;
  end;
end;

procedure TformMain.actnMoveDownExecute(Sender: TObject);
begin
  MoveNodeDown(FActiveNode);
end;

procedure TformMain.MoveNodeDown(Node: TTreeNode);
var
  ToNode: TTreeNode;
  Mode: TNodeAttachMode;
begin
  if Node = nil then Exit;
  Node.Expanded := False;

  FTreeDB.MoveNodeDown(GetNodeID(Node));

  ToNode := Node.GetNextSibling;
  Mode := naInsertBehind;
  // Circular move | 循环移动
  if ToNode = nil then begin
    if Node.Parent = nil then
      ToNode := Node.TreeView.Items[0]
    else
      // ToNode will not be nil | ToNode 不会是 nil
      ToNode := Node.Parent.GetFirstChild;
    Mode := naInsert;
  end;

  if Node <> ToNode then begin
    Node.MoveTo(ToNode, Mode);
    DBChanged := True;
  end;
end;

procedure TformMain.actnMoveLeftExecute(Sender: TObject);
begin
  MoveNodeLeft(FActiveNode);
end;

procedure TformMain.MoveNodeLeft(Node: TTreeNode);
var
  NextNode: TTreeNode;
  NeedExpand: Boolean;
begin
  if Node = nil then Exit;
  NeedExpand := False;

  if FTreeDB.MoveNodeLeft(GetNodeID(Node)) then begin
    Node.TreeView.BeginUpdate;

    NextNode := Node.GetNextSibling;
    // If new nodes are recruited, show them
    // 如果吸收了新节点，则将其展示出来
    NeedExpand := NextNode <> nil;
    while NextNode <> nil do begin
      NextNode.MoveTo(Node, naAddChild);
      NextNode := Node.GetNextSibling;
    end;
    Node.MoveTo(Node.Parent, naInsertBehind);
    if NeedExpand then Node.Expanded := True;

    Node.TreeView.EndUpdate;
    DBChanged := True;
  end;
end;

procedure TformMain.actnMoveRightExecute(Sender: TObject);
begin
  MoveNodeRight(FActiveNode);
end;

procedure TformMain.MoveNodeRight(Node: TTreeNode);
var
  ChildNode: TTreeNode;
begin
  if Node = nil then Exit;

  if FTreeDB.MoveNodeRight(GetNodeID(Node), Node.Expanded) then begin
    Node.TreeView.BeginUpdate;

    Node.MoveTo(Node.GetPrevSibling, naAddChild);
    // If the node is expanded, its children will moved out of the node, otherwise remain them.
    // 如果节点是展开状态，则移出其子节点，否则保留其子节点
    if Node.Expanded then begin
      ChildNode := Node.GetLastChild;
      while ChildNode <> nil do begin
        ChildNode.MoveTo(Node, naInsertBehind);
        ChildNode := Node.GetLastChild;
      end;
    end;

    Node.TreeView.EndUpdate;
    DBChanged := True;
  end;
end;

{ ============================================================ }
{ Drag And Drop Node 拖拽节点                                  }
{ ============================================================ }

procedure TformMain.treeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // ssCtrl means start drag using Move mode, ssAlt means start drag using Copy mode
  // ssCtrl 表示以移动模式开始拖动，ssAlt 表示以复制模式开始拖动
  if ((ssCtrl in Shift) or (ssAlt in Shift)) and (Button = mbLeft) and
    (htOnItem in (Sender as TTreeView).GetHitTestInfoAt(X, Y)) then
  begin
    (Sender as TTreeView).BeginDrag(False);
    FDragMode := dmMove;
    // Only trevTree is allowed to copy node | 只有 trevTree 可以复制节点
    if (ssAlt in Shift) and (Sender = trevTree) then
      FDragMode := dmCopy
  end;
end;

procedure TformMain.treeDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = trevTree) or (Source = trevRecy);
end;

procedure TformMain.treeDragDrop(Sender, Source: TObject; X, Y: Integer);
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

function TformMain.CopyNode(ToTree: TTreeView; Node, ToNode: TTreeNode;
  Mode: TNodeAttachMode): TTreeNode;
var
  ID, ToID, NewID: Integer;
begin
  Result := nil;
  if Node = nil then Exit;
  ID := GetNodeID(Node);

  if ToNode <> nil then
    ToID := GetNodeID(ToNode)
  else if ToTree = trevTree then
    ToID := RootID
  else
    ToID := RecyclerID;

  // Copy the node in database | 在数据库中复制节点
  NewID := FTreeDB.CopyNode(ID, ToID, Mode);

  // Copy the node in TreeView | 在 TreeView 中复制节点
  Result := LoadNode(ToTree, NewID, ToNode, Mode);

  DBChanged := True;
end;

