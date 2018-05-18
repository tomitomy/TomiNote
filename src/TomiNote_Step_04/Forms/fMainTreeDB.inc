
{ ============================================================ }
{ Control State 控件状态                                       }
{ ============================================================ }

procedure TformMain.actnRenameUpdate(Sender: TObject);
var
  AEnabled: Boolean;
begin
  AEnabled := FActiveNode <> nil;
  actnRename               .Enabled := AEnabled;
  actnDeleteNode           .Enabled := AEnabled;

  AEnabled := FDBActive;
  actnAddToFront           .Enabled := AEnabled;
  actnAddToBehind          .Enabled := AEnabled;
  actnAddToChildFirst      .Enabled := AEnabled;
  actnAddToChildLast       .Enabled := AEnabled;
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

  // Enter active state | 进入活动状态
  FDBName := FileName;
  DBActive := True;
  DBChanged := False;
  trevTree.Enabled := True;

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
  trevTree.Enabled := False;

  UnloadTree(trevTree);

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
  for ID in FTreeDB.GetChildren(RootID) do begin
    NewNode := Tree.Items.AddChild(nil, FTreeDB.GetName(ID));
    SetNodeID(NewNode, ID);

    LoadSubNodes(Tree, NewNode);
  end;
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

