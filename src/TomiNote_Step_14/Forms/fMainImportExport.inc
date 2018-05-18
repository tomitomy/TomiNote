
{ ============================================================ }
{ Import And Export 导入和导出                                 }
{ ============================================================ }

procedure TformMain.actnImportExecute(Sender: TObject);
begin
  if formImport = nil then
    Application.CreateForm(TformImport, formImport);
  formImport.Show;
end;

procedure TformMain.actnExportExecute(Sender: TObject);
begin
  if formExport = nil then
    Application.CreateForm(TformExport, formExport);
  formExport.Show;
end;

procedure TformMain.actnSaveAsExecute(Sender: TObject);
var
  FileName: string;
  TempFileName: string;
begin
  if FDBName <> '' then
    saveDlg1.InitialDir := ExtractFileDir(FDBName);
  saveDlg1.FileName := '';

  if not saveDlg1.Execute then Exit;

  FileName := saveDlg1.FileName;

  if ExtractFileExt(FileName) = '' then
    FileName := ChangeFileExt(FileName, DBFileExt);

  if FileExists(FileName) and
    (Application.MessageBox(PChar(Res_OverwriteFileTip), PChar(AppTitle),
    MB_YESNO + MB_ICONQUESTION) <> IDYES) then
    Exit;

  TempFileName := FileName + '.tominote_temp';
  while FileExists(TempFileName) do
    TempFileName := TempFileName + '+';

  if not ExportDB(TempFileName) then begin
    Application.MessageBox(PChar(Res_SaveDBFail), PChar(AppTitle), MB_OK);
    Exit;
  end;

  if not CloseDB(False) then begin
    if FileName = FDBName then begin
      DeleteFile(TempFileName);
      Application.MessageBox(PChar(Res_OverwriteFileFail), PChar(AppTitle), MB_OK);
    end else
      RenameFile(TempFileName, FileName);
    Exit;
  end;

  if FileExists(FileName) then DeleteFile(FileName);
  if FileExists(FileName) then begin
    Application.MessageBox(PChar(Res_OverwriteFileFail), PChar(AppTitle), MB_OK);
    Exit;
  end;

  RenameFile(TempFileName, FileName);
  if not OpenDB(FileName) then Exit;
end;

function TformMain.ExportDB(FileName: string): Boolean;
begin
  SubmitNote;

  Result := FTreeDB.ExportToDB(RootID, RootID, FileName, AllDepth) and
    FTreeDB.ExportToDB(RecyclerID, RecyclerID, FileName, AllDepth);
end;

{ Import 导入 }

// Import single file | 导入单个文件
function TformMain.ImportFile(FileName: string; IncludeExt: boolean;
  Mode: TNodeAttachMode): TTreeNode;
var
  NewID, ToID: integer;
begin
  Result := trevTree.Selected;
  if not FileExists(FileName) then Exit;

  if trevTree.Selected = nil then
    ToID := RootID
  else
    ToID := GetNodeID(trevTree.Selected);

  NewID := FTreeDB.ImportFile(FileName, IncludeExt, ToID, Mode);

  // If trevTree is not empty, there must be a node selected
  // 如果 trevTree 不为空，则必然有节点被选中
  if trevTree.Selected = nil then begin
    LoadTree(trevTree, RootID);
    Result := trevTree.Items[0];
  end else
    Result := LoadNode(trevTree, NewID, trevTree.Selected, Mode);

  Result.Selected := True;
end;

// Import a directory | 导入目录
function TformMain.ImportDir(DirName: string; IncludeEntry: boolean;
  IncludeExt: boolean; Mode: TNodeAttachMode): TTreeNode;
var
  NewID, ToID: integer;
begin
  Result := trevTree.Selected;
  if not DirectoryExists(DirName) then Exit;

  if trevTree.Selected = nil then
    ToID := RootID
  else
    ToID := GetNodeID(trevTree.Selected);

  NewID := FTreeDB.ImportDir(DirName, IncludeEntry, IncludeExt, ToID, Mode);

  // If trevTree is not empty, there must be a node selected
  // 如果 trevTree 不为空，则必然有节点被选中
  if trevTree.Selected = nil then begin
    LoadTree(trevTree, RootID);
    Result := trevTree.Items[0];
  end else if NewID <> ToID then
    // Include the entry directory | 包含入口目录
    Result := LoadNode(trevTree, NewID, trevTree.Selected, Mode)
  else begin
    // Does not include the entry directory | 不包含入口目录
    Result := trevTree.Selected;
    case Mode of
      naAddChild, naAddChildFirst:
        Result := ReLoadSubNodes(trevTree, Result);
      else
        if Result.Level = 0 then
          Result := ReLoadSubNodes(trevTree, nil)
        else
          Result := ReLoadSubNodes(trevTree, Result.Parent)
    end;
  end;

  Result.Selected := True;
end;

// Import a database | 导入数据库
function TformMain.ImportDB(FromPath: string; Mode: TNodeAttachMode): TTreeNode;
var
  NewID, ToID: integer;
begin
  Result := trevTree.Selected;
  if not FileExists(FromPath) then Exit;

  if trevTree.Selected = nil then
    ToID := RootID
  else
    ToID := GetNodeID(trevTree.Selected);

  // FromID is fixed to RootID | FromID 固定设置为 RootID
  NewID := FTreeDB.ImportDB(FromPath, RootID, ToID, Mode);

  // If trevTree is not empty, there must be a node selected
  // 如果 trevTree 不为空，则必然有节点被选中
  if trevTree.Selected = nil then begin
    LoadTree(trevTree, RootID);
    Result := trevTree.Items[0];
  end else if NewID <> ToID then
    // FromID <> RootID, it can't be happened here
    // FromID <> RootID，这种情况不会发生
    Result := LoadNode(trevTree, NewID, trevTree.Selected, Mode)
  else begin
    // FromID == RootID | FromID 就是 RootID
    Result := trevTree.Selected;
    case Mode of
      naAddChild, naAddChildFirst:
        Result := ReLoadSubNodes(trevTree, Result);
      else
        if Result.Level = 0 then
          Result := ReLoadSubNodes(trevTree, nil)
        else
          Result := ReLoadSubNodes(trevTree, Result.Parent)
    end;
  end;

  Result.Selected := True;
end;

{ Export 导出 }

// Export a node and its child nodes into a single text file
// 将节点及其子节点导出到单个文本文件中
procedure TformMain.ExportToFile(ToPath: string; Splitter: string; Depth: integer);
begin
  if FActiveNode = nil then Exit;
  FTreeDB.ExportToFile(GetNodeID(FActiveNode), ToPath, Splitter, Depth);
end;

// Export a node and its child nodes into a directory
// 将节点及其子节点导出到目录中
procedure TformMain.ExportToDir(ToPath: string; Ext: string; Depth: integer);
begin
  if FActiveNode = nil then Exit;
  FTreeDB.ExportToDir(GetNodeID(FActiveNode), ToPath, Ext, Depth);
end;

// Export the node and its child nodes to a database file, the original content in the database
// will not be overwritten, the new content will be append to the target database
// 将节点及其子节点导出到数据库文件中，数据库中原有的内容不会被覆盖，新内容将添加到目标数据库的尾部
function TformMain.ExportToDB(ToPath: string; Depth: integer): boolean;
var
  ToID: Integer;
begin
  if FActiveTree = trevTree then ToID := RootID else ToID := RecyclerID;
  Result := FTreeDB.ExportToDB(GetNodeID(FActiveNode), ToID, ToPath, Depth);
end;

