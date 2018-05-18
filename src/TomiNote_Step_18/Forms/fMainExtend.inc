
{ ============================================================ }
{ Rename Node 重命名节点                                       }
{ ============================================================ }

// TreeView's Naming Box cannot change the background color, so use TEdit
// instead of the Nename Box to implement the rename function
// TreeView 的重命名框无法改变背景色，所以用 TEdit 代替重命名框实现重命名功能
procedure TformMain.treeEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
var
  ARect: TRect;
begin
  AllowEdit := False;
  editRename.Parent := Node.TreeView;
  ARect := Node.DisplayRect(True);
  ARect.Right := editRename.Parent.ClientRect.Right;
  editRename.BoundsRect := ARect;

  editRename.Text := Node.Text;
  editRename.Show;
  editRename.SetFocus;
  editRename.SelectAll;

  FEditHistory.Reset;
  FEditHistory.InEdit := True;
end;

procedure TformMain.editRenameKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    SubmitRename(Sender)
  else if Key = VK_ESCAPE then
    CancelRename;
end;

procedure TformMain.SubmitRename(Sender: TObject);
begin
  if not editRename.Visible then Exit;

  if FActiveNode.Text <> editRename.Text then begin
    FTreeDB.SetName(GetNodeID(FActiveNode), editRename.Text);
    FActiveNode.Text := editRename.Text;
    DBChanged := True;
  end;
  FEditHistory.InEdit := False;
  editRename.Hide;
  FActiveNode.TreeView.SetFocus;
end;

procedure TformMain.CancelRename;
begin
  if not editRename.Visible then Exit;
  editRename.Hide;
  FEditHistory.Reset;
  if FActiveNode <> nil then
    FActiveNode.TreeView.SetFocus
  else
    memoNote.SetFocus;
end;

procedure TformMain.pmiRenameClick(Sender: TObject);
begin
  case (Sender as TMenuItem).Name of
    'pmiCutRn'       : editRename.CutToClipboard;
    'pmiCopyRn'      : editRename.CopyToClipboard;
    'pmiPasteRn'     : FEditHistory.PasteText;
    'pmiSelectAllRn' : editRename.SelectAll;
    'pmiDeleteRn'    : FEditHistory.DeleteText;
    'pmiUndoRn'      : FEditHistory.Undo;
    'pmiRedoRn'      : FEditHistory.Redo;
  end;
end;

procedure TformMain.treeResize(Sender: TObject);
begin
  CancelRename;
end;

{ ============================================================ }
{ Recent Files 最近打开的文件                                   }
{ ============================================================ }

// RecentFiles[i]="Rank=FileName"
procedure TformMain.AddRecentFile(FileName: string);
var
  i, Index, Rank, CurRank: Integer;
  NewMenuItem: TMenuItem;
const
  RecentFilesLimit = 10;
begin
  // Lookup file index | 查找文件索引
  Index := -1;
  for i := 0 to Config.RecentFiles.Count - 1 do
    if Config.RecentFiles.ValueFromIndex[i] = FileName then begin
      Index := i;
      break;
    end;

  // Move the file's rank to first if exists | 如果文件在列表中，则将其移到最前
  if Index >= 0 then begin
    // Get file's rank to use later, then move it to first | 获取文件排名备用，并将其移到最前
    Rank := StrToIntDef(Config.RecentFiles.Names[Index], RecentFilesLimit);
    Config.RecentFiles[Index] := '1=' + Config.RecentFiles.ValueFromIndex[i];
    // Move files less than this rank backward | 将小于该排名的文件后移
    for i := 0 to Config.RecentFiles.Count - 1 do begin
      if i = Index then continue;
      CurRank := StrToIntDef(Config.RecentFiles.Names[i], RecentFilesLimit);
      if CurRank < Rank then
        Config.RecentFiles[i] := IntToStr(CurRank + 1) + '=' + Config.RecentFiles.ValueFromIndex[i];
    end;
    Exit;
  end;

  // If the file is not in the list, move all files backward
  // 如果文件不在列表中，则将所有文件后移
  for i := Config.RecentFiles.Count - 1 downto 0 do begin
    Rank := StrToIntDef(Config.RecentFiles.Names[i], RecentFilesLimit);
    if Rank >= RecentFilesLimit then begin
      // Delete files that will be out of range | 删除即将超出范围的文件项
      mmiRecentFiles.Delete(i);
      Config.RecentFiles.Delete(i);
    end
    else
      // Move other files backward | 将其它文件后移
      Config.RecentFiles[i] := IntToStr(Rank + 1) + '=' + Config.RecentFiles.ValueFromIndex[i];
  end;

  // Add new menu item | 添加新的菜单项
  NewMenuItem := TMenuItem.Create(Self);
  NewMenuItem.Caption := FileName;
  NewMenuItem.OnClick := @OpenRecentFile;
  mmiRecentFiles.Insert(0, NewMenuItem);
  Config.RecentFiles.Insert(0, '1='+FileName);
  mmiClearRecentFiles.Enabled := Config.RecentFiles.Count > 0;
end;

procedure TformMain.DelRecentFile(FileName: string);
var
  i: integer;
  Rank, CurRank: integer;
const
  RecentFilesLimit = 10;
begin
  // Lookup file index | 查找文件索引
  for i := 0 to Config.RecentFiles.Count - 1 do
    if Config.RecentFiles.ValueFromIndex[i] = FileName then
      break;

  // Exit if the file is not in the list | 文件不在列表中则退出
  if i >= Config.RecentFiles.Count then Exit;

  // Get rank to use later, then delete it | 获取文件排名备用，然后删除文件项
  Rank := StrToIntDef(Config.RecentFiles.Names[i], RecentFilesLimit);
  mmiRecentFiles.Delete(i);
  Config.RecentFiles.Delete(i);

  // Move items larger than this rank forward | 将大于此排名的项目前移
  for i := 0 to Config.RecentFiles.Count - 1 do begin
    CurRank := StrToIntDef(Config.RecentFiles.Names[i], RecentFilesLimit);
    if CurRank > Rank then
      Config.RecentFiles[i] := IntToStr(CurRank - 1) + '=' + Config.RecentFiles.ValueFromIndex[i];
  end;
  mmiClearRecentFiles.Enabled := Config.RecentFiles.Count > 0;
end;

procedure TformMain.LoadRecentFiles;
var
  i: Integer;
  NewMenuItem: TMenuItem;
begin
  for i := Config.RecentFiles.Count - 1 downto 0 do begin
    NewMenuItem := TMenuItem.Create(self);
    NewMenuItem.Caption := Config.RecentFiles.ValueFromIndex[i];
    NewMenuItem.OnClick := @OpenRecentFile;
    mmiRecentFiles.Insert(0, NewMenuItem);
  end;
  mmiClearRecentFiles.Enabled := Config.RecentFiles.Count > 0;
end;

procedure TformMain.OpenRecentFile(Sender: TObject);
var
  FileName: string;
  AbsolutePath: string;
begin
  if Sender = nil then Exit;
  FileName := (Sender as TMenuItem).Caption;

  if IsKeyDown(VK_SHIFT) then begin
    DelRecentFile(FileName);
    Exit;
  end;

  AbsolutePath := SetDirSeparators(FileName);

  if not FileExists(AbsolutePath) then
    AbsolutePath := ConcatPaths([AppDir, AbsolutePath]);

  if not FileExists(AbsolutePath) then begin
    if Application.MessageBox(PChar(Res_RecentFileNotExists), PChar(AppTitle),
      MB_YESNO+MB_ICONQUESTION) = ID_YES then
      DelRecentFile(FileName);
    Exit;
  end;

  actnCloseExecute(Sender);
  if FDBActive then Exit;

  if not OpenDB(AbsolutePath) then Exit;

  AddRecentFile(FileName); // Raise the rank | 提高排名
  memoNote.SetFocus; // Deactivate TreeView | 取消激活 TreeView
end;

procedure TformMain.mmiClearRecentFilesClick(Sender: TObject);
begin
  while mmiRecentFiles.Count > 2 do begin
    mmiRecentFiles.Delete(0);
  end;
  Config.RecentFiles.Clear;
  mmiClearRecentFiles.Enabled := Config.RecentFiles.Count > 0;
end;

{ ============================================================ }
{ Database File Mutex 数据库文件互斥                           }
{ ============================================================ }

// Create a lock file | 创建一个锁文件
function TformMain.LockDB(DBFileName: string): Boolean;
var
  LockFile: string;
  FileHandle: THandle;
begin
  Result := False;
  LockFile := ChangeFileExt(DBFileName, LockFileExt);
  if FileExists(LockFile) then Exit;
  try
    FileHandle := FileCreate(LockFile);
    FileClose(FileHandle);
    Result := FileHandle <> THandle(-1);
  except
  end;
end;

// Delete the lock file | 删除一个锁文件
function TformMain.UnLockDB(DBFileName: string): Boolean;
var
  LockFile: string;
begin
  LockFile := ChangeFileExt(DBFileName, LockFileExt);
  if FileExists(LockFile) then
    DeleteFile(LockFile);
  Result := not FileExists(LockFile);
end;

// Detect the lock file | 检测锁文件
function TformMain.DBLocked(DBFileName: string): Boolean;
begin
  Result := FileExists(ChangeFileExt(DBFileName, LockFileExt)) and
  (Application.MessageBox(PChar(Res_DBIsLocked), PChar(AppTitle), MB_YESNO) <> ID_YES);
end;

{ ============================================================ }
{ Auto Save 自动保存                                           }
{ ============================================================ }

procedure TformMain.timrBackupTimer(Sender: TObject);
begin
  // Auto Save | 自动保存
  if Config.AutoSaveInterval > 0 then begin
    Dec(FAutoSaveRemaining, 1);

    if FAutoSaveRemaining <= 0 then begin
      FAutoSaveRemaining := Config.AutoSaveInterval;
      SaveDB;
    end;
  end;

  // Auto Backup | 自动备份
  if (Config.AutoBackupInterval > 0) and (Config.AutoBackupCount > 0) and FChangedAfterBackup then begin
    Dec(FAutoBackupRemaining, 1);

    if FAutoBackupRemaining <= 0 then begin
      if BackupDB then begin
        FAutoBackupRemaining := Config.AutoBackupInterval;
        FChangedAfterBackup := False;
      end else
        Application.MessageBox(PChar(Res_BackupDBFail), PChar(AppTitle), MB_OK + MB_ICONERROR);
    end;
  end;
  statMain.Panels[1].Text := Format(Res_AutoSaveInfo, [FAutoSaveRemaining, FAutoBackupRemaining]);
end;

{ ============================================================ }
{ Auto Backup 自动备份                                         }
{ ============================================================ }

function TformMain.BackupDB: Boolean;
var
  FilePath: string;
begin
  FilePath := ExtractFilePath(FDBName);
  Result := FTreeDB.BackupDB(ConcatPaths([FilePath, 'backup']), Config.AutoBackupCount);
end;

{ ============================================================ }
{ Utils 工具                                                   }
{ ============================================================ }

procedure TformMain.actnUtilsExecute(Sender: TObject);
begin
  if formUtils = nil then
    Application.CreateForm(TformUtils, formUtils);
  formUtils.Show;
end;

procedure TformMain.CompareEvent(Sender: TObject; Node1, Node2: TTreeNode; var Compare: Integer);
begin
  if (Node1.Parent <> FSortParent) or (Node2.Parent <> FSortParent) then
    Compare := 0  // Ignore non-sibling nodes | 忽略非兄弟节点
  else if FSortDesc then
    Compare := Node2.Text.CompareTo(Node1.Text)
  else
    Compare := Node1.Text.CompareTo(Node2.Text);
end;

procedure TformMain.SortNode(InSibling, Descending: boolean);
var
  i, ChildID, ParentID: Integer;
  Tree: TTreeView;
  ChildNode: TTreeNode;
  Children: TBoundArray;
begin
  if FActiveNode = nil then Exit;

  Tree := FActiveNode.TreeView as TTreeView;

  if InSibling then
    FSortParent := FActiveNode.Parent
  else
    FSortParent := FActiveNode;

  FSortDesc := Descending;

  // Sort in TreeView | 在 TreeView 中排序
  Tree.OnCompare := @CompareEvent;
  Tree.SortType := stText;  // Start sort | 开始排序
  Tree.SortType := stNone;  // Restore for next sort | 为下次排序复位
  Tree.OnCompare := nil;

  if FSortParent <> nil then
    ParentID := GetNodeID(FSortParent)
  else if FActiveNode.TreeView = trevTree then
    ParentID := RootID
  else
    ParentID := RecyclerID;

  // Write the result into database | 将结果写入数据库
  Children := FTreeDB.GetChildren(ParentID);
  if FSortParent <> nil then
    ChildNode := FSortParent.GetFirstChild
  else
    ChildNode := Tree.Items[0];

  i := 0;
  while ChildNode <> nil do begin
    ChildID := GetNodeID(ChildNode);
    if Children[i] <> ChildID then begin
      DBChanged := True;
      Children[i] := ChildID;
    end;
    Inc(i);
    ChildNode := ChildNode.GetNextSibling;
  end;

  FTreeDB.SetChildren(ParentID, Children);

  // Show the result | 显示结果
  if not InSibling then FActiveNode.Expand(False);
end;

procedure TformMain.SplitNote(Splitter, Title: string;
  IncludeSplitter: boolean; PreNumLen: integer);
var
  ID, NewNoteStart, NewNoteLen, Count: Integer;
  SubExpr: TRegExpr;
  Note, NewNote, NewTitle, PreNum: string;

  procedure SplitOneNote;
  begin
    // Get the child note | 获取子笔记
    NewNote := Copy(Note, NewNoteStart, NewNoteLen);
    NewNote := TrimBlank(NewNote);
    if NewNote = '' then Exit;
    // Get the child name | 获取子名称
    if (Title <> '') and SubExpr.Exec(NewNote) then
      NewTitle := SubExpr.Match[0]
    else
      NewTitle := Res_UnnamedNode;
    // Add prefix number | 添加数字前缀
    if PreNumLen > 0 then begin
      Inc(Count);
      PreNum := IntToStr(Count);
      while Length(PreNum) < PreNumLen do PreNum := '0' + PreNum;
      NewTitle := PreNum + NewTitle;
    end;
    // Write to database | 写入数据库
    FTreeDB.AddNode(NewTitle, NewNote, ID, naAddChild);
    DBChanged := True;
  end;

var
  Expr: TRegExpr;
  Found: Boolean;
  PrevMatchPos: SizeInt;
begin
  if (FActiveNode = nil) or (Splitter = '') then Exit;

  ID := GetNodeID(FActiveNode);
  Note := FTreeDB.GetNote(ID);

  Expr := TRegExpr.Create(Splitter);
  SubExpr := TRegExpr.Create(Title);

  Count := 0;
  NewNoteStart := 1;
  PrevMatchPos := 1;

  // Find the separator | 查找分隔符
  Found := Expr.Exec(Note);
  while Found do begin
    // Get the length of the child note | 获取子笔记的长度
    NewNoteLen := UTF8PosToBytePos(PChar(Note) + NewNoteStart - 1,
      Length(Note) - NewNoteStart + 1, Expr.MatchPos[0] - PrevMatchPos + 1) - 1;

    // Create child node | 创建子节点
    SplitOneNote;

    // Calculate the position of the next child note | 计算下一个子笔记的位置
    if IncludeSplitter then begin
      NewNoteStart := NewNoteStart + NewNoteLen;
      PrevMatchPos := Expr.MatchPos[0];
    end else begin
      NewNoteStart := NewNoteStart + Length(Expr.Match[0]) + NewNoteLen;
      PrevMatchPos := Expr.MatchPos[0] + Expr.MatchLen[0];
    end;

    // Find the next separator | 查找下一个分隔符
    Found := Expr.ExecNext;
  end;
  // Handle the content behind the last separator
  // 处理最后一个分隔符之后的内容
  NewNoteLen := Length(Note) - NewNoteStart + 1;
  SplitOneNote;

  SubExpr.Free;
  Expr.Free;

  // Load the new nodes | 载入新结点
  ReLoadSubNodes(trevTree, trevTree.Selected);
  // Show the new nodes | 显示新节点
  FActiveNode.Expand(False);
end;

procedure TformMain.RenameNodes(ASearchText, ANameText: string;
  SearchInNote: boolean; PreNumLen, Depth: Integer);
var
  Expr: TRegExpr;
  Count: Integer;
  AText, PreNum: string;
  ANode: TTreeNode;

  procedure DoRename(ID: Integer);
  var
    Child: integer;
  begin
    Dec(Depth);

    // If Depth > 0, only rename at the special depth.
    // If Depth < 0, rename at every depth.
    // 如果 Depth > 0，则只在指定深度进行重命名
    // 如果 Depth < 0，则在每个深度都进行重命名
    if (Depth <= 0) then begin
      // Get content to search from | 获取搜索对象
      if SearchInNote then
        AText := FTreeDB.GetNote(ID)
      else
        AText := FTreeDB.GetName(ID);

      // No replacement if no match | 如果无匹配项则不执行替换操作
      if Expr.Exec(AText) then
      begin
        AText := Expr.Replace(Expr.Match[0], ANameText, True);
        // Insert serial numbers as needed | 根据需要插入序号
        if PreNumLen > 0 then begin
          Inc(Count);
          PreNum := IntToStr(Count);
          while Length(PreNum) < PreNumLen do PreNum := '0' + PreNum;
          AText := PreNum + AText;
        end;
        // Perform a Replace operation and write the replace result into the node name
        // 执行替换操作，并将替换结果写入节点名称中
        FTreeDB.SetName(ID, AText);
        FSearchResult.Add(ID, 0, 0);
        // Update node name in TreeView | 在 TreeView 中更新节点名称
        ANode := IDToNode(ID, False);
        if ANode <> nil then
          ANode.Text := AText;
      end;
    end;
    // When reaches specified depth, no longer in-depth | 达到指定深度则不再深入
    if Depth <> 0 then
      for Child in FTreeDB.GetChildren(ID) do
        DoRename(Child);

    Inc(Depth);
  end;

var
  ID, Child: integer;
begin
  // Empty SearchText causes Regular Expression exceptions
  // 空搜索内容会导致正则表达式异常
  if (FActiveNode = nil) or (ASearchText = '') or (Depth = 0) then Exit;

  // Get entry | 获取搜索入口
  if Depth <> WholeTree then
    ID := GetNodeID(FActiveNode)
  else if FActiveNode.TreeView = trevTree then
    ID := RootID
  else
    ID := RecyclerID;

  Expr := TRegExpr.Create(ASearchText);

  SubmitNote;
  FSearchResult.Clear;

  Count := 0;
  PreNum := '';
  if ID < FirstID then
    for Child in FTreeDB.GetChildren(ID) do
      DoRename(Child)
  else
    DoRename(ID);

  Expr.Free;

  LoadSearchResult;

  if FSearchResult.Count > 0 then DBChanged := True;
end;

{ ScriptThread }

constructor TScriptThread.Create;
begin
  // Suspended after create | 创建后暂停
  inherited Create(True);
end;

procedure TScriptThread.Execute;
begin
  // Auto destory after execute | 执行完毕后自动销毁
  FreeOnTerminate := True;
  // Enter the search process | 进入搜索过程
  formMain.FSearchResult.SearchFinished := False;
  ExecuteScript;
  // Complete the search process | 完成搜索过程
  formMain.FSearchResult.SearchFinished := True;
end;

// If InSelection=True, the Depth and Filter will be ignore
// 如果 InSelection=True, 则 Depth 和 Filter 将被忽略
procedure TformMain.ExecuteScript(ScriptID: Integer; Depth: Integer;
  Filter: string; InSelection: Boolean);

  procedure ExecuteInSelection;
  var
    OldSelText: string;
    NewSelText: string;
  begin
    // Get replaced result | 获取替换结果
    {$ifdef MSWINDOWS}
      OldSelText := ToLF(memoNote.SelText);
    {$else}
      OldSelText := memoNote.SelText;
    {$endif}
    NewSelText := FScript.ExecScript(OldSelText);
    // Show result in memoNote | 在 memNote 中显示结果
    if FScript.TextChanged then begin
      FHistory.SelChange(OldSelText, NewSelText, True);
      DBChanged := True;
    end;
  end;

var
  ScriptText: string;

begin
  if FActiveNode = nil then Exit;
  if InSelection and (memoNote.SelLength = 0) then Exit;

  ScriptText := FScriptDB.GetNote(ScriptID);
  if ScriptText = '' then Exit;

  FScript.ParseScript(ScriptText);

  if InSelection then begin
    ExecuteInSelection;
    Exit;
  end;

  SubmitNote;
  // Pre-load the nodes, do not load node in non-main thread (IDToNode will load note)
  // 预加载节点，不要在非主线程中加载节点（IDToNode会加载节点）
  LoadBranch(FActiveNode.TreeView as TTreeView, FActiveNode, Depth);

  // Disable main form controls and clear search results
  // 禁用主窗口控件并清空搜索结果
  BeginSearch;
  FSearchResult.FixLineEnding := False;
  FSearchResult.ShowResultList := (Depth = WholeTree) or
    (Depth <> 1) and (FActiveNode <> nil) and (FActiveNode.Count > 0);

  // Create search thread | 创建搜索线程
  with TScriptThread.Create do begin
    // Provide search parameters | 提供搜索参数
    FDepth        := Depth;
    FFilter       := Filter;
    // Start thread | 启动线程
    Suspended     := False;
  end;
end;

// Do not change the data of the formMain control in the thread process
// 不要在线程中更改 formMain 控件的数据
procedure TScriptThread.ExecuteScript; inline;
begin
  formMain.ExecuteScriptProcess(FDepth, FFilter);
end;

procedure TformMain.ExecuteScriptProcess(Depth: Integer; Filter: string);
var
  AName, ANote, NewNote    : string;
  // Regular expression object | 正则表达式对象
  FilterRegExpr            : TRegExpr;
  HistoryData              : TSteps;
  CountLimit               : SizeInt;

  procedure ExecuteInTree(AID: Integer);
  begin
    // Capture "User Abort" signal | 捕获“用户中止”信号
    if FSearchResult.SearchAbort then
      Exit;

    // Count limit | 数量限制
    if FSearchResult.Count >= CountLimit then begin
      FSearchResult.SearchAbort := True;
      Exit;
    end;

    AName := FTreeDB.GetName(AID);
    ANote := FTreeDB.GetNote(AID);

    // Name filter | 名称过滤
    if (FilterRegExpr = nil) or FilterRegExpr.Exec(AName) then begin

      Inc(FSearchResult.SearchSize, Length(ANote));

      // Start replace | 开始替换
      NewNote := FScript.ExecScript(ANote);

      // Process history and write changes to the database
      // 处理历史记录并将更改写入数据库
      if FScript.TextChanged then begin
        HistoryData := ProcessHistoryData(AID, ANote, NewNote);

        // The LineEnding has been fixed in HistoryData,
        // so the FSearchResult does not need to fix it again.
        // 换行符在 HistoryData 中已经修复过了，不需要在 FSearchResult 中再次修复。
        if HistoryData <> nil then begin
          with HistoryData.CurStep^ do begin
            if SelStart < 0 then
              FSearchResult.Add(AID, -SelStart, 0)
            else
              FSearchResult.Add(AID, SelStart, UTF8LengthFast(SelText));
          end;
          DBChanged := True;
        end;
      end;

    end;

    Dec(Depth);

    if (Depth <> 0) and (not FSearchResult.SearchAbort) then
      for AID in FTreeDB.GetChildren(AID) do
        ExecuteInTree(AID);
    Inc(Depth);
  end;

var
  ID: Integer;
begin
  // Check parameter | 参数检查
  if (FActiveNode = nil) or (Depth = 0) then Exit;

  // Initialize search limit | 初始化搜索限制
  if Config.SearchCountLimit <= 0 then
    CountLimit := MaxInt
  else
    CountLimit := Config.SearchCountLimit;

  FilterRegExpr := nil;
  if Filter <> '' then FilterRegExpr := TregExpr.Create(Filter);

  // Get the entry node | 获取入口节点
  if Depth <> WholeTree then
    ID := GetNodeID(FActiveNode)
  else if FActiveNode.TreeView = trevTree then
    ID := RootID
  else
    ID := RecyclerID;

  // Enter recursive search | 进入递归搜索
  if ID < FirstID then
    for ID in FTreeDB.GetChildren(ID) do
      ExecuteInTree(ID)
  else
    ExecuteInTree(ID);
end;

{ Script }

procedure TformMain.LoadScriptMenu;

  procedure AddMenuItem(ID: Integer; AParent: TMenuItem);
  var
    MenuItem: TMenuItem;
  begin
    MenuItem := TMenuItem.Create(Self);
    MenuItem.Caption := FScriptDB.GetName(ID);
    MenuItem.Tag := ID;
    AParent.Add(MenuItem);
    for ID in FScriptDB.GetChildren(ID) do
      AddMenuItem(ID, MenuItem);
    if MenuItem.Count = 0 then
      MenuItem.OnClick := @pmiScriptClick;
  end;

var
  Child: Integer;
begin
  for Child in FScriptDB.GetChildren(RootID) do
    AddMenuItem(Child, pmiScript);
end;

procedure TformMain.UnLoadScriptMenu;
var
  MenuItem: TMenuItem;

  procedure UnLoadMenu(AParent: TMenuItem);
  var
    i: Integer;
  begin
    for i := AParent.Count - 1 downto 0 do begin
      UnLoadMenu(AParent.Items[i]);
      MenuItem := AParent.Items[i];
      AParent.Delete(i);
      MenuItem.Free;
    end;
  end;

begin
  UnLoadMenu(pmiScript);
end;

procedure TformMain.pmiScriptClick(Sender: TObject);
var
  MenuItem: TMenuItem;
begin
  MenuItem := (Sender as TMenuItem);
  if MenuItem.Count > 0 then Exit;

  ExecuteScript(MenuItem.Tag, 1, '', memoNote.SelLength > 0);
end;

procedure TformMain.LocalizationSpecialControl;
begin
  openDlg1       .Title   := Res_TitleOpenDlg;
  saveDlg1       .Title   := Res_TitleSaveDlg;

  pmiCutRn       .Caption := actnCut.Caption;
  pmiCopyRn      .Caption := actnCopy.Caption;
  pmiPasteRn     .Caption := actnPaste.Caption;
  pmiSelectAllRn .Caption := actnSelectAll.Caption;
  pmiDeleteRn    .Caption := actnDeleteText.Caption;
  pmiUndoRn      .Caption := actnUndo.Caption;
  pmiRedoRn      .Caption := actnRedo.Caption;
end;

