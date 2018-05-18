
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

