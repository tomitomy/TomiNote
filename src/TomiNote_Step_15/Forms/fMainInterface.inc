
{ ============================================================ }
{ Form 窗体                                                    }
{ ============================================================ }

procedure TformMain.actnExitExecute(Sender: TObject);
begin
  Close;
end;

{ ============================================================ }
{ Layout 布局                                                  }
{ ============================================================ }

procedure TformMain.actnToggleMenuBarExecute(Sender: TObject);
begin
  ToggleMenuBar(not actnToggleMenuBar.Checked);
end;

procedure TformMain.actnToggleToolBarExecute(Sender: TObject);
begin
  ToggleToolBar(not actnToggleToolBar.Checked);
end;

procedure TformMain.actnToggleStatBarExecute(Sender: TObject);
begin
  ToggleStatBar(not actnToggleStatBar.Checked);
end;

procedure TformMain.actnToggleTreeBarExecute(Sender: TObject);
begin
  ToggleTreeBar(not actnToggleTreeBar.Checked);
end;

procedure TformMain.actnToggleInfoBarExecute(Sender: TObject);
begin
  ToggleInfoBar(not actnToggleInfoBar.Checked);
end;

procedure TformMain.actnToggleRecyBarExecute(Sender: TObject);
begin
  ToggleRecyBar(not actnToggleRecyBar.Checked);
end;

procedure TformMain.actnFullScreenExecute(Sender: TObject);
begin
  FullScreen(FormMain.BorderStyle <> bsNone);
end;

procedure TformMain.actnFullWindowExecute(Sender: TObject);
begin
  FullWindow(
    (Config.FullWindowHideMenuBar and actnToggleMenuBar.Checked) or
    (Config.FullWindowHideToolBar and actnToggleToolBar.Checked) or
    (Config.FullWindowHideStatBar and actnToggleStatBar.Checked) or
    (Config.FullWindowHideTreeBar and actnToggleTreeBar.Checked) or
    (Config.FullWindowHideInfoBar and actnToggleInfoBar.Checked));
end;

{ Visible 可见 }

procedure TformMain.ToggleMenuBar(AVisible: Boolean);
begin
  actnToggleMenuBar.Checked := AVisible;
  if AVisible then Menu := menuMain else Menu := nil;
end;

procedure TformMain.ToggleToolBar(AVisible: Boolean);
begin
  actnToggleToolBar.Checked := AVisible;
  tbarMain.Visible := AVisible;
end;

procedure TformMain.ToggleStatBar(AVisible: Boolean);
begin
  actnToggleStatBar.Checked := AVisible;
  statMain.Visible := AVisible;
end;

procedure TformMain.ToggleTreeBar(AVisible: Boolean);
begin
  if FInEdit then CancelRename;
  actnToggleTreeBar.Checked := AVisible;
  ControlBarAutoAdjust;
  panlTree.Visible := AVisible;
  spltMain.Visible := AVisible;
  // Make sure spltMain is on the right of panlTree
  // 确保 spltNote 在 lstbInfo 的右边
  spltMain.Left := Width;
end;

procedure TformMain.ToggleInfoBar(AVisible: Boolean);
begin
  actnToggleInfoBar.Checked := AVisible;
  ControlBarAutoAdjust;
  lstbInfo.Visible := AVisible;
  spltNote.Visible := AVisible;
  // Make sure spltNote is above lstbInfo
  // 确保 spltNote 在 lstbInfo 的上边
  spltNote.Top := 0;
end;

procedure TformMain.ToggleRecyBar(AVisible: Boolean);
begin
  if FInEdit and (FActiveTree = trevRecy) then begin
    CancelRename;
    if not AVisible then begin
      treeEnter(trevTree); // Active trevTree | 激活 trevTree
      memoNote.SetFocus;   // Defocused TreeView | 取消 TreeView 的焦点
    end;
  end;
  actnToggleRecyBar.Checked := AVisible;
  ControlBarAutoAdjust;
  trevRecy.Visible := AVisible;
  spltTree.Visible := AVisible;
  spltTree.Top := 0;
end;

// Need to achieve "Maximized -> FullScreen -> Resotre FullScreen -> Resotre Maximized" is normal.
// 需要实现“最大化 -> 全屏 -> 恢复全屏 -> 恢复最大化”是正常的。
procedure TformMain.FullScreen(Full: Boolean);
var
  Form: TForm;
begin
  if Full then begin
    Config.Maximized := WindowState = wsMaximized;
    if not Config.Maximized then
      Config.MainFormRect := BoundsRect;

    Config.FullScreen := True;
    BorderStyle := bsNone;
    BoundsRect  := Screen.DesktopRect;

    // Fix GTK2 bug | 修复 GTK2 Bug
    Form := TForm.Create(nil);
    Parent := Form;
    Parent := nil;
    Form.Free;
  end else begin
    // "Windows" needs to set WindowState to wsNormal before the FormShow event is fired.
    // “Windows”需要在触发 FormShow 事件之前将 WindowState 设置为 wsNormal
    WindowState := wsNormal;
    BoundsRect  := Config.MainFormRect;
    BorderStyle := bsSizeable;
    Config.FullScreen := False;

    // Used to rigger FormShow event
    // 用于触发 FormShow 事件
    Form := TForm.Create(nil);
    Parent := Form;
    Parent := nil;
    Form.Free;
  end;
end;

procedure TformMain.FullWindow(Full: Boolean);
begin
  Full := not Full;
  if Config.FullWindowHideMenuBar then ToggleMenuBar(Full);
  if Config.FullWindowHideToolBar then ToggleToolBar(Full);
  if Config.FullWindowHideStatBar then ToggleStatBar(Full);
  if Config.FullWindowHideTreeBar then ToggleTreeBar(Full);
  if Config.FullWindowHideInfoBar then ToggleInfoBar(Full);
  if Config.FullWindowHideRecyBar then ToggleRecyBar(Full);
end;

{ Size 尺寸 }

procedure TformMain.SetTreeBarWidth(AWidth: Integer);
begin
  panlTree.Width := AWidth;
  spltMain.Left := Width;
end;

procedure TformMain.SetInfoBarHeight(AHeight: Integer);
begin
  lstbInfo.Top := panlNote.Height - AHeight;
  lstbInfo.Height := AHeight;
  spltNote.Top := 0;
end;

procedure TformMain.SetRecyBarHeight(AHeight: Integer);
begin
  // Use panlNote because its Height is the same as panlTree and it will not
  // be hidden. Use hidden panlTree's Height will causes calculation error.
  // 使用 panlNote 是因为它的高度和 panlTree 是一样的，而且它不会被隐藏。
  // 如果使用隐藏的 panlTree 的高度值进行计算会出错。
  trevRecy.Top := panlNote.Height - AHeight;
  trevRecy.Height := AHeight;
  spltTree.Top := 0;
end;

procedure TformMain.ControlBarAutoSize;
begin
  // Disable automatic adjustment of controls
  // 禁用控件的自动调整功能
  DisableAutoSizing;

  if Config.TreeBarAutoSize then
    SetTreeBarWidth(Width * Config.TreeBarPercent div 100);

  if Config.InfoBarAutoSize then
    SetInfoBarHeight(panlNote.Height * Config.InfoBarPercent div 100);

  if Config.RecyBarAutoSize then
    SetRecyBarHeight(panlNote.Height * Config.RecyBarPercent div 100);

  // Enable automatic adjustment of controls
  // 启用控件的自动调整功能
  EnableAutoSizing;
end;

procedure TformMain.ControlBarAutoAdjust;
begin
  if panlTree.Width > Width - 50 then begin
    panlTree.Width := Width - 50;
    // Make sure the Splitter is on the right of panlTree
    // 确保分割条在 panlTree 的右边
    spltMain.Left := Width;
  end;

  if lstbInfo.Height > panlNote.Height - 50 then begin
    lstbInfo.Height := panlNote.Height - 50;
    // Make sure the Splitter is above lstbInfo
    // 确保分割条在 lstbInfo 的上面
    lstbInfo.top := 50;
    spltNote.Top := 0;
  end;

  if trevRecy.Height > panlNote.Height - 50 then begin
    trevRecy.Height := panlNote.Height - 50;
    // Make sure the Splitter is above trevRecy
    // 确保分割条在 trevRecy 的上面
    trevRecy.top := 50;
    spltTree.Top := 0;
  end;
end;

{ ============================================================ }
{ Theme 主题                                                   }
{ ============================================================ }

procedure TformMain.actnToggleThemeExecute(Sender: TObject);
begin
  if FActiveTheme = 0 then begin
    if actnToggleTheme.Checked then begin
      actnToggleTheme.Checked := False;
      Config.ActiveTheme := BrightThemeID;
    end else begin
      actnToggleTheme.Checked := True;
      Config.ActiveTheme := DarkThemeID;
    end;
    Exit; // File not opened | 文件未打开
  end;

  if FActiveTheme = 1 then
    FActiveTheme := 2
  else
    FActiveTheme := 1;

  SetActiveTheme(FActiveTheme);
end;

procedure TformMain.SetForeColor(AColor: TColor);
begin
  trevTree.ExpandSignColor := AColor;
  trevTree.TreeLineColor   := AColor;
  trevTree.Font.Color      := AColor;

  trevRecy.ExpandSignColor := AColor;
  trevRecy.TreeLineColor   := AColor;
  trevRecy.Font.Color      := AColor;

  memoNote.Font.Color      := AColor;
  lstbInfo.Font.Color      := AColor;

  // editRename.Font.Color    := AColor;
end;

procedure TformMain.SetBackColor(AColor: TColor);
begin
  panlTree.Color           := AColor;
  panlNote.Color           := AColor;

  trevTree.BackGroundColor := AColor;
  trevRecy.BackGroundColor := AColor;

  memoNote.Color           := AColor;
  lstbInfo.Color           := AColor;

  // editRename.Color         := AColor;
end;

procedure TformMain.SetActiveTheme(AThemeID: Integer);
begin
  FActiveTheme := AThemeID;
  case AThemeID of
    BrightThemeID: begin
      SetForeColor(Config.BrightForeColor);
      SetBackColor(Config.BrightBackColor);
      actnToggleTheme.Checked := False;
      Config.ActiveTheme := BrightThemeID;
    end;
    DarkThemeID: begin
      SetForeColor(Config.DarkForeColor);
      SetBackColor(Config.DarkBackColor);
      actnToggleTheme.Checked := True;
      Config.ActiveTheme := DarkThemeID;
    end;
    else begin // File not opened | 文件未打开
      SetForeColor($000000);
      SetBackColor($808080);
    end;
  end;
end;

{ Font Name 字体名称 }

procedure TformMain.SetWindowFontName(AName: string);
begin
  if Screen.Fonts.IndexOf(AName) = -1 then
    AName := 'default';
  Font.Name := AName;
end;

procedure TformMain.SetTreeBarFontName(AName: string);
begin
  if Screen.Fonts.IndexOf(AName) = -1 then
    AName := 'default';
  trevTree.Font.Name := AName;
  trevRecy.Font.Name := AName;
  editRename.Font.Name := AName;
end;

procedure TformMain.SetNoteBarFontName(AName: string);
begin
  if Screen.Fonts.IndexOf(AName) = -1 then
    AName := 'default';
  memoNote.Font.Name := AName;
end;

procedure TformMain.SetInfoBarFontName(AName: string);
begin
  if Screen.Fonts.IndexOf(AName) = -1 then
    AName := 'default';
  lstbInfo.Font.Name := AName;
end;

{ Font Size 字体大小 }

procedure TformMain.SetWindowFontSize(ASize: Integer); inline;
begin
  Font.Size := ASize;
end;

procedure TformMain.SetTreeBarFontSize(ASize: Integer);
begin
  trevTree.Font.Size := ASize;
  trevRecy.Font.Size := ASize;
  editRename.Font.Size := ASize;

  // Used for the Options dialog | 用于选项对话框
  Config.TreeBarFontSize := ASize;

  // Size of Expand Sign | “展开标记”的大小
  if Config.ExpandSignSize > 0 then
    ASize := Config.ExpandSignSize
  else
    ASize := DefExpandSignSize;

  trevTree.ExpandSignSize := ASize;
  trevRecy.ExpandSignSize := ASize;
end;

procedure TformMain.SetNoteBarFontSize(ASize: Integer); inline;
begin
  memoNote.Font.Size := ASize;

  // Used for the Options dialog | 用于选项对话框
  Config.NoteBarFontSize := ASize;
end;

procedure TformMain.SetInfoBarFontSize(ASize: Integer); inline;
begin
  lstbInfo.Font.Size := ASize;

  // Used for the Options dialog | 用于选项对话框
  Config.InfoBarFontSize := ASize;
end;

procedure TformMain.MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  Size: Integer;
begin
  CancelRename;
  if not (ssCtrl in Shift) then Exit;

  if ssShift in Shift then Size := 10 else Size := 1;
  if WheelDelta < 0 then Size := -Size;

  if (Sender as TControl).Font.Size = 0 then
    Size := DefFontSize + Size
  else
    Size := (Sender as TControl).Font.Size + Size;

  if Size <= 0 then Size := 1;

  case (Sender as TControl).Name of
    'memoNote': SetNoteBarFontSize(Size);
    'trevTree': SetTreeBarFontSize(Size);
    'trevRecy': SetTreeBarFontSize(Size);
    'lstbInfo': SetInfoBarFontSize(Size);
  end;
end;

procedure TformMain.treeCustomDrawArrow(Sender: TCustomTreeView; const ARect: TRect; ACollapsed: Boolean);
begin
  Sender.Canvas.Rectangle(ARect);
  with ARect do
  begin
    Sender.Canvas.Line(Left + 2, Top + Height div 2, Right - 2, Top + Height div 2);
    if ACollapsed then
      Sender.Canvas.Line(Left + Width div 2, Top + 2, Left + Width div 2, Top + Height - 2);
  end;
end;

{ ============================================================ }
{ Memo 文本框                                                  }
{ ============================================================ }

procedure TformMain.actnWordWrapExecute(Sender: TObject);
begin
  ToggleWordWrap(not actnWordWrap.Checked);
end;

procedure TformMain.ToggleWordWrap(AWordWrap: Boolean);
begin
  actnWordWrap.Checked := AWordWrap;
  memoNote.WordWrap := AWordWrap;
end;

// Remove "MenuBar" item from memoNote's context menu.
// 从 memoNote 的上下文菜单中移除“菜单栏”条目
procedure TformMain.RemoveMenuBarItem(ARemove: Boolean);
var
  NotExist: Boolean;
  LastItem: TMenuItem;
begin
  NotExist := menuNote.Items.IndexOf(pmiToggleMenuBar) = -1;
  if ARemove = NotExist then Exit;

  if ARemove then begin
    // Remove "MenuBar" Item | 删除 MenuBar 菜单项
    menuNote.Items.Remove(pmiToggleMenuBar);
    // Remove the end Separator | 删除末端的分隔条
    if menuNote.Items.Count > 0 then begin
      LastItem := menuNote.Items[menuNote.Items.Count - 1];
      if LastItem.Caption = '-' then begin
        menuNote.Items.Remove(LastItem);
        LastItem.Free;
      end;
    end;
  end else begin
    // Add the end Separator | 添加末端的分隔条
    if menuNote.Items.Count > 0 then begin
      LastItem := menuNote.Items[menuNote.Items.Count - 1];
      if LastItem.Caption <> '-' then begin
        LastItem := TMenuItem.Create(Self);
        LastItem.Caption := '-';
        menuNote.Items.Add(LastItem);
      end;
    end;
    // Add "MenuBar" Item | 添加 MenuBar 菜单项
    menuNote.Items.Add(pmiToggleMenuBar);
  end;
end;

{ ============================================================ }
{ Config 配置信息                                              }
{ ============================================================ }

procedure TformMain.SaveConfig;
begin
  if (WindowState = wsNormal) and not Config.FullScreen then
    Config.MainFormRect := BoundsRect;

  Config.Maximized         := WindowState = wsMaximized;

  Config.MenuBarVisible    := Menu <> nil;
  Config.ToolBarVisible    := tbarMain.Visible;
  Config.StatBarVisible    := statMain.Visible;
  Config.TreeBarVisible    := panlTree.Visible;
  Config.InfoBarVisible    := lstbInfo.Visible;
  Config.RecyBarVisible    := trevRecy.Visible;

  Config.TreeBarWidth      := panlTree.Width;
  Config.InfoBarHeight     := lstbInfo.Height;
  Config.RecyBarHeight     := trevRecy.Height;

  Config.WindowFontSize    := Font.Size;
  Config.TreeBarFontSize   := trevTree.Font.Size;
  Config.NoteBarFontSize   := memoNote.Font.Size;
  Config.InfoBarFontSize   := lstbInfo.Font.Size;

  Config.WordWrap          := memoNote.WordWrap;

  Config.Save;
end;

{ ============================================================ }
{ Options Dialog 选项对话框                                    }
{ ============================================================ }

procedure TformMain.actnOptionsExecute(Sender: TObject);
begin
  if formOptions = nil then
    Application.CreateForm(TformOptions, formOptions);
  formOptions.ShowModal;
end;

procedure TformMain.LoadControlState;
begin
  SetWindowFontName(Config.WindowFontName);
  SetTreeBarFontName(Config.TreeBarFontName);
  SetNoteBarFontName(Config.NoteBarFontName);
  SetInfoBarFontName(Config.InfoBarFontName);

  SetWindowFontSize(Config.WindowFontSize);
  SetTreeBarFontSize(Config.TreeBarFontSize);
  SetNoteBarFontSize(Config.NoteBarFontSize);
  SetInfoBarFontSize(Config.InfoBarFontSize);

  ControlBarAutoSize;

  RemoveMenuBarItem(Config.RemoveMenuBarItem);

  if FActiveTheme <> 0 then
    SetActiveTheme(Config.ActiveTheme);

  HistoryMaxSize  := Config.HistoryMaxSize * 1024;
  HistoryMinCount := Config.HistoryMinCount;

  if (FAutoSaveRemaining = 0) or (FAutoSaveRemaining > Config.AutoSaveInterval) then
    FAutoSaveRemaining := Config.AutoSaveInterval;

  if FAutoBackupRemaining > Config.AutoBackupInterval then
    FAutoBackupRemaining := Config.AutoBackupInterval;

  if not FChangedAfterBackup then
    FAutoBackupRemaining := Config.AutoBackupInterval;

  statMain.Panels[1].Text := Format(Res_AutoSaveInfo, [FAutoSaveRemaining, FAutoBackupRemaining]);
end;

