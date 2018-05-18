
{ ============================================================ }
{ Search And Replace 搜索和替换                                }
{ ============================================================ }

procedure TformMain.actnSearchExecute(Sender: TObject);
begin
  if formSearch = nil then
    Application.CreateForm(TformSearch, formSearch);
  formSearch.Show;
end;

procedure TformMain.lstbInfoDblClick(Sender: TObject);
begin
  ShowSearchResult;
  // Prepare to focus the memoNote | 准备将焦点转移到备忘录
  FCheckSearchResult := True;
end;

// Focus the memoNote | 将焦点转移到备忘录
procedure TformMain.lstbInfoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FCheckSearchResult then begin
    memoNote.SetFocus;
    FCheckSearchResult := False;
  end;
end;

procedure TformMain.lstbInfoSelectionChange(Sender: TObject; User: boolean);
begin
  statMain.Panels[2].Text := Format(Res_SearchResultIndex,
    [lstbInfo.ItemIndex + 1, lstbInfo.Items.Count]);
end;

procedure TformMain.lstbInfoKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) then begin
    ShowSearchResult;
  end;
end;

procedure TformMain.actnPrevSearchResultExecute(Sender: TObject);
begin
  if lstbInfo.ItemIndex <= 0 then
    lstbInfo.ItemIndex := lstbInfo.Count - 1
  else
    lstbInfo.ItemIndex := lstbInfo.ItemIndex - 1;
  ShowSearchResult;
end;

procedure TformMain.actnNextSearchResultExecute(Sender: TObject);
begin
  if lstbInfo.ItemIndex = lstbInfo.Count - 1 then
    lstbInfo.ItemIndex := 0
  else
    lstbInfo.ItemIndex := lstbInfo.ItemIndex + 1;
  ShowSearchResult;
end;

// Process history and write changes to the database
// 处理历史记录并将更改写入数据库
function TformMain.ProcessHistoryData(ID: Integer; const Note, NewNote: string): TSteps;
var
  Node: TTreeNode;
  OldHistoryDataSize: SizeInt;
begin
  Result := nil;
  Node := IDToNode(ID, True);
  if Node = nil then Exit;

  // Process the history
  // 处理历史记录
  if Node = FActiveNode then
    Result := FHistory.GetHistoryData
  else
    Result := GetNodeHistory(Node);

  OldHistoryDataSize := 0;
  if Result = nil then
    Result := TSteps.Create
  else
    OldHistoryDataSize := Result.Size;

  // FullChange will return false if Note=NewNote
  // 如果 Note=NewNote 则 FullChange 将返回 False
  if Result.FullChange(Note, NewNote) then begin
    if Node = FActiveNode then
      FHistory.SetHistoryData(Result, NewNote)
    else begin
      SetNodeHistory(Node, Result);
      Inc(FHistorySize, Result.Size - OldHistoryDataSize);
    end;
    // Write changes to the database | 将更改写入数据库
    FTreeDB.SetNote(ID, NewNote);
  end else if Result.Count = 0 then
    FreeAndNil(Result)
  else
    Result := nil;
end;

{ General Search 普通搜索 }

procedure TformMain.Search(SearchText: string; SearchInName,
  SearchInNote: boolean; Depth: integer; IgnoreCase: boolean; Filter: string);
begin
  ClearSearchResult;

  SearchProcess(SearchText, SearchInName, SearchInNote,
    Depth, IgnoreCase, Filter);

  LoadSearchResult;
end;

procedure TformMain.SearchProcess(SearchText: string; SearchInName,
  SearchInNote: boolean; Depth: integer; IgnoreCase: boolean; Filter: string);

var
  FilterRegExpr            : TRegExpr;
  // Search from | 搜索源
  AName, ANote             : string;
  // Temporary search results | 临时搜索结果
  Matches                  : TSizeIntArray;
  // Length of Search Text | 搜索内容的长度
  SearchLen                : SizeInt;
  // Current search count limit | 当前搜索数量限制
  SearchCount              : Integer;
  // Total search count limit | 总搜索数量限制
  CountLimit               : Integer;

  // Recursively search | 递归搜索
  procedure DoSearch(AID: Integer);
  var
    i: Integer;
  begin
    // Count limit | 数量限制
    if FSearchResult.Count >= CountLimit then begin
      FSearchResult.SearchAbort := True;
      Exit;
    end;

    // Name Filter | 名称过滤
    AName := FTreeDB.GetName(AID);
    if (FilterRegExpr = nil) or FilterRegExpr.Exec(AName) then begin

      // Search in node name | 在节点名称中搜索
      if SearchInName and (StringPos(AName, SearchText, 1, IgnoreCase) > 0) then
        // Record the result | 记录搜索结果
        FSearchResult.Add(AID, 0, 0);

      // Search in node note | 在节点内容中搜索
      if SearchInNote then begin
        ANote := FTreeDB.GetNote(AID);
        Inc(FSearchResult.SearchSize, Length(ANote));
        // Current search count limit | 当前搜索数量限制
        SearchCount :=  CountLimit- FSearchResult.Count;
        // Start search | 开始搜索
        UTF8FindMatches(ANote, SearchText, Matches, SearchCount, IgnoreCase);
        // Record the result | 记录搜索结果
        for i := 0 to High(Matches) do
          FSearchResult.Add(AID, Matches[i], SearchLen);
      end;

    end;

    // Depth limit | 深度限制
    Dec(Depth);
    if (Depth <> 0) and (not FSearchResult.SearchAbort) then
      for AID in FTreeDB.GetChildren(AID) do
        DoSearch(AID);
    Inc(Depth);
  end;

var
  ID: Integer;
begin
  // Check parameter | 参数检查
  if (FActiveNode = nil) or (SearchText = '') or (Depth = 0) then Exit;

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

  // The length of the Search Text, used to write to FSearchResult.
  // 搜索内容的长度，用于写入 FSearchResult
  SearchLen := UTF8LengthFast(SearchText);

  // Enter recursive search | 进入递归搜索
  if ID < FirstID then
    for ID in FTreeDB.GetChildren(ID) do
      DoSearch(ID)
  else
    DoSearch(ID);
end;

{ General Replace 普通替换 }

procedure TformMain.Replace(SearchText, ReplaceText: string; SearchInName,
  SearchInNote: boolean; Depth: integer; IgnoreCase: Boolean; Filter: string);
begin
  ClearSearchResult;

  ReplaceProcess(SearchText, ReplaceText, SearchInName, SearchInNote,
    Depth, IgnoreCase, Filter);

  LoadSearchResult;
end;

procedure TformMain.ReplaceProcess(SearchText, ReplaceText: string; SearchInName,
  SearchInNote: boolean; Depth: integer; IgnoreCase: Boolean; Filter: string);
var
  FilterRegExpr            : TRegExpr;
  // Search from | 搜索源
  AName, ANote             : string;
  // Replaced result | 替换结果
  NewName, NewNote         : string;
  // Temporary search results | 临时搜索结果
  Matches                  : TSizeIntArray;
  // Length of Replace Text | 替换内容的长度
  ReplaceLen               : Integer;
  // Current search count limit | 当前搜索数量限制
  SearchCount              : Integer;
  // Total search count limit | 总搜索限制
  CountLimit               : Integer;
  // Used to process history | 用于处理历史记录
  ANode                    : TTreeNode;

  // Recursively search | 递归搜索
  procedure DoReplace(AID: Integer);
  var
    i: Integer;
  begin
    // Count limit | 数量限制
    if FSearchResult.Count >= CountLimit then begin
      FSearchResult.SearchAbort := True;
      Exit;
    end;

    // Name Filter | 名称过滤
    AName := FTreeDB.GetName(AID);
    if (FilterRegExpr = nil) or FilterRegExpr.Exec(AName) then begin

      // Search in node name | 在节点名称中搜索
      if SearchInName then begin
        NewName := ReplaceMatches(AName, SearchText, ReplaceText, Matches, 0, IgnoreCase);
        if Length(Matches) > 0 then begin
          // Record the result | 记录搜索结果
          FSearchResult.Add(AID, 0, 0);

          // Write changes to the database | 将更改写入数据库
          FTreeDB.SetName(AID, NewName);

          // Update node name in TreeView | 更新 TreeView 中的节点名称
          ANode := IDToNode(AID, SearchInNote);
          if ANode <> nil then
            ANode.Text := NewName;
        end;
      end;

      // Search in node note | 在节点内容中搜索
      if SearchInNote then begin
        ANote := FTreeDB.GetNote(AID);

        Inc(FSearchResult.SearchSize, Length(ANote));

        // Current search count limit | 当前搜索数量限制
        SearchCount := CountLimit - FSearchResult.Count;
        // Star replace | 开始替换
        NewNote := UTF8ReplaceMatches(ANote, SearchText, ReplaceText, Matches, SearchCount, IgnoreCase);

        // Process history and write changes to the database
        // 处理历史记录并将更改写入数据库
        if Length(Matches) > 0 then
          ProcessHistoryData(AID, ANote, NewNote);

        // Record search results | 记录搜索结果
        for i := 0 to High(Matches) do
          FSearchResult.Add(AID, Matches[i], ReplaceLen);
      end;

    end;

    // Depth limit | 深度限制
    Dec(Depth);
    if (Depth <> 0) and (not FSearchResult.SearchAbort) then
      for AID in FTreeDB.GetChildren(AID) do
        DoReplace(AID);
    Inc(Depth);
  end;
var
  ID: Integer;
begin
  // Check parameter | 参数检查
  if (FActiveNode = nil) or (SearchText = '') or (Depth = 0) then Exit;

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

  // The length of the Replace Text, used to write to FSearchResult
  // 替换内容的长度，用于写入 FSearchResult
  ReplaceLen := UTF8LengthFast(ReplaceText);

  // Enter recursive search | 进入递归搜索
  if ID < FirstID then
    for ID in FTreeDB.GetChildren(ID) do
      DoReplace(ID)
  else
    DoReplace(ID);

  // Mark the database has changed | 标记数据库被改变
  if FSearchResult.Count > 0 then DBChanged := True;
end;

{ Regular Expression Search 正则表达式搜索 }

procedure TformMain.RegExprSearch(SearchText: string; SearchInName,
  SearchInNote: boolean; Depth: integer; Filter: string);
begin
  ClearSearchResult;

  RegExprSearchProcess(SearchText, SearchInName, SearchInNote,
    Depth, Filter);

  LoadSearchResult;
end;

procedure TformMain.RegExprSearchProcess(SearchText: string; SearchInName,
  SearchInNote: boolean; Depth: integer; Filter: string);
var
  // Search from | 搜索源
  AName, ANote: string;
  // Total search limit | 总搜索限制
  CountLimit               : Integer;
  // Regular expression object | 正则表达式对象
  Expr, FilterRegExpr      : TRegExpr;
  // Regular expression execution result | 正则表达式执行结果
  Found                    : Boolean;

  // Recursively search | 递归搜索
  procedure DoSearch(AID: Integer);
  begin
    // Count limit | 数量限制
    if FSearchResult.Count >= CountLimit then begin
      FSearchResult.SearchAbort := True;
      Exit;
    end;

    // Name Filter | 名称过滤
    AName := FTreeDB.GetName(AID);
    if (FilterRegExpr = nil) or FilterRegExpr.Exec(AName) then begin

      // Search in node name | 在节点名称中搜索
      if SearchInName and Expr.Exec(AName) then
        // Record the result | 记录搜索结果
        FSearchResult.Add(AID, 0, 0);

      // Search in node note | 在节点内容中搜索
      if SearchInNote then begin
        ANote := FTreeDB.GetNote(AID);
        Inc(FSearchResult.SearchSize, Length(ANote));
        // Start search | 开始搜索
        Found := Expr.Exec(ANote);
        while Found and (FSearchResult.Count < CountLimit) do begin
          // Record the result | 记录搜索结果
          FSearchResult.Add(AID, Expr.MatchPos[0], Expr.MatchLen[0]);
          // Search next | 搜索下一个
          Found := Expr.ExecNext;
        end;
      end;

    end;

    // Depth limit | 深度限制
    Dec(Depth);
    if (Depth <> 0) and (not FSearchResult.SearchAbort) then
      for AID in FTreeDB.GetChildren(AID) do
        DoSearch(AID);
    Inc(Depth);
  end;

var
  ID: Integer;
begin
  // Check parameter | 参数检查
  if (FActiveNode = nil) or (SearchText = '') or (Depth = 0) then Exit;

  // Initialize search limit | 初始化搜索限制
  if Config.SearchCountLimit <= 0 then
    CountLimit := MaxInt
  else
    CountLimit := Config.SearchCountLimit;

  // Get the entry node | 获取入口节点
  if Depth <> WholeTree then
    ID := GetNodeID(FActiveNode)
  else if FActiveNode.TreeView = trevTree then
    ID := RootID
  else
    ID := RecyclerID;

  Expr := TRegExpr.Create(SearchText);

  FilterRegExpr := nil;
  if Filter <> '' then FilterRegExpr := TregExpr.Create(Filter);

  // Enter recursive search | 进入递归搜索
  if ID < FirstID then
    for ID in FTreeDB.GetChildren(ID) do
      DoSearch(ID)
  else
    DoSearch(ID);

  Expr.Free;
  if FilterRegExpr <> nil then FilterRegExpr.Free;
end;

{ Regular Expression Replace 正则表达式替换 }

procedure TformMain.RegExprReplace(SearchText, ReplaceText: string; SearchInName,
  SearchInNote: boolean; Depth: integer; Filter: string);
begin
  ClearSearchResult;

  RegExprReplaceProcess(SearchText, ReplaceText, SearchInName, SearchInNote,
    Depth, Filter);

  LoadSearchResult;
end;

procedure TformMain.RegExprReplaceProcess(SearchText, ReplaceText: string; SearchInName,
  SearchInNote: boolean; Depth: integer; Filter: string);
var
  // Search from | 搜索源
  ANote                    : string;
  // Replaced result | 替换结果
  Buf                      : TStringBuffer;
  NewNote                  : string;
  // Regular expression object | 正则表达式对象
  Expr, FilterRegExpr      : TRegExpr;
  // Regular expression execution result | 正则表达式执行结果
  Found                    : Boolean;
  // The character position after the previous match
  // 前一匹配结果之后的字符位置
  SearchStart              : SizeInt;
  // The character position after the previous replaced result
  // 前一替换结果之后的字符位置
  ReplaceStart             : SizeInt;
  // The length of current replaced result | 当前替换结果的长度
  ReplaceLength            : Integer;
  // The replace result of single match | 单个匹配项的替换结果
  CurrReplaced             : string;
  // The string between two matches | 两个匹配项之间的字符串
  MiddleString             : string;
  // Used to process history | 用于处理历史记录
  ANode                    : TTreeNode;
  // Total search limit | 总搜索限制
  CountLimit               : Integer;

  // Replace single node's content | 替换单个节点的内容
  procedure ReplaceNote(AID: Integer);
  begin
    ANote := FTreeDB.GetNote(AID);
    Inc(FSearchResult.SearchSize, Length(ANote));

    SearchStart     := 1;
    ReplaceStart    := 1;
    ReplaceLength   := 0;

    Buf.Reset;

    // Start search | 开始搜索
    Found := Expr.Exec(ANote);

    while Found do
    begin
      // Get the string between two matches
      // 获取两个匹配项之间的字符串
      MiddleString := UTF8Copy(ANote, SearchStart, Expr.MatchPos[0] - SearchStart);

      // Get single replaced result | 获取单个替换结果
      CurrReplaced := ReplaceRegExpr(SearchText, Expr.Match[0], ReplaceText, True);

      // Record the position and length of the replaced text
      // 记录替换内容的位置和长度
      ReplaceStart := ReplaceStart + UTF8LengthFast(MiddleString);
      ReplaceLength := UTF8LengthFast(CurrReplaced);

      // Record the result | 记录替换结果
      FSearchResult.Add(AID, ReplaceStart, ReplaceLength);

      // Write into Result | 写入替换结果
      Buf.WriteString(MiddleString);
      Buf.WriteString(CurrReplaced);

      // Update the start position of search and replace
      // 更新查找和替换的起始位置
      SearchStart := Expr.MatchPos[0] + Expr.MatchLen[0];
      ReplaceStart := ReplaceStart + ReplaceLength;

      // Search next | 搜索下一个
      Found := Expr.ExecNext;
    end;
    // SearchStart = 1 means matched nothing
    // SearchStart = 1 表示没有匹配到任何东西
    if SearchStart > 1 then begin
      // Write the content after the last match into the Result
      // 将最后一个查找结果之后的内容写入 Result 中
      MiddleString := UTF8Copy(ANote, SearchStart, MaxInt);
      Buf.WriteString(MiddleString);
      NewNote := Buf.DataString;

      // Process history and write changes to the database
      // 处理历史记录并将更改写入数据库
      ProcessHistoryData(AID, ANote, NewNote);
    end;
  end;

var
  // Search from | 搜索源
  AName                    : string;
  // Replaced result | 替换结果
  NewName                  : string;

  // Recursively search | 递归搜索
  procedure DoReplace(AID: Integer);
  begin
    // Count limit | 数量限制
    if FSearchResult.Count >= CountLimit then begin
      FSearchResult.SearchAbort := True;
      Exit;
    end;

    // Name Filter | 名称过滤
    AName := FTreeDB.GetName(AID);
    if (FilterRegExpr = nil) or FilterRegExpr.Exec(AName) then begin

      // Search in node name | 在节点名称中搜索
      if SearchInName then begin
        if Expr.Exec(AName) then begin
          NewName := Expr.Replace(AName, ReplaceText, True);
          // Record the result | 记录搜索结果
          FSearchResult.Add(AID, 0, 0);
          // Write changes to the database | 将更改写入数据库
          FTreeDB.SetName(AID, NewName);
          // Update node name in TreeView
          // 更新 TreeView 中的节点名称
          ANode := IDToNode(AID, SearchInNote);
          if ANode <> nil then
            ANode.Text := NewName;
        end;
      end;

      // Search in node note | 在节点内容中搜索
      if SearchInNote then ReplaceNote(AID);

    end;

    // Depth limit | 深度限制
    Dec(Depth);
    if (Depth <> 0) and (not FSearchResult.SearchAbort) then
      for AID in FTreeDB.GetChildren(AID) do
        DoReplace(AID);
    Inc(Depth);
  end;

var
  ID: Integer;
begin
  // Check parameter | 参数检查
  if (FActiveNode = nil) or (SearchText = '') or (Depth = 0) then Exit;

  // Initialize search limit | 初始化搜索限制
  if Config.SearchCountLimit <= 0 then
    CountLimit := MaxInt
  else
    CountLimit := Config.SearchCountLimit;

  // Get the entry node | 获取入口节点
  if Depth <> WholeTree then
    ID := formMain.GetNodeID(formMain.FActiveNode)
  else if formMain.FActiveNode.TreeView = formMain.trevTree then
    ID := RootID
  else
    ID := RecyclerID;

  Expr := TRegExpr.Create(SearchText);

  FilterRegExpr := nil;
  if Filter <> '' then FilterRegExpr := TregExpr.Create(Filter);

  Buf := TStringBuffer.Create;

  // Enter recursive search | 进入递归搜索
  if ID < FirstID then
    for ID in FTreeDB.GetChildren(ID) do
      DoReplace(ID)
  else
    DoReplace(ID);

  Expr.Free;
  if FilterRegExpr <> nil then FilterRegExpr.Free;
  Buf.Free;

  // Mark the database has changed | 标记数据库被改变
  if FSearchResult.Count > 0 then DBChanged := True;
end;

{ Search Result 搜索结果 }

procedure TformMain.ClearSearchResult;
begin
  actnPrevSearchResult.Enabled := False;
  actnNextSearchResult.Enabled := False;
  lstbInfo.Clear;
  FSearchResult.Clear;
end;

// List search results in lstbInfo
// 将搜索结果在 lstbInfo 中列出
procedure TformMain.LoadSearchResult;
var
  i                        : Integer;
  CurRecord                : PSearchRecord;
  ID                       : Integer;
  Start                    : SizeInt;
  Len                      : SizeInt;
  AName, ANote             : string;
  Sample                   : string;
  // Used to compatible with line breaks on different platforms
  // 用于兼容不同平台上的换行符
  PNote                    : PChar;
  UPos                     : SizeInt;
  BPos                     : SizeInt;
  UEnd                     : SizeInt;
  FixNum                   : SizeInt;
begin
  // If you perform a paste operation and then perform a search operation,
  // the contents of the paste may contain \r. To avoid an error in the display
  // of search results, reload the content of the node here.
  // 执行了粘贴操作之后再执行搜索操作，则粘贴的内容中可能含有 \r，为了避免
  // 搜索结果显示错误，这里重新载入节点内容
  LoadNote(FActiveNode);

  lstbInfo.Items.BeginUpdate;

  for i := 0 to FSearchResult.Count - 1 do begin
    CurRecord := FSearchResult[i];
    ID        := CurRecord^.ID;
    Start     := CurRecord^.UStart;
    Len       := CurRecord^.ULength;

    AName     := FTreeDB.GetName(ID);

    // Start is based 1, Start=0 means "the result was searched in the node name"
    // Start 是从 1 开始计数的，Start=0 表示“在节点名称中搜索的结果”
    if Start = 0 then begin
      // Search in node name | 在节点名称中搜索
      lstbInfo.Items.Add(AName);
    end else begin
      // Search in node note | 在节点内容中搜索
      ANote := FTreeDB.GetNote(ID);

      // Get the sample text of the result
      // 获取结果的示例文本
      if Start <= 10 then
        Sample := UTF8Copy(ANote, 1, 20)
      else
        Sample := UTF8Copy(ANote, Start - 10, 20);
      Sample := StringReplace(Sample, #10, ' ', 0, False);

      // The default Line Feed Character used by this program is #10, but the
      // Line Feed Character used by TMemo in Windows is #13#10, which are
      // different in length, so they need to be patched to get the correct SelStart.
      // 本程序使用的默认换行符是 #10，而 Windows 中的 TMemo 使用的换行符
      // 是 #13#10，长度不同，所以需要修补才能得到正确的 SelStart
      if FSearchResult.FixLineEnding and (Length(LineEnding) = 2) then begin
        PNote := PChar(ANote);
        // Fix SelStart
        UPos := 1;
        BPos := 0;
        UEnd := Start;
        FixNum := 0;
        while UPos < UEnd do begin
          case PNote[BPos] of
            #10: begin Inc(BPos); Inc(FixNum);end;
            #192..#223: Inc(BPos, 2);
            #224..#239: Inc(BPos, 3);
            #240..#247: Inc(BPos, 4);
            else Inc(BPos);
          end;
          Inc(UPos);
        end;
        CurRecord^.UStart := Start + FixNum;
        // Fix SelLength
        UEnd := Start + Len - 1;
        FixNum := 0;
        while UPos < UEnd do begin
          case PNote[BPos] of
            #10: begin Inc(BPos); Inc(FixNum);end;
            #192..#223: Inc(BPos, 2);
            #224..#239: Inc(BPos, 3);
            #240..#247: Inc(BPos, 4);
            else Inc(BPos);
          end;
          Inc(UPos);
        end;
        CurRecord^.ULength := Len + FixNum;
      end;

      // Write the information to the list box | 将信息写入列表框
      lstbInfo.Items.Add(Format('[%s] %s | (%d %d)', [AName, Sample,
        CurRecord^.UStart, CurRecord^.ULength]));
    end;
  end;
  lstbInfo.Items.EndUpdate;

  if FSearchResult.ShowResultList then ToggleInfoBar(True);

  actnPrevSearchResult.Enabled := FSearchResult.Count > 0;
  actnNextSearchResult.Enabled := FSearchResult.Count > 0;

  ShowSearchInfo;
end;

// Locate the node and content of a search result
// 定位搜索结果所对应的节点及内容
procedure TformMain.ShowSearchResult;
var
  ANade: TTreeNode;
  SearchRecord: PSearchRecord;
begin
  if lstbInfo.ItemIndex < 0 then Exit;
  SearchRecord := FSearchResult[lstbInfo.ItemIndex];
  // Locate the relevant node | 定位相应节点
  ANade := IDToNode(SearchRecord^.ID, True);
  if ANade <> nil then ANade.Selected := True;

  // Locate the result in the memoNote | 在 memoNote 中定位结果
  memoNote.SelStart := SearchRecord^.UStart - 1;
  memoNote.SelLength := SearchRecord^.ULength;

  // Show result index | 显示结果索引
  statMain.Panels[2].Text := Format(Res_SearchResultIndex,
    [lstbInfo.ItemIndex + 1, lstbInfo.Items.Count]);
  // Focus the memoNote | 将焦点转移到备忘录
  memoNote.SetFocus;
end;

procedure TformMain.ShowSearchInfo;
var
  TimeUsed      : Int64;
  SearchSize    : Double;
  SearchSizeStr : string;
begin
  // Update search status information in the status bar
  // 在状态栏更新搜索状态信息
  TimeUsed := (GetTickCount64 - FSearchResult.TickCount) div 1000;
  SearchSize := FSearchResult.SearchSize;

  if SearchSize < 1024 then begin
    SearchSizeStr := Format('%.0fB', [SearchSize]);
  end else if SearchSize < 1024 * 1024 then begin
    SearchSize := SearchSize / 1024;
    SearchSizeStr := Format('%.1fKB', [SearchSize]);
  end else begin
    SearchSize := SearchSize / (1024 * 1024);
    SearchSizeStr := Format('%.1fMB', [SearchSize]);
  end;

  if FSearchResult.SearchAbort or not FSearchResult.SearchFinished then
    statMain.Panels[0].Text := Format(Res_ScarchResultPart,
      [FSearchResult.Count, SearchSizeStr, TimeUsed])
  else
    statMain.Panels[0].Text := Format(Res_ScarchResultTotal,
      [FSearchResult.Count, SearchSizeStr, TimeUsed]);
end;

{ ============================================================ }
{ TSearchResult                                                }
{ ============================================================ }

constructor TSearchResult.Create;
begin
  inherited Create;
  FList := TList.Create;

  SearchSize               := 0;
  SearchAbort              := False;
  SearchFinished           := True;
  TickCount                := 0;
  FixLineEnding            := True;
  ShowResultList           := True;
end;

destructor TSearchResult.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TSearchResult.Get(Index: integer): PSearchRecord;
begin
  Result := PSearchRecord(FList.Items[Index]);
end;

procedure TSearchResult.Put(Index: integer; Item: PSearchRecord);
begin
  FList.Items[Index] := Item;
end;

procedure TSearchResult.Add(PSR: PSearchRecord);
begin
  FList.Add(PSR);
end;

procedure TSearchResult.Add(ID: integer; UStart, ULength: SizeInt);
var
  CurRecord: PSearchRecord;
begin
  CurRecord := new(PSearchRecord);
  CurRecord^.ID      := ID;
  CurRecord^.UStart  := UStart;
  CurRecord^.ULength := ULength;

  FList.Add(CurRecord);
end;

procedure TSearchResult.Del(Index: integer);
begin
  dispose(PSearchRecord(FList.Items[Index]));
  FList.Delete(Index);
end;

function TSearchResult.Count: integer;
begin
  Result := FList.Count;
end;

procedure TSearchResult.Clear;
var
  i: integer;
begin
  for i := FList.Count - 1 downto 0 do
    dispose(Items[i]);

  FList.Clear;

  SearchSize               := 0;
  SearchAbort              := False;
  SearchFinished           := True;
  TickCount                := GetTickCount64;
  FixLineEnding            := True;
  ShowResultList           := True;
end;

