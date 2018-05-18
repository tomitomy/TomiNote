unit uConfig;

{$mode objfpc}{$H+}

// String constants and string literals require this option
// 字符串常量和字符串字面量需要此选项
{$codepage UTF8}

interface

uses
  Classes, SysUtils, IniFiles, Forms;

type

  { TConfig }

  TConfig = class

    { AppInit }

    KeepMainFormRect         : Boolean;
    MainFormRect             : TRect;

    Maximized                : Boolean;
    FullScreen               : Boolean;

    TreeBarWidth             : Integer;
    InfoBarHeight            : Integer;
    RecyBarHeight            : Integer;

    MenuBarVisible           : Boolean;
    ToolBarVisible           : Boolean;
    StatBarVisible           : Boolean;
    TreeBarVisible           : Boolean;
    InfoBarVisible           : Boolean;
    RecyBarVisible           : Boolean;

    WordWrap                 : Boolean;

    RecentFiles              : TStringList;

    LoadLastFile             : boolean;
    LastFile                 : string;

    SelectLastNode           : boolean;

    { Layout }

    TreeBarAutoSize          : Boolean;
    InfoBarAutoSize          : Boolean;
    RecyBarAutoSize          : Boolean;

    TreeBarPercent           : Integer;
    InfoBarPercent           : Integer;
    RecyBarPercent           : Integer;

    FullWindowHideMenuBar    : Boolean;
    FullWindowHideToolBar    : Boolean;
    FullWindowHideStatBar    : Boolean;
    FullWindowHideTreeBar    : Boolean;
    FullWindowHideInfoBar    : Boolean;
    FullWindowHideRecyBar    : Boolean;

    RemoveMenuBarItem        : Boolean;

    { Theme }

    ActiveTheme              : Integer;

    BrightForeColor          : Integer;
    BrightBackColor          : Integer;

    DarkForeColor            : Integer;
    DarkBackColor            : Integer;

    WindowFontName           : string;
    TreeBarFontName          : string;
    NoteBarFontName          : string;
    InfoBarFontName          : string;

    WindowFontSize           : Integer;
    TreeBarFontSize          : Integer;
    NoteBarFontSize          : Integer;
    InfoBarFontSize          : Integer;

    ExpandSignSize           : Integer;

    { Options }

    KeepOptionsFormRect      : Boolean;
    OptionsFormRect          : TRect;

    KeepImportFormRect       : Boolean;
    KeepExportFormRect       : Boolean;
    KeepSearchFormRect       : Boolean;
    KeepUtilsFormRect        : Boolean;

    SwapOKCancel             : Boolean;

    SearchCountLimit         : Integer;
    RecentCountLimit         : Integer;

    AutoSaveInterval         : Integer;
    AutoBackupInterval       : Integer;
    AutoBackupCount          : Integer;

    HistoryMaxSize           : Integer; // KB
    HistoryMinCount          : Integer;

    { Search }

    SearchFormRect           : TRect;
    SearchFrom               : Integer;
    RecentSearch             : TStringList;
    SearchText               : string;
    RecentReplace            : TStringList;
    ReplaceText              : string;
    SearchInName             : Boolean;
    SearchInNote             : Boolean;
    SearchIgnoreCase         : Boolean;
    UseNameFilter            : Boolean;
    NameFilter               : string;
    SearchUseRegExpr         : Boolean;
    SearchMultiLineMode      : Boolean;
    SearchNonGreedyMode      : Boolean;
    DoReplace                : Boolean;

    { Import }

    ImportFormRect           : TRect;
    ImportFileExt            : Boolean;
    ImportRootDir            : Boolean;
    ImportFrom               : Integer;
    ImportTo               : Integer;
    LastImportDir            : string;

    { Export }

    ExportFormRect           : TRect;
    ExportAddFileExt         : Boolean;
    ExportAddSeparator       : Boolean;
    ExportFileExt            : string;
    ExportSeparator          : string;
    ExportTo                 : Integer;
    ExportFrom               : Integer;
    LastExportDir            : string;

    { Utils }

    UtilsFormRect            : TRect;
    UtilsTabIndex            : Integer;

    { Utils - Sort }

    SortDirection            : Integer;
    SortOf                   : Integer;

    { Utils - Split }

    SplitSeparator           : string;
    SplitTitle               : string;

    RecentSeparator          : TStringList;
    RecentTitle              : TStringList;

    IgnoreCaseSp             : Boolean;
    MultiLineSp              : Boolean;
    NonGreedySp              : Boolean;

    IncludeSeparator         : Boolean;

    AddPreNumSp              : Boolean;
    PreNumLenSp              : Integer;

    { Utils - Rename }

    SearchTextRn             : string;
    ReplaceTextRn            : string;

    RecentSearchRn           : TStringList;
    RecentReplaceRn          : TStringList;

    IgnoreCaseRn             : Boolean;
    MultiLineRn              : Boolean;
    NonGreedyRn              : Boolean;

    SearchIn                 : Integer;

    SpecifyDepthRn           : Boolean;
    DepthRn                  : Integer;

    AddPreNumRn              : Boolean;
    PreNumLenRn              : Integer;

  private

    FConfigFile              : string;

  public

    constructor Create(ConfigFile: string);
    destructor Destroy; override;
    procedure Load;
    procedure Save;

  end;

var
  Config: TConfig;

implementation

uses
  uResources;

{ TConfig }

constructor TConfig.Create(ConfigFile: string);
begin
  inherited Create;
  FConfigFile := ConfigFile;

  RecentFiles              := TStringList.Create;
  RecentSearch             := TStringList.Create;
  RecentReplace            := TStringList.Create;
  RecentSeparator          := TStringList.Create;
  RecentTitle              := TStringList.Create;
  RecentSearchRn           := TStringList.Create;
  RecentReplaceRn          := TStringList.Create;
end;

destructor TConfig.Destroy;
begin
  RecentFiles              .Free;
  RecentSearch             .Free;
  RecentReplace            .Free;
  RecentSeparator          .Free;
  RecentTitle              .Free;
  RecentSearchRn           .Free;
  RecentReplaceRn          .Free;

  inherited Destroy;
end;

procedure TConfig.Load;
var
  Ini                      : TIniFile;
  i                        : Integer;
  Recent                   : string;
  Left, Top, Width, Height : Integer;
begin
  Ini := TIniFile.Create(FConfigFile, [ifoStripQuotes]);

  { AppInit }

  KeepMainFormRect         := Ini.ReadBool   ('AppInit', 'KeepMainFormRect', True);

  Left   := Ini.ReadInteger('AppInit', 'Left', (Screen.Width - 800) div 2);
  Top    := Ini.ReadInteger('AppInit', 'Top', (Screen.Height - 600) div 2);
  Width  := Ini.ReadInteger('AppInit', 'Width', 800);
  Height := Ini.ReadInteger('AppInit', 'Height', 600);

  if KeepMainFormRect and
  (Left >= 0) and (Left <= Screen.Width  - 64) and
  (Top  >= 0) and (Top  <= Screen.Height - 64) and
  ((Width > 64) or (Height > 64)) then begin
    MainFormRect.Left        := Left;
    MainFormRect.Top         := Top;
    MainFormRect.Width       := Width;
    MainFormRect.Height      := Height;
  end else begin
    MainFormRect.Left        := (Screen.Width - 800) div 2;
    MainFormRect.Top         := (Screen.Height - 600) div 2;
    MainFormRect.Width       := 800;
    MainFormRect.Height      := 600;
  end;

  Maximized                := Ini.ReadBool   ('AppInit', 'Maximized', False);
  FullScreen               := Ini.ReadBool   ('AppInit', 'FullScreen', False);

  MenuBarVisible           := Ini.ReadBool   ('AppInit', 'MenuBarVisible', True);
  ToolBarVisible           := Ini.ReadBool   ('AppInit', 'ToolBarVisible', True);
  StatBarVisible           := Ini.ReadBool   ('AppInit', 'StatBarVisible', True);
  TreeBarVisible           := Ini.ReadBool   ('AppInit', 'TreeBarVisible', True);
  InfoBarVisible           := Ini.ReadBool   ('AppInit', 'InfoBarVisible', False);
  RecyBarVisible           := Ini.ReadBool   ('AppInit', 'RecyBarVisible', False);

  TreeBarWidth             := Ini.ReadInteger('AppInit', 'TreeBarWidth', 160);
  InfoBarHeight            := Ini.ReadInteger('AppInit', 'InfoBarHeight', 100);
  RecyBarHeight            := Ini.ReadInteger('AppInit', 'RecyBarHeight', 100);

  WordWrap                 := Ini.ReadBool   ('AppInit', 'WordWrap', True);

  RecentFiles.Clear;
  for i := 1 to 10 do begin
    Recent := Ini.ReadString ('AppInit', 'RecentFile' + IntToStr(i), '');
    if Recent <> '' then RecentFiles.Add(Recent);
  end;

  LoadLastFile             := Ini.ReadBool   ('AppInit', 'LoadLastFile', True);
  LastFile                 := Ini.ReadString ('AppInit', 'LastFile', '');

  SelectLastNode           := Ini.ReadBool   ('AppInit', 'SelectLastNode', True);

  { Layout }

  TreeBarAutoSize          := Ini.ReadBool   ('Layout', 'TreeBarAutoSize', True);
  InfoBarAutoSize          := Ini.ReadBool   ('Layout', 'InfoBarAutoSize', True);
  RecyBarAutoSize          := Ini.ReadBool   ('Layout', 'RecyBarAutoSize', True);

  TreeBarPercent           := Ini.ReadInteger('Layout', 'TreeBarPercent', DefTreeBarPercent);
  InfoBarPercent           := Ini.ReadInteger('Layout', 'InfoBarPercent', DefInfoBarPercent);
  RecyBarPercent           := Ini.ReadInteger('Layout', 'RecyBarPercent', DefRecyBarPercent);

  FullWindowHideMenuBar    := Ini.ReadBool   ('Layout', 'FullWindowHideMenuBar', True);
  FullWindowHideToolBar    := Ini.ReadBool   ('Layout', 'FullWindowHideToolBar', True);
  FullWindowHideStatBar    := Ini.ReadBool   ('Layout', 'FullWindowHideStatBar', True);
  FullWindowHideTreeBar    := Ini.ReadBool   ('Layout', 'FullWindowHideTreeBar', True);
  FullWindowHideInfoBar    := Ini.ReadBool   ('Layout', 'FullWindowHideInfoBar', False);
  FullWindowHideRecyBar    := Ini.ReadBool   ('Layout', 'FullWindowHideRecyBar', False);

  RemoveMenuBarItem        := Ini.ReadBool   ('Layout', 'RemoveMenuBarItem', False);

  { Theme }

  ActiveTheme              := Ini.ReadInteger('Theme', 'ActiveTheme', BrightThemeID);

  BrightForeColor          := Ini.ReadInteger('Theme', 'BrightFontColor', DefBrightForeColor);
  BrightBackColor          := Ini.ReadInteger('Theme', 'BrightBackColor', DefBrightBackColor);

  DarkForeColor            := Ini.ReadInteger('Theme', 'DarkFontColor', DefDarkForeColor);
  DarkBackColor            := Ini.ReadInteger('Theme', 'DarkBackColor', DefDarkBackColor);

  WindowFontName           := Ini.ReadString ('Theme', 'WindowFontName', '');
  TreeBarFontName          := Ini.ReadString ('Theme', 'TreeBarFontName', '');
  NoteBarFontName          := Ini.ReadString ('Theme', 'NoteBarFontName', '');
  InfoBarFontName          := Ini.ReadString ('Theme', 'InfoBarFontName', '');

  WindowFontSize           := Ini.ReadInteger('Theme', 'WindowFontSize', 0);
  TreeBarFontSize          := Ini.ReadInteger('Theme', 'TreeBarFontSize', DefFontSize);
  NoteBarFontSize          := Ini.ReadInteger('Theme', 'NoteBarFontSize', DefFontSize);
  InfoBarFontSize          := Ini.ReadInteger('Theme', 'InfoBarFontSize', DefFontSize);

  ExpandSignSize           := Ini.ReadInteger('Theme', 'ExpandSignSize', DefExpandSignSize);

  { Options }

  KeepOptionsFormRect      := Ini.ReadBool   ('Options', 'KeepOptionsFormRect', True);
  OptionsFormRect.Left     := Ini.ReadInteger('Options', 'Left', (Screen.Width - 640) div 2);
  OptionsFormRect.Top      := Ini.ReadInteger('Options', 'Top', (Screen.Height - 480) div 2);
  OptionsFormRect.Width    := Ini.ReadInteger('Options', 'Width', 640);
  OptionsFormRect.Height   := Ini.ReadInteger('Options', 'Height', 480);

  KeepSearchFormRect       := Ini.ReadBool   ('Options', 'KeepSearchFormRect', True);
  KeepImportFormRect       := Ini.ReadBool   ('Options', 'KeepImportFormRect', True);
  KeepExportFormRect       := Ini.ReadBool   ('Options', 'KeepExportFormRect', True);
  KeepUtilsFormRect        := Ini.ReadBool   ('Options', 'KeepUtilsFormRect', False);

  SwapOKCancel             := Ini.ReadBool   ('Options', 'SwapOKCancel', False);

  SearchCountLimit         := Ini.ReadInteger('Options', 'SearchCountLimit', DefSearchCountLimit);
  RecentCountLimit         := Ini.ReadInteger('Options', 'RecentCountLimit', DefRecentCountLimit);

  AutoSaveInterval         := Ini.ReadInteger('Options', 'AutoSaveInterval', 0);
  AutoBackupInterval       := Ini.ReadInteger('Options', 'AutoBackupInterval', 60);
  AutoBackupCount          := Ini.ReadInteger('Options', 'AutoBackupCount', 5);

  HistoryMaxSize           := Ini.ReadInteger('Options', 'HistoryMaxSize', 512);
  HistoryMinCount          := Ini.ReadInteger('Options', 'HistoryMinCount', 10);

  { Search }

  SearchFormRect.Left      := Ini.ReadInteger('Search', 'Left', (Screen.Width - 520) div 2);
  SearchFormRect.Top       := Ini.ReadInteger('Search', 'Top', (Screen.Height - 280) div 2);
  SearchFormRect.Width     := Ini.ReadInteger('Search', 'Width', 520);
  SearchFormRect.Height    := Ini.ReadInteger('Search', 'Height', 280);

  SearchFrom               := Ini.ReadInteger('Search', 'SearchFrom', 0);
  DoReplace                := Ini.ReadBool   ('Search', 'DoReplace', False);
  SearchInName             := Ini.ReadBool   ('Search', 'SearchInName', False);
  SearchInNote             := Ini.ReadBool   ('Search', 'SearchInNote', True);
  SearchIgnoreCase         := Ini.ReadBool   ('Search', 'IgnoreCase', True);
  UseNameFilter            := Ini.ReadBool   ('Search', 'UseNameFilter', False);
  NameFilter               := Ini.ReadString ('Search', 'NameFilter', '');
  SearchUseRegExpr         := Ini.ReadBool   ('Search', 'UseRegExpr', False);
  SearchMultiLineMode      := Ini.ReadBool   ('Search', 'MultiLineMode', True);
  SearchNonGreedyMode      := Ini.ReadBool   ('Search', 'NonGreedyMode', False);

  SearchText               := Ini.ReadString ('Search', 'SearchText', '');
  ReplaceText              := Ini.ReadString ('Search', 'ReplaceText', '');

  RecentSearch.Clear;
  for i := 1 to RecentCountLimit do begin
    Recent := Ini.ReadString ('Search', 'RecentSearch' + IntToStr(i), '');
    if Recent <> '' then RecentSearch.Add(Recent);
  end;

  RecentReplace.Clear;
  for i := 1 to RecentCountLimit do begin
    Recent := Ini.ReadString ('Search', 'RecentReplace' + IntToStr(i), '');
    if Recent <> '' then RecentReplace.Add(Recent);
  end;

  { Import }

  ImportFormRect.Left      := Ini.ReadInteger('Import', 'Left', (Screen.Width - 520) div 2);
  ImportFormRect.Top       := Ini.ReadInteger('Import', 'Top', (Screen.Height - 220) div 2);
  ImportFormRect.Width     := Ini.ReadInteger('Import', 'Width', 520);
  ImportFormRect.Height    := Ini.ReadInteger('Import', 'Height', 220);

  ImportFileExt            := Ini.ReadBool   ('Import', 'IncludeFileExt', False);
  ImportRootDir            := Ini.ReadBool   ('Import', 'IncludeRootDir', False);
  ImportFrom               := Ini.ReadInteger('Import', 'ImportFrom', 1);
  ImportTo                 := Ini.ReadInteger('Import', 'ImportTo', 2);
  LastImportDir            := Ini.ReadString ('Import', 'LastDir', '');

  { Export }

  ExportFormRect.Left      := Ini.ReadInteger('Export', 'Left', (Screen.Width - 520) div 2);
  ExportFormRect.Top       := Ini.ReadInteger('Export', 'Top', (Screen.Height - 220) div 2);
  ExportFormRect.Width     := Ini.ReadInteger('Export', 'Width', 520);
  ExportFormRect.Height    := Ini.ReadInteger('Export', 'Height', 220);

  ExportFrom               := Ini.ReadInteger('Export', 'ExportFrom', 1);
  ExportTo                 := Ini.ReadInteger('Export', 'ExportTo', 1);
  ExportAddFileExt         := Ini.ReadBool   ('Export', 'AddFileExt', True);
  ExportAddSeparator       := Ini.ReadBool   ('Export', 'AddSeparator', True);
  ExportFileExt            := Ini.ReadString ('Export', 'FileExt', '.txt');
  ExportSeparator          := Ini.ReadString ('Export', 'Separator', '==========');
  LastExportDir            := Ini.ReadString ('Export', 'LastDir', '');

  { Utils }

  UtilsFormRect.Left       := Ini.ReadInteger('Utils', 'Left', (Screen.Width - 420) div 2);
  UtilsFormRect.Top        := Ini.ReadInteger('Utils', 'Top', (Screen.Height - 360) div 2);
  UtilsFormRect.Width      := Ini.ReadInteger('Utils', 'Width', 420);
  UtilsFormRect.Height     := Ini.ReadInteger('Utils', 'Height', 360);

  UtilsTabIndex            := Ini.ReadInteger('Utils', 'TabIndex', 0);

  { Utils - Sort }

  SortDirection            := Ini.ReadInteger('Utils', 'SortDirection', 0);
  SortOf                   := Ini.ReadInteger('Utils', 'SortOf', 0);

  { Utils - Split }

  SplitSeparator           := Ini.ReadString ('Utils', 'SplitSeparator', '');
  SplitTitle               := Ini.ReadString ('Utils', 'SplitTitle', '');

  RecentSeparator.Clear;
  for i := 1 to RecentCountLimit do begin
    Recent := Ini.ReadString ('Utils', 'RecentSeparator' + IntToStr(i), '');
    if Recent <> '' then RecentSeparator.Add(Recent);
  end;

  RecentTitle.Clear;
  for i := 1 to RecentCountLimit do begin
    Recent := Ini.ReadString ('Utils', 'RecentTitle' + IntToStr(i), '');
    if Recent <> '' then RecentTitle.Add(Recent);
  end;

  IgnoreCaseSp             := Ini.ReadBool   ('Utils', 'IgnoreCaseSp', True);
  MultiLineSp              := Ini.ReadBool   ('Utils', 'MultiLineSp', True);
  NonGreedySp              := Ini.ReadBool   ('Utils', 'NonGreedySp', False);

  IncludeSeparator         := Ini.ReadBool   ('Utils', 'IncludeSeparator', False);

  AddPreNumSp              := Ini.ReadBool   ('Utils', 'AddPreNumSp', False);
  PreNumLenSp              := Ini.ReadInteger('Utils', 'PreNumLenSp', 3);

  { Utils - Rename }

  SearchTextRn             := Ini.ReadString ('Utils', 'SearchTextRn', '');
  ReplaceTextRn            := Ini.ReadString ('Utils', 'ReplaceTextRn', '');

  RecentSearchRn.Clear;
  for i := 1 to RecentCountLimit do begin
    Recent := Ini.ReadString ('Utils', 'RecentSearchRn' + IntToStr(i), '');
    if Recent <> '' then RecentSearchRn.Add(Recent);
  end;

  RecentReplaceRn.Clear;
  for i := 1 to RecentCountLimit do begin
    Recent := Ini.ReadString ('Utils', 'RecentReplaceRn' + IntToStr(i), '');
    if Recent <> '' then RecentReplaceRn.Add(Recent);
  end;

  IgnoreCaseRn             := Ini.ReadBool   ('Utils', 'IgnoreCaseRn', True);
  MultiLineRn              := Ini.ReadBool   ('Utils', 'MultiLineRn', True);
  NonGreedyRn              := Ini.ReadBool   ('Utils', 'NonGreedyRn', False);

  SearchIn                 := Ini.ReadInteger('Utils', 'SearchIn', 1);

  SpecifyDepthRn           := Ini.ReadBool   ('Utils', 'SpecifyDepthRn', False);
  DepthRn                  := Ini.ReadInteger('Utils', 'DepthRn', 1);

  AddPreNumRn              := Ini.ReadBool   ('Utils', 'AddPreNumRn', False);
  PreNumLenRn              := Ini.ReadInteger('Utils', 'PreNumLenRn', 3);

  Ini.Free;
end;

procedure TConfig.Save;
var
  Ini                      : TIniFile;
  i                        : Integer;
  Recent                   : string;
begin
  Ini := TIniFile.Create(FConfigFile);

  { AppInit }

  Ini.WriteBool   ('AppInit', 'KeepMainFormRect', KeepMainFormRect);

  Ini.WriteInteger('AppInit', 'Left'  , MainFormRect.Left);
  Ini.WriteInteger('AppInit', 'Top'   , MainFormRect.Top);
  Ini.WriteInteger('AppInit', 'Width' , MainFormRect.Width);
  Ini.WriteInteger('AppInit', 'Height', MainFormRect.Height);

  Ini.WriteBool   ('AppInit', 'Maximized', Maximized );
  Ini.WriteBool   ('AppInit', 'FullScreen', FullScreen);

  Ini.WriteBool   ('AppInit', 'MenuBarVisible', MenuBarVisible);
  Ini.WriteBool   ('AppInit', 'ToolBarVisible', ToolBarVisible);
  Ini.WriteBool   ('AppInit', 'StatBarVisible', StatBarVisible);
  Ini.WriteBool   ('AppInit', 'TreeBarVisible', TreeBarVisible);
  Ini.WriteBool   ('AppInit', 'InfoBarVisible', InfoBarVisible);
  Ini.WriteBool   ('AppInit', 'RecyBarVisible', RecyBarVisible);

  Ini.WriteInteger('AppInit', 'TreeBarWidth', TreeBarWidth );
  Ini.WriteInteger('AppInit', 'InfoBarHeight', InfoBarHeight);
  Ini.WriteInteger('AppInit', 'RecyBarHeight', RecyBarHeight);

  Ini.WriteBool   ('AppInit', 'WordWrap', WordWrap);

  for i := 1 to 100 do
  begin
    Recent := 'RecentFile' + IntToStr(i);
    if RecentFiles.Count >= i then
      Ini.WriteString('AppInit', Recent, '"' + RecentFiles[i - 1] + '"')
    else
      Ini.DeleteKey('AppInit', Recent);
  end;

  Ini.WriteBool   ('AppInit', 'LoadLastFile', LoadLastFile);
  Ini.WriteString ('AppInit', 'LastFile', '"' + LastFile + '"');

  Ini.WriteBool   ('AppInit', 'SelectLastNode', SelectLastNode);

  { Layout }

  Ini.WriteBool   ('Layout', 'TreeBarAutoSize', TreeBarAutoSize);
  Ini.WriteBool   ('Layout', 'InfoBarAutoSize', InfoBarAutoSize);
  Ini.WriteBool   ('Layout', 'RecyBarAutoSize', RecyBarAutoSize);

  Ini.WriteInteger('Layout', 'TreeBarPercent', TreeBarPercent);
  Ini.WriteInteger('Layout', 'InfoBarPercent', InfoBarPercent);
  Ini.WriteInteger('Layout', 'RecyBarPercent', RecyBarPercent);

  Ini.WriteBool   ('Layout', 'FullWindowHideMenuBar', FullWindowHideMenuBar);
  Ini.WriteBool   ('Layout', 'FullWindowHideToolBar', FullWindowHideToolBar);
  Ini.WriteBool   ('Layout', 'FullWindowHideStatBar', FullWindowHideStatBar);
  Ini.WriteBool   ('Layout', 'FullWindowHideTreeBar', FullWindowHideTreeBar);
  Ini.WriteBool   ('Layout', 'FullWindowHideInfoBar', FullWindowHideInfoBar);
  Ini.WriteBool   ('Layout', 'FullWindowHideRecyBar', FullWindowHideRecyBar);

  Ini.WriteBool   ('Layout', 'RemoveMenuBarItem', RemoveMenuBarItem);

  { Theme }

  Ini.WriteInteger('Theme', 'ActiveTheme', ActiveTheme);

  Ini.WriteInteger('Theme', 'BrightFontColor', BrightForeColor);
  Ini.WriteInteger('Theme', 'BrightBackColor', BrightBackColor);

  Ini.WriteInteger('Theme', 'DarkFontColor', DarkForeColor);
  Ini.WriteInteger('Theme', 'DarkBackColor', DarkBackColor);

  Ini.WriteString ('Theme', 'WindowFontName', '"' + WindowFontName + '"');
  Ini.WriteString ('Theme', 'TreeBarFontName', '"' + TreeBarFontName + '"');
  Ini.WriteString ('Theme', 'NoteBarFontName', '"' + NoteBarFontName + '"');
  Ini.WriteString ('Theme', 'InfoBarFontName', '"' + InfoBarFontName + '"');

  Ini.WriteInteger('Theme', 'WindowFontSize', WindowFontSize);
  Ini.WriteInteger('Theme', 'TreeBarFontSize', TreeBarFontSize);
  Ini.WriteInteger('Theme', 'NoteBarFontSize', NoteBarFontSize);
  Ini.WriteInteger('Theme', 'InfoBarFontSize', InfoBarFontSize);

  Ini.WriteInteger('Theme', 'ExpandSignSize', ExpandSignSize);

  { Options }

  Ini.WriteBool   ('Options', 'KeepOptionsFormRect', KeepOptionsFormRect);
  Ini.WriteInteger('Options', 'Left', OptionsFormRect.Left);
  Ini.WriteInteger('Options', 'Top', OptionsFormRect.Top);
  Ini.WriteInteger('Options', 'Width', OptionsFormRect.Width);
  Ini.WriteInteger('Options', 'Height', OptionsFormRect.Height);

  Ini.WriteBool   ('Options', 'KeepSearchFormRect', KeepSearchFormRect);
  Ini.WriteBool   ('Options', 'KeepImportFormRect', KeepImportFormRect);
  Ini.WriteBool   ('Options', 'KeepExportFormRect', KeepExportFormRect);

  Ini.WriteBool   ('Options', 'SwapOKCancel', SwapOKCancel);

  Ini.WriteInteger('Options', 'SearchCountLimit', SearchCountLimit);
  Ini.WriteInteger('Options', 'RecentCountLimit', RecentCountLimit);

  Ini.WriteInteger('Options', 'AutoSaveInterval', AutoSaveInterval);
  Ini.WriteInteger('Options', 'AutoBackupInterval', AutoBackupInterval);
  Ini.WriteInteger('Options', 'AutoBackupCount', AutoBackupCount);

  Ini.WriteInteger('Options', 'HistoryMaxSize', HistoryMaxSize);
  Ini.WriteInteger('Options', 'HistoryMinCount', HistoryMinCount);

  { Search }

  Ini.WriteInteger('Search', 'Left', SearchFormRect.Left);
  Ini.WriteInteger('Search', 'Top', SearchFormRect.Top);
  Ini.WriteInteger('Search', 'Width', SearchFormRect.Width);
  Ini.WriteInteger('Search', 'Height', SearchFormRect.Height);

  Ini.WriteInteger('Search', 'SearchFrom', SearchFrom);
  Ini.WriteBool   ('Search', 'DoReplace', DoReplace);
  Ini.WriteBool   ('Search', 'SearchInName', SearchInName);
  Ini.WriteBool   ('Search', 'SearchInNote', SearchInNote);
  Ini.WriteBool   ('Search', 'IgnoreCase', SearchIgnoreCase);
  Ini.WriteBool   ('Search', 'UseNameFilter', UseNameFilter);
  Ini.WriteString ('Search', 'NameFilter', '"' + NameFilter + '"');
  Ini.WriteBool   ('Search', 'UseRegExpr', SearchUseRegExpr);
  Ini.WriteBool   ('Search', 'MultiLineMode', SearchMultiLineMode);
  Ini.WriteBool   ('Search', 'NonGreedyMode', SearchNonGreedyMode);

  Ini.WriteString ('Search', 'SearchText', '"' + SearchText + '"');
  Ini.WriteString ('Search', 'ReplaceText', '"' + ReplaceText + '"');

  for i := 1 to 100 do
  begin
    Recent := 'RecentSearch' + IntToStr(i);
    if RecentSearch.Count >= i then
      Ini.WriteString('Search', Recent, '"' + RecentSearch[i - 1] + '"')
    else
      Ini.DeleteKey('Search', Recent);
  end;

  for i := 1 to 100 do
  begin
    Recent := 'RecentReplace' + IntToStr(i);
    if RecentReplace.Count >= i then
      Ini.WriteString('Search', Recent, '"' + RecentReplace[i - 1] + '"')
    else
      Ini.DeleteKey('Search', Recent);
  end;

  { Import }

  Ini.WriteInteger('Import', 'Left', ImportFormRect.Left);
  Ini.WriteInteger('Import', 'Top', ImportFormRect.Top);
  Ini.WriteInteger('Import', 'Width', ImportFormRect.Width);
  Ini.WriteInteger('Import', 'Height', ImportFormRect.Height);

  Ini.WriteBool   ('Import', 'IncludeFileExt', ImportFileExt);
  Ini.WriteBool   ('Import', 'IncludeRootDir', ImportRootDir);
  Ini.WriteInteger('Import', 'ImportFrom', ImportFrom);
  Ini.WriteInteger('Import', 'ImportMode', ImportTo);
  Ini.WriteString ('Import', 'LastDir', '"' + LastImportDir + '"');

  { Export }

  Ini.WriteInteger('Export', 'Left', ExportFormRect.Left);
  Ini.WriteInteger('Export', 'Top', ExportFormRect.Top);
  Ini.WriteInteger('Export', 'Width', ExportFormRect.Width);
  Ini.WriteInteger('Export', 'Height', ExportFormRect.Height);

  Ini.WriteBool   ('Export', 'AddFileExt', ExportAddFileExt);
  Ini.WriteBool   ('Export', 'AddSeparator', ExportAddSeparator);
  Ini.WriteString ('Export', 'FileExt', '"' + ExportFileExt + '"');
  Ini.WriteString ('Export', 'Separator', '"' + ExportSeparator + '"');
  Ini.WriteInteger('Export', 'ExportTo', ExportTo);
  Ini.WriteInteger('Export', 'ExportMode', ExportFrom);
  Ini.WriteString ('Export', 'LastDir', '"' + LastExportDir + '"');

  { Utils }

  Ini.WriteInteger('Utils', 'Left', UtilsFormRect.Left);
  Ini.WriteInteger('Utils', 'Top', UtilsFormRect.Top);
  Ini.WriteInteger('Utils', 'Width', UtilsFormRect.Width);
  Ini.WriteInteger('Utils', 'Height', UtilsFormRect.Height);

  Ini.WriteInteger('Utils', 'TabIndex', UtilsTabIndex);

  { Utils - Sort }

  Ini.WriteInteger('Utils', 'SortDirection', SortDirection);
  Ini.WriteInteger('Utils', 'SortOf', SortOf);

  { Utils - Split }

  Ini.WriteString ('Utils', 'SplitSeparator', '"' + SplitSeparator + '"');
  Ini.WriteString ('Utils', 'SplitTitle', '"' + SplitTitle + '"');

  for i := 1 to 100 do
  begin
    Recent := 'RecentSeparator' + IntToStr(i);
    if RecentSeparator.Count >= i then
      Ini.WriteString('Utils', Recent, '"' + RecentSeparator[i - 1] + '"')
    else
      Ini.DeleteKey('Utils', Recent);
  end;

  for i := 1 to 100 do
  begin
    Recent := 'RecentTitle' + IntToStr(i);
    if RecentTitle.Count >= i then
      Ini.WriteString('Search', Recent, '"' + RecentTitle[i - 1] + '"')
    else
      Ini.DeleteKey('Search', Recent);
  end;

  Ini.WriteBool   ('Utils', 'IgnoreCaseSp', IgnoreCaseSp);
  Ini.WriteBool   ('Utils', 'MultiLineSp', MultiLineSp);
  Ini.WriteBool   ('Utils', 'NonGreedySp', NonGreedySp);

  Ini.WriteBool   ('Utils', 'IncludeSeparator', IncludeSeparator);

  Ini.WriteBool   ('Utils', 'AddPreNumSp', AddPreNumSp);
  Ini.WriteInteger('Utils', 'PreNumLenSp', PreNumLenSp);

  { Utils - Rename }

  Ini.WriteString ('Utils', 'SearchTextRn', '"' + SearchTextRn + '"');
  Ini.WriteString ('Utils', 'ReplaceTextRn', '"' + ReplaceTextRn + '"');

  for i := 1 to 100 do
  begin
    Recent := 'RecentSearchRn' + IntToStr(i);
    if RecentSearchRn.Count >= i then
      Ini.WriteString('Utils', Recent, '"' + RecentSearchRn[i - 1] + '"')
    else
      Ini.DeleteKey('Utils', Recent);
  end;

  for i := 1 to 100 do
  begin
    Recent := 'RecentReplaceRn' + IntToStr(i);
    if RecentReplaceRn.Count >= i then
      Ini.WriteString('Search', Recent, '"' + RecentReplaceRn[i - 1] + '"')
    else
      Ini.DeleteKey('Search', Recent);
  end;

  Ini.WriteBool   ('Utils', 'IgnoreCaseRn', IgnoreCaseRn);
  Ini.WriteBool   ('Utils', 'MultiLineRn', MultiLineRn);
  Ini.WriteBool   ('Utils', 'NonGreedyRn', NonGreedyRn);

  Ini.WriteInteger('Utils', 'SearchIn', SearchIn);

  Ini.WriteBool   ('Utils', 'SpecifyDepthRn', SpecifyDepthRn);
  Ini.WriteInteger('Utils', 'DepthRn', DepthRn);

  Ini.WriteBool   ('Utils', 'AddPreNumRn', AddPreNumRn);
  Ini.WriteInteger('Utils', 'PreNumLenRn', PreNumLenRn);

  Ini.Free;
end;

end.

