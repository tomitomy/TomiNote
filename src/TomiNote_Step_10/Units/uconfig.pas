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

    SwapOKCancel             : Boolean;

  private

    FConfigFile              : string;

  public

    constructor Create(ConfigFile: string);
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
end;

procedure TConfig.Load;
var
  Ini                      : TIniFile;
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

  SwapOKCancel             := Ini.ReadBool   ('Options', 'SwapOKCancel', False);

  Ini.Free;
end;

procedure TConfig.Save;
var
  Ini                      : TIniFile;
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

  Ini.Free;
end;

end.

