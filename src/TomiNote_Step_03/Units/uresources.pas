unit uResources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

var

  AppFullName              : string;
  AppName                  : string;
  AppDir                   : string;

const

  AppTitle                 = 'TomiNote';
  AppVersion               = '1.1';

  BrightThemeID            = 1;
  DarkThemeID              = 2;

  DefFontSize              = 12;
  DefExpandSignSize        = 9;

  DefTreeBarPercent        = 25;
  DefRecyBarPercent        = 30;
  DefInfoBarPercent        = 30;

  DefBrightForeColor       = $212121;
  DefBrightBackColor       = $F7F7F7;
  DefDarkForeColor         = $D3D7CF;
  DefDarkBackColor         = $2E3436;


resourcestring

  Res_CaptionOK            = 'OK(&O)';
  Res_CaptionCancel        = 'Cancel(&C)';

implementation

initialization

  AppFullName              := ParamStr(0);
  AppName                  := ExtractFileName(AppFullName);
  AppDir                   := ExtractFilePath(AppFullName);

end.

