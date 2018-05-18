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

implementation

initialization

  AppFullName              := ParamStr(0);
  AppName                  := ExtractFileName(AppFullName);
  AppDir                   := ExtractFilePath(AppFullName);

end.

