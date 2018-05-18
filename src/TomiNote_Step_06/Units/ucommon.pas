unit uCommon;

{$mode objfpc}{$H+}

// String constants and string literals require this option
// 字符串常量和字符串字面量需要此选项
{$codepage UTF8}

interface

uses
  Classes, SysUtils;

// Check if a key is pressed | 检查某个按键是否按下
function IsKeyDown(const Key: Integer): Boolean; inline;

implementation

uses
  LCLIntf;

// Check if a key is pressed | 检查某个按键是否按下
function IsKeyDown(const Key: Integer): Boolean; inline;
begin
  Result := GetKeyState(Key) < 0;
end;

end.

