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

// Change the \r\n or \r in S into \n
// 将 S 中的 \r\n 或 \r 转换为 \n
function ToLF(S: string): string;

implementation

uses
  LCLIntf;

// Check if a key is pressed | 检查某个按键是否按下
function IsKeyDown(const Key: Integer): Boolean; inline;
begin
  Result := GetKeyState(Key) < 0;
end;

// Change the \r\n or \r in S into \n
// 将 S 中的 \r\n 或 \r 转换为 \n
function ToLF(S: string): string;
var
  StartPos, CurrPos, ResultPos, Len: integer;
begin
  if S = '' then begin
    Result := '';
    Exit;
  end;

  StartPos  := 1;
  CurrPos   := 1;
  Len       := Length(S);
  ResultPos := 1;
  SetLength(Result, Len);

  while CurrPos < Len do begin
    if S[CurrPos] = #13 then begin
      // Write the contents between two #13 to Result
      // 将两个 #13 之间的内容写入 Result 中
      move(S[StartPos], Result[ResultPos], CurrPos - StartPos);
      Inc(ResultPos, CurrPos - StartPos);
      Inc(CurrPos);
      StartPos := CurrPos;
      // For #13#10, ignore #13 directly
      // 对于 #13#10，直接忽略掉 #13 即可
      if S[CurrPos] = #10 then continue;
      // For single #13, convert it to #10 then write it to Result
      // 对于单独的 #13，将其转换为 #10 写入 Result 中
      Result[ResultPos] := #10;
       Inc(ResultPos);
    end else
      Inc(CurrPos);
  end;
  // Writes the content after the last #13 (not the ending #13) into Result
  // 将最后一个 #13（除了尾部的 #13）之后的内容写入 Result
  move(S[StartPos], Result[ResultPos], Len - StartPos);
  Inc(ResultPos, Len - StartPos);
  // Handle the ending #13 | 处理尾部的 #13
  if S[Len] = #13 then
    Result[ResultPos] := #10
  else
    Result[ResultPos] := S[Len];
  // Remove excess buffer space | 去掉多余的缓冲区空间
  SetLength(Result, ResultPos);
end;

end.

