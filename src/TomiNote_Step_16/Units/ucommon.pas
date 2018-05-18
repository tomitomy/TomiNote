unit uCommon;

{$mode objfpc}{$H+}

// String constants and string literals require this option
// 字符串常量和字符串字面量需要此选项
{$codepage UTF8}

interface

uses
  Classes, SysUtils;

const
  DefGrowSize = 8  * 1024;
  MaxGrowSize = 32 * 1024;

type

  { TStringBuffer }

  TStringBuffer = class

  private

    FDataString              : string;
    FCap                     : LongInt;  // MaxSize 2GB | 最大容量 2GB
    FSize                    : LongInt;  // MaxSize 2GB | 最大容量 2GB
    FPosition                : LongInt;  // MaxSize 2GB | 最大容量 2GB

    procedure Grow(NeedSize: LongInt);
    function  GetDataString: string;

  public

    constructor Create;
    constructor Create(S: string);
    constructor Create(ASize: LongInt);
    destructor  Destroy; override;

    procedure Reset;
    function  Seek(Offset: Longint; Origin: Integer): LongInt;

    function  Read(var Buffer; Count: LongInt): LongInt;
    function  ReadString(Count: LongInt): string;

    procedure Write(const Buffer; Count: LongInt);
    procedure WriteString(const S: string); inline;

  published

    property  DataString : string  read GetDataString;
    property  Cap        : LongInt read FCap;
    property  Size       : LongInt read FSize;
    property  Position   : LongInt read FPosition;

  end;

// Check if a key is pressed | 检查某个按键是否按下
function IsKeyDown(const Key: Integer): Boolean; inline;

// Change the \r\n or \r in S into \n
// 将 S 中的 \r\n 或 \r 转换为 \n
function ToLF(const S: string): string;

// Change the \n in S into \r\n
// 将 S 中的 \n 转换为 \r\n
function ToCRLF(const S: string): string;

// Read the contents of the text file and unifying the line break to #10
// 读取文本文件内容，同时将换行符统一为 #10
function ReadFile(FileName: string): string;

// Write the string to a file | 将字符串写入文件
procedure WriteFile(FileName: string; Content: string);

// Convert illegal characters in file name to '_'
// 将文件名中的非法字符转换为 '_'
function FixFileName(FileName: string): string;

// Get a filename which is different from an existing filename
// 获取一个与现有文件不重名的文件名
function GetNonExistsPath(Dir, Name, Ext: string; SuffixLen: integer): string;

// Search all files and subdirectories in Path, the result is stored in Rst.
// 搜索 Path 中的所有文件和子目录，结果存入 Rst 返回
procedure FindFiles(Path: string; var Rst: TStringList);

// Escape and unescape | 转义和取消转义
function Escape(S: string): string;
function UnEscape(S: string): string;
function RegExprUnEscape(S: string): string;

function UTF8PosToBytePos(const Text: PChar; const Size: SizeInt; UPos: SizeInt): SizeInt;
function UTF8PosToBytePos(const Text: String; const UPos: SizeInt): SizeInt; inline;

// Get the sub string of Text, the Index is based 1.
// StartUPos: The character index of sub string.
// CharCount: The character count of sub string.
// 获取 Text 的子串，索引从 1 开始
// StartUPos：子串起始索引，字符索引
// CharCount：子串字符长度
function UTF8Copy(const Text: string; StartUPos, CharCount: SizeInt): string;

// Remove the leading and trailing blank characters of AStr (#0..#32, #127)
// 删除字符串 AStr 首尾的空白字符 (#0..#32, #127)
function TrimBlank(const Text: string): string;

// Remove the leading and trailing blank lines of AStr (#10)
// 删除字符串 AStr 首尾的空行 (#10)
function TrimBlankLine(const Text: string): string;

implementation

uses
  LCLIntf;

{ TStringBuffer }

constructor TStringBuffer.Create;
begin
  inherited Create;

  FDataString := '';
  FCap        := 0;
  FSize       := 0;
  FPosition   := 0;
end;

constructor TStringBuffer.Create(S: string);
begin
  inherited Create;

  FDataString := S;
  FCap        := Length(S);
  FSize       := FCap;
  FPosition   := FCap;
end;

constructor TStringBuffer.Create(ASize: LongInt);
begin
  inherited Create;

  if ASize <= 0 then
    ASize := DefGrowSize;

  SetLength(FDataString, ASize);
  FCap        := ASize;
  FSize       := ASize;
  FPosition   := 0;
end;

destructor TStringBuffer.Destroy;
begin
  SetLength(FDataString, 0);
  inherited Destroy;
end;

procedure TStringBuffer.Grow(NeedSize: LongInt);
begin
  if NeedSize < DefGrowSize then
    NeedSize := DefGrowSize
  else if NeedSize < MaxGrowSize then
    NeedSize := MaxGrowSize
  else
    NeedSize := NeedSize + DefGrowSize;

  SetLength(FDataString, FCap + NeedSize);
  FCap := Length(FDataString);
end;

function TStringBuffer.GetDataString: string;
begin
  SetLength(FDataString, FSize);
  FCap   := FSize;
  Result := FDataString;
end;

procedure TStringBuffer.Reset;
begin
  if FCap > DefGrowSize then begin
    FCap := DefGrowSize;
    SetLength(FDataString, FCap);
  end;
  FSize     := 0;
  FPosition := 0;
end;

function TStringBuffer.Seek(Offset: Longint; Origin: Integer): LongInt;
begin
  Case Origin of
     1 : FPosition := Offset;
     0 : FPosition := FPosition + Offset;
    -1 : FPosition := FSize + Offset;
  end;

  if FPosition > FSize then
    FPosition := FSize
  else if FPosition < 0 then
    FPosition := 0;

  Result := FPosition;
end;

function TStringBuffer.Read(var Buffer; Count: LongInt): LongInt;
begin
  Result := FSize - FPosition;

  if Result > Count then
    Result := Count;

  if Result > 0 then begin
    Move(PChar(FDataString)[FPosition], Buffer, Result);
    FPosition := FPosition + Result;
  end;
end;

function TStringBuffer.ReadString(Count: LongInt): string;
var
  MaxCount: LongInt;
begin
  MaxCount := FSize - FPosition;
  if Count > MaxCount then
    Count := MaxCount;

  SetLength(Result, Count);
  Read(Pointer(Result)^, Count);
end;

procedure TStringBuffer.Write(const Buffer; Count: LongInt);
begin
  if FPosition + Count > FCap then
    Grow(Count);

  Move(Buffer, PChar(FDataString)[FPosition], Count);

  FPosition := FPosition + Count;
  FSize     := FPosition;
end;

procedure TStringBuffer.WriteString(const S: string); inline;
begin
  Write(PChar(S)[0], Length(S));
end;

// Check if a key is pressed | 检查某个按键是否按下
function IsKeyDown(const Key: Integer): Boolean; inline;
begin
  Result := GetKeyState(Key) < 0;
end;

// Change the \r\n or \r in S into \n
// 将 S 中的 \r\n 或 \r 转换为 \n
function ToLF(const S: string): string;
var
  PrevPos, CurPos, EndPos, ResultPos: PChar;
  Len: SizeInt;
begin
  Result := '';
  Len := Length(S);
  if Len = 0 then Exit;
  SetLength(Result, Len);

  PrevPos   := PChar(S);
  CurPos    := PrevPos;
  EndPos    := CurPos + Len;
  ResultPos := PChar(Result);

  while CurPos < EndPos do begin
    if CurPos^ = #13 then begin
      // Write the contents between two #13 to Result
      // 将两个 #13 之间的内容写入 Result 中
      Len := CurPos - PrevPos;
      move(PrevPos^, ResultPos^, Len);
      Inc(ResultPos, Len);
      Inc(CurPos);
      PrevPos := CurPos; // Skip #13 | 跳过 #13

      // For #13#10, ignore #13 directly
      // 对于 #13#10，直接忽略掉 #13 即可
      if CurPos^ = #10 then begin
        Inc(CurPos);
        continue;
      end;

      // For single #13, convert it to #10 then write it to Result
      // 对于单独的 #13，将其转换为 #10 写入 Result 中
      ResultPos^ := #10;
      Inc(ResultPos);
    end else
      Inc(CurPos);
  end;

  if PrevPos < EndPos then begin
    // Writes the content after the last #13 into Result
    // 将最后一个 #13 之后的内容写入 Result
    Len := EndPos - PrevPos;
    move(PrevPos^, ResultPos^, Len);
    Inc(ResultPos, Len);
  end;

  // Remove excess buffer space | 去掉多余的缓冲区空间
  SetLength(Result, ResultPos - PChar(Result));
end;

// Change the \n in S into \r\n
// 将 S 中的 \n 转换为 \r\n
function ToCRLF(const S: string): string;
var
  ResultCurPos, ResultEndPos: PChar;

  procedure Grow(GrowSize: LongInt);
  var
    OldSize: SizeInt;
  begin
    Inc(GrowSize, 512);  // Give more 512 bytes | 多给出 512 字节
    OldSize := ResultCurPos - PChar(Result);
    SetLength(Result, Length(Result) + GrowSize);
    ResultCurPos := PChar(Result) + OldSize;
    ResultEndPos := PChar(Result) + Length(Result);
  end;

var
  PrevPos, CurPos, EndPos: PChar;
  Len: SizeInt;
begin
  Result := '';
  Len := Length(S);
  if Len = 0 then Exit;

  PrevPos   := PChar(S);
  CurPos    := PrevPos;
  EndPos    := CurPos + Len;

  // Assume that every 128 bytes will encounter a line break
  // 假设每128个字节会遇到一个换行符
  if Len <= 128 * 512 then // <= 64 KB
    SetLength(Result, Len + 512)
  else
    SetLength(Result, Len + Len div 128);

  ResultCurPos := PChar(Result);
  ResultEndPos := PChar(Result) + Length(Result);

  while CurPos < EndPos do begin
    if CurPos^ = #10 then begin
      Len := CurPos - PrevPos;
      // Ignore #13#10 | 忽略 #13#10
      if (Len = 0) or ((CurPos-1)^ <> #13) then begin
        // Make sure Result has enough capacity
        // 确保 Result 有足够的容量
        if ResultEndPos - ResultCurPos <= Len + 2 then
          Grow(Len + 2);
        // Write the contents between two #10 to Result
        // 将两个 #10 之间的内容写入 Result 中
        move((PrevPos)^, ResultCurPos^, Len);
        Inc(ResultCurPos, Len);
        // Convert #10 to #13#10 | 将 #10 转换为 #13#10
        ResultCurPos^ := #13;
        Inc(ResultCurPos);
        ResultCurPos^ := #10;
        Inc(ResultCurPos);
        PrevPos := CurPos + 1; // Skip #10 in S | 跳过 S 中的 #10
      end;
    end;
    Inc(CurPos);
  end;

  if PrevPos < EndPos then begin
    // Writes the content after the last #10 into Result
    // 将最后一个 #10 之后的内容写入 Result
    Len := EndPos - PrevPos;
    move(PrevPos^, ResultCurPos^, Len);
    Inc(ResultCurPos, Len);
  end;

  // Remove excess buffer space | 去掉多余的缓冲区空间
  SetLength(Result, ResultCurPos - PChar(Result));
end;

// Read the contents of the text file and unifying the line break to #10
// 读取文本文件内容，同时将换行符统一为 #10
function ReadFile(FileName: string): string;
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(FileName, fmOpenRead);
  try
    // Ignore UTF8-BOM | 忽略 UTF8-BOM
    if (AStream.Size >= 3) and
    (AStream.ReadByte = $EF) and
    (AStream.ReadByte = $BB) and
    (AStream.ReadByte = $BF) then begin
      SetLength(Result, AStream.Size - 3);
      AStream.ReadBuffer(Result[1], AStream.Size - 3);
    end else begin
      AStream.Seek(0, SoFromBeginning);
      SetLength(Result, AStream.Size);
      AStream.ReadBuffer(Result[1], AStream.Size);
    end;
    Result := ToLF(Result);
  finally
    AStream.Free;
  end;
end;

// Write the string to a file | 将字符串写入文件
procedure WriteFile(FileName: string; Content: string);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(FileName, fmCreate);
  try
    if LineEnding = #13#10 then
      Content := ToCRLF(Content);
    AStream.WriteBuffer(Content[1], Length(Content));
  finally
    AStream.Free;
  end;
end;

// Convert illegal characters in file name to '_'
// 将文件名中的非法字符转换为 '_'
function FixFileName(FileName: string): string;
var
  i: integer;
begin
  for i := Low(FileName) to High(FileName) do
    if FileName[i] in ['/', '\', ':', '<', '>', '*', '?', '|', '"', #9, #10, #13] then
      FileName[i] := '_';
  Result := FileName;
end;

// Get a filename which is different from an existing filename
// 获取一个与现有文件不重名的文件名
function GetNonExistsPath(Dir, Name, Ext: string; SuffixLen: integer): string;
var
  i: integer;
  Suffix: string;
begin
  i := 1;
  Result := ConcatPaths([Dir, Name + Ext]);
  while FileExists(Result) or DirectoryExists(Result) do begin
    Suffix := IntToStr(i);
    while Length(Suffix) < SuffixLen do Suffix := '0' + Suffix;
    Result := ConcatPaths([Dir, Name + ' (' + Suffix + ')' + Ext]);
    Inc(i);
  end;
end;

// Search all files and subdirectories in Path, the result is stored in Rst.
// 搜索 Path 中的所有文件和子目录，结果存入 Rst 返回
procedure FindFiles(Path: string; var Rst: TStringList);
var
  SearchRec: TRawbyteSearchRec;
begin
  if FindFirst(ConcatPaths([Path, '*']), faAnyFile or faDirectory, SearchRec) = 0 then
    repeat
      if (SearchRec.Name = '.') or (SearchRec.Name = '..') then continue;
      Rst.Add(SearchRec.Name);
    until FindNext(SearchRec) <> 0;
  FindClose(SearchRec);
end;

// Convert the special character in S (#13 #10 #9) to escape character (\r \n \t).
// This function does not process \\ characters, so it can be used for regular expression strings.
// Only special \ character will be processed, for example,'\'#10 will be converted to'\\\n'.
// 将 S 中的特殊字符（#13 #10 #9）转换为转义字符(\r \n \t)
// 这个转换函数不会处理 \ 字符，所以可以用于正则表达式字符串
// 只有特殊的 \ 字符会被处理，比如 '\'#10 会被转换成 '\\\n'
function Escape(S: string): string;
var
  StartPos  : SizeInt;
  i         : SizeInt;
  EscapeStr : string;
  MeetSlash : Boolean;
begin
  StartPos  := 1;
  Result    := '';
  EscapeStr := '';
  MeetSlash := False;

  for i := 1 to Length(S) do begin
    if MeetSlash then begin
      MeetSlash := False;
      case S[i] of
        #13: EscapeStr := '\\r';
        #10: EscapeStr := '\\n';
        #9 : EscapeStr := '\\t';
        else continue; // Does not need escape | 不需要转义
      end;
    end else begin
      case S[i] of
        '\': begin MeetSlash := True; continue; end;
        #13: EscapeStr := '\r';
        #10: EscapeStr := '\n';
        #9 : EscapeStr := '\t';
        else continue; // Does not need escape | 不需要转义
      end;
    end;
    Result := Result + Copy(S, StartPos, i - StartPos) + EscapeStr;
    StartPos := i + 1;
  end;

  if StartPos <= Length(S) then
    Result := Result + Copy(S, StartPos, Length(S) - StartPos + 1);
end;

// Convert the escape characters in S (\r \n \t \\ \xFF) to special characters (#13 #10 #9 \ Char).
// Other single \ character will be retained.
// 将 S 中的转义字符（\r \n \t \\ \xFF）转换为特殊字符（#13 #10 #9 \ Char）
// 其它地方单独的 \ 字符会被保留
function UnEscape(S: string): string;
var
  StartPos  : SizeInt;
  CurrPos   : SizeInt;
  Len       : SizeInt;
  MeetSlash : boolean;
begin
  StartPos  := 1;
  CurrPos   := 1;
  Len       := Length(S);

  Result    := '';
  MeetSlash := False;

  while CurrPos <= Len do begin
    if MeetSlash then begin
      MeetSlash := False;
      case S[CurrPos] of
        '\': Result := Result + '\';
        'r': Result := Result + #13;
        'n': Result := Result + #10;
        't': Result := Result + #9;
        'x': begin
          if (CurrPos <= Len - 2) and
             (S[CurrPos+1] in ['0'..'9', 'A'..'F', 'a'..'z']) and
             (S[CurrPos+2] in ['0'..'9', 'A'..'F', 'a'..'z']) then
          begin
            Result := Result + Chr(StrToInt('$' + Copy(S, CurrPos+1, 2)));
            Inc(CurrPos, 2);
            StartPos := CurrPos + 1;
          end else
            Result := Result + '\' + S[CurrPos];
        end;
        else Result := Result + '\' + S[CurrPos];
      end;
    end else if S[CurrPos] = '\' then begin
      MeetSlash := True;
      Result := Result + Copy(S, StartPos, CurrPos - StartPos);
      StartPos := CurrPos + 2;
    end;
    Inc(CurrPos);
  end;

  if StartPos <= Len then
    Result := Result + Copy(S, StartPos, Length(S) - StartPos + 1);
end;

// Convert the escape characters in S (\r \n \t \xFF) to special characters (#13 #10 #9 Char).
// Other single \ character will be retained.
// It's dedicated to the replacement content of regular expressions,
// Because regular expressions will process \\ by itself.
// 将 S 中的转义字符（\r \n \t \xFF）转换为特殊字符（#13 #10 #9 Char）
// 其它地方单独的 \ 字符会被保留
// 专用于正则表达式的替换内容，因为正则表达式自己会处理 \\
function RegExprUnEscape(S: string): string;
var
  StartPos  : SizeInt;
  CurrPos   : SizeInt;
  Len       : SizeInt;
  MeetSlash : boolean;
begin
  StartPos  := 1;
  CurrPos   := 1;
  Len       := Length(S);

  Result    := '';
  MeetSlash := False;

  while CurrPos <= Len do begin
    if MeetSlash then begin
      MeetSlash := False;
      case S[CurrPos] of
        'r': Result := Result + #13;
        'n': Result := Result + #10;
        't': Result := Result + #9;
        'x': begin
          if (CurrPos <= Len - 2) and
             (S[CurrPos+1] in ['0'..'9', 'A'..'F', 'a'..'z']) and
             (S[CurrPos+2] in ['0'..'9', 'A'..'F', 'a'..'z']) then
          begin
            Result := Result + Chr(StrToInt('$' + Copy(S, CurrPos+1, 2)));
            Inc(CurrPos, 2);
            StartPos := CurrPos + 1;
          end else
            Result := Result + '\' + S[CurrPos];
        end;
        else Result := Result + '\' + S[CurrPos];
      end;
    end else if S[CurrPos] = '\' then begin
      MeetSlash := True;
      Result := Result + Copy(S, StartPos, CurrPos - StartPos);
      StartPos := CurrPos + 2;
    end;
    Inc(CurrPos);
  end;

  if StartPos <= Len then
    Result := Result + Copy(S, StartPos, Length(S) - StartPos + 1);
end;

// Convert the character index of a UTF8 string to a byte index. Returns 0 if
// UPos <= 0, return Size + 1 if UPos > Size. This function does not check the
// integrity of the UTF8 encoding, does not support multi-codepoint character,
// the multi-codepoint character will be treated as multiple characters.

// 将 UTF8 字符串的字符索引转换为字节索引， 如果 UPos <= 0，则返回 0，
// 如果 UPos > Size，则返回 Size + 1。本函数不检查 UTF8 编码的完整性，
// 不支持多码点字符，多码点字符会被当成多个字符处理。

// Text         : UTF8 string | UTF8 字符串
// Size         : the bytes size of UTF8 string | UTF8 字符串的字节长度
// UPos         : index of character, based 1 | 字符索引，从 1 开始
// return value : byte index of UPos, based 1 | UPos 对应的字节索引，从 1 开始
function UTF8PosToBytePos(const Text: PChar; const Size: SizeInt; UPos: SizeInt): SizeInt;
begin
  Result := 0;
  if UPos <= 0 then Exit;

  while (UPos > 1) and (Result < Size) do begin
    case Text[Result] of
      // #0  ..#127: Inc(Pos);
      #192..#223: Inc(Result, 2);
      #224..#239: Inc(Result, 3);
      #240..#247: Inc(Result, 4);
      else Inc(Result);
    end;
    Dec(UPos);
  end;

  Inc(Result);
end;

function UTF8PosToBytePos(const Text: String; const UPos: SizeInt): SizeInt; inline;
begin
  Result := UTF8PosToBytePos(PChar(Text), Length(Text), UPos);
end;

// This function does not check the integrity of the UTF8 encoding,
// multi-codepoint character will be treated as multiple characters.
// 本函数不检查 UTF8 编码的完整性，多码点字符会被当成多个字符处理。
function UTF8Copy(const Text: string; StartUPos, CharCount: SizeInt): string;
var
  StartBPos, Size, CopySize: SizeInt;
begin
  Result := '';
  Size := Length(Text);
  StartBPos := UTF8PosToBytePos(PChar(Text), Size, StartUPos);
  if (StartBPos = 0) or (StartBPos > Size) then Exit;
  CopySize := UTF8PosToBytePos(@Text[StartBPos], Size - StartBPos + 1, CharCount + 1) - 1;
  Result := Copy(Text, StartBPos, CopySize);
end;

// Remove the head and tail space characters of AStr (#0..#32, #127)
// 删除字符串 AStr 首尾的空白字符 (#0..#32, #127)
function TrimBlank(const Text: string): string;
var
  StartBPos, EndBPos: integer;
begin
  StartBPos := 1;
  EndBPos := Length(Text);

  while (StartBPos <= EndBPos) and (Text[StartBPos] in [#0..#32, #127]) do
    Inc(StartBPos);

  while (StartBPos <= EndBPos) and (Text[EndBPos] in [#0..#32, #127]) do
    Dec(EndBPos);

  Result := Copy(Text, StartBPos, EndBPos - StartBPos + 1);
end;

// Remove the leading and trailing blank lines of AStr (#10)
// 删除字符串 AStr 首尾的空行 (#10)
function TrimBlankLine(const Text: string): string;
var
  StartBPos, EndBPos: integer;
begin
  StartBPos := 1;
  EndBPos := Length(Text);

  while (StartBPos <= EndBPos) and (Text[StartBPos] = #10) do
    Inc(StartBPos);

  while (StartBPos <= EndBPos) and (Text[EndBPos] = #10) do
    Dec(EndBPos);

  Result := Copy(Text, StartBPos, EndBPos - StartBPos + 1);
end;

end.

