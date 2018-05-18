unit uSearch;

{$mode objfpc}{$H+}

// String constants and string literals require this option
// 字符串常量和字符串字面量需要此选项
{$codepage UTF8}

interface

uses
  Classes, SysUtils;

type
  // Used to store search or replace results
  // 用来存储搜索或替换结果的数组
  TSizeIntArray = array of SizeInt;

{ ============================================================ }
{ Base Function 基础函数                                       }
{ ============================================================ }

// Find single byte. For single byte, this function is faster than
// the BoyerMoore algorithm.
// Return Value: Number of matched(Matches contain the byte index of the search result)
// 查找单个字节。对于搜索单个字节，速度比 BoyerMoore 算法快
// 返回值：成功匹配的次数（Matches 中存放的是搜索结果的字节索引）
function FindMatchesChar(const Text: PChar; const SearchChar: Char;
  TextSize: SizeInt; out Matches: TSizeIntArray; MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): SizeInt;

function FindMatchesChar(const Text: String; const SearchChar: Char;
  out Matches: TSizeIntArray; MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): SizeInt; inline;

// Search string (Boyer Moore algorithm), copied from StrUtils, and made a simple modification.
// suitable for long SearchText
// Return Value: Number of matched(Matches contain the byte index of the search result)
// 查找字符串（Boyer Moore 算法），从 StrUtils 中复制而来，进行了简单的修改
// 适合 SearchText 比较长的情况
// 返回值：成功匹配的次数（Matches 中存放的是搜索结果的字节索引）
function FindMatchesBoyerMoore(const Text: PChar; SearchText: PChar;
  TextSize, SearchSize: SizeInt; out Matches: TSizeIntArray; MatchCount: SizeInt = 0;
  IgnoreCase: Boolean = False): SizeInt;

function FindMatchesBoyerMoore(const Text, SearchText: string;
  out Matches: TSizeIntArray; MatchCount: SizeInt = 0;
  IgnoreCase: Boolean = False): SizeInt; inline;

{ ============================================================ }
{ Combinatorial Function 组合函数                              }
{ ============================================================ }

// Search string, it will automatically select efficient algorithms
// based on the length of SearchText.
// Return Value: Number of matched(Matches contain the byte index of the search result)
// 搜索字符串，根据 SearchText 的长度自动选择高效算法
// 返回值：成功匹配的次数（Matches 中存放的是搜索结果的字节索引）
function FindMatches(const Text: string; SearchText: string;
  out Matches: TSizeIntArray; MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): SizeInt; inline;

function UTF8FindMatches(const Text: string; SearchText: string;
  out Matches: TSizeIntArray; MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): SizeInt; inline;

// Replace string, It will automatically select efficient algorithms
// based on the length of SearchText.
// 替换字符串，根据 SearchText 的长度自动选择高效算法
function ReplaceMatches(const Text, SearchText, ReplaceText: string;
  out Matches: TSizeIntArray; MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): string;

function UTF8ReplaceMatches(const Text, SearchText, ReplaceText: string;
  out Matches: TSizeIntArray; MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): string;

// Replace for regular expression string, it uses functions in the RegExpr unit to search.
// 搜索正则表达式字符串，使用 RegExpr 单元中的功能进行搜索
function RegExprFindMatches(const Text: string; SearchText: string;
  out Matches: TSizeIntArray; MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): SizeInt; inline;

// Replace regular expression string, it uses functions in the RegExpr unit to search.
// 替换正则表达式字符串，使用 RegExpr 单元中的功能进行搜索
function RegExprReplaceMatches(const Text: string; SearchText, ReplaceText: string;
  out Matches: TSizeIntArray; MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): string;

// Search string
// Return Value: The byte index of the last matched
// 搜索字符串
// 返回值：最后一次匹配的字节位置
function StringPos(const Text, SearchText: String; MatchCount: SizeInt = 1;
  IgnoreCase: Boolean = False): SizeInt;

// Replace string
// Return Value: The replaced string
// 替换字符串
// 返回值：替换之后的字符串
function StringReplace(const Text, SearchText, ReplaceText: String;
const MatchCount: SizeInt = 0; const IgnoreCase: Boolean = False): string; inline;

{ ============================================================ }
{ Auxiliary Function 辅助函数                                  }
{ ============================================================ }

// Invert the case state
// 反转大小写状态
function InvertCase(const C: Char): Char;
function InvertCase(const Str: PChar; Size: SizeInt): string;
function InvertCase(const Str: String): string; inline;

// Convert the byte index in Matches to the UTF8 character index, indexing from 1
// 将 Matches 中的字节索引转换为 UTF8 字符索引，索引都是从 1 开始
procedure ByteMatchesToUTF8Matches(const Str: string; var Matches: TSizeIntArray);

// Get the number of UTF8 characters in Str
// 获取 Str 中 UTF8 字符的个数
function UTF8LengthFast(const Str: PChar; Size: SizeInt): SizeInt;
function UTF8LengthFast(const Str: String): SizeInt; inline;

implementation

uses
  uCommon,
  RegExpr; // Enabled Unicode support | 开启 Unicode 支持

{ ============================================================ }
// Main parameter description
// Text       : The source string in which to search
// SearchChar : The character to search for
// SearchText : The string to search for
// TextSize   : The length of source string to search in
// SearchSize : The length of the string to search for
// Matches    : Array of search results, array index from 0, string index from 1
// MatchCount : Limit the number of matches, 0 means no limit
// IgnoreCase : Ignore case

// 主要参数说明
// Text       ：要在其中进行搜索的源字符串
// SearchChar ：要搜索的字符
// SearchText ：要搜索的字符串
// TextSize   ：要在其中进行搜索的源字符串长度
// SearchSize ：要搜索的字符串长度
// Matches    ：存放搜索结果的数组，数组索引从 0 开始，存放的字符串索引从 1 开始
// MatchCount ：限制匹配次数，0 表示无限制
// IgnoreCase ：忽略大小写
{ ============================================================ }

{ ============================================================ }
{ Base Function 基础函数                                       }
{ ============================================================ }

// Find single character, for single character, this function is faster than
// the BoyerMoore algorithm.
// Return Value: Number of matched(Matches contain the byte index of the search result)
// 查找单个字符，对于搜索单个字符，速度比 BoyerMoore 算法快
// 返回值：成功匹配的次数（Matches 中存放的是搜索结果的字节索引）
function FindMatchesChar(const Text: PChar; const SearchChar: Char;
  TextSize: SizeInt; out Matches: TSizeIntArray; MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): SizeInt;
const
  // Incremental value when expanding Matches | 扩展 Matches 时的增量值
  MATCHESCOUNTRESIZER = 100;
var
  // The initialization length of Matches | Matches 的初始化长度
  MatchesAllocatedLimit: SizeInt = 1;
  // Number of matched | 已匹配的数量
  FoundCount: Integer;

  // Dynamically expand the length of the match, it doesn't need to add the
  // inline keyword because the loop executes this code not every time.
  // 动态扩展 Matches 的长度，不是每次循环都会执行这里的代码，所以不必
  // 添加 inline 关键字
  procedure ResizeAllocatedMatches;
  begin
    MatchesAllocatedLimit:=FoundCount+MATCHESCOUNTRESIZER;
    SetLength(Matches,MatchesAllocatedLimit);
  end;

  // Add a search result | 添加一个查找结果
  procedure AddMatch(APosition: SizeInt); inline;
  begin
    // 根据需要动态扩展 Matches 的长度
    // dynamically expand the length of Matches as needed
    if FoundCount = MatchesAllocatedLimit then ResizeAllocatedMatches;
    // Add search result | 添加查找结果
    Matches[FoundCount]:=APosition;
    // Number of matched, used for comparison with the parameter MatchCount
    // 已匹配的数量，用于与参数 MatchCount 进行比较
    inc(FoundCount);
  end;

var
  CurPos: SizeInt;

  // SearchChar after invert case | 反转大小写之后的 SearchChar
  SearchChar2: Char;

begin
  SetLength(Matches, MatchesAllocatedLimit);

  // Define a case-inverted character for "case-insensitive" comparison
  // 定义一个大小写相反的字符用来进行“不区分大小写”的比较
  if IgnoreCase then begin
    SearchChar2 := InvertCase(SearchChar);
    // If two characters are the same, there is no need to distinguish between case
    // 如果两个字符相同，则没必要区分大小写
    IgnoreCase := SearchChar <> SearchChar2;
  end;

  CurPos := 0;
  FoundCount := 0;
  if IgnoreCase then begin
    while (CurPos < TextSize) do begin
      if (Text[CurPos] = SearchChar) or (Text[CurPos] = SearchChar2) then begin
        AddMatch(CurPos + 1);
        if FoundCount = MatchCount then break;
      end;
      Inc(CurPos);
    end;
  end else begin
    while (CurPos < TextSize) do begin
      if Text[CurPos] = SearchChar then begin
        AddMatch(CurPos + 1);
        if FoundCount = MatchCount then break;
      end;
      Inc(CurPos);
    end;
  end;

  SetLength(Matches, FoundCount);
  Result := FoundCount;
end;

function FindMatchesChar(const Text: String; const SearchChar: Char;
  out Matches: TSizeIntArray; MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): SizeInt; inline;
begin
  Result := FindMatchesChar(PChar(Text), SearchChar,
    Length(Text), Matches, MatchCount, IgnoreCase);
end;

const
  // Uppercase lowercase mapping table for quickly convert characters in the
  // source string to lowercase
  // 大写转小写映射表，用于快速将源字符串中的字符转换成小写
  LowerCaseArray: array [0..255] of char = (
  #0,#1,#2,#3,#4,#5,#6,#7,#8,#9,#10,
  #11,#12,#13,#14,#15,#16,#17,#18,#19,#20,
  #21,#22,#23,#24,#25,#26,#27,#28,#29,#30,
  #31,#32,#33,#34,#35,#36,#37,#38,#39,#40,
  #41,#42,#43,#44,#45,#46,#47,#48,#49,#50,
  #51,#52,#53,#54,#55,#56,#57,#58,#59,#60,
  #61,#62,#63,#64,
  'a','b','c','d','e','f','g','h','i','j','k','l','m',
  'n','o','p','q','r','s','t','u','v','w','x','y','z',
  #91,#92,#93,#94,#95,#96,
  'a','b','c','d','e','f','g','h','i','j','k','l','m',
  'n','o','p','q','r','s','t','u','v','w','x','y','z',
  #123,#124,#125,#126,#127,#128,#129,#130,
  #131,#132,#133,#134,#135,#136,#137,#138,#139,#140,
  #141,#142,#143,#144,#145,#146,#147,#148,#149,#150,
  #151,#152,#153,#154,#155,#156,#157,#158,#159,#160,
  #161,#162,#163,#164,#165,#166,#167,#168,#169,#170,
  #171,#172,#173,#174,#175,#176,#177,#178,#179,#180,
  #181,#182,#183,#184,#185,#186,#187,#188,#189,#190,
  #191,#192,#193,#194,#195,#196,#197,#198,#199,#200,
  #201,#202,#203,#204,#205,#206,#207,#208,#209,#210,
  #211,#212,#213,#214,#215,#216,#217,#218,#219,#220,
  #221,#222,#223,#224,#225,#226,#227,#228,#229,#230,
  #231,#232,#233,#234,#235,#236,#237,#238,#239,#240,
  #241,#242,#243,#244,#245,#246,#247,#248,#249,#250,
  #251,#252,#253,#254,#255);

// Search string (Boyer Moore algorithm), copied from StrUtils, and made a simple modification.
// suitable for long SearchText
// Return Value: Number of matched(Matches contain the byte index of the search result)
// 查找字符串（Boyer Moore 算法），从 StrUtils 中复制而来，进行了简单的修改
// 适合 SearchText 比较长的情况
// 返回值：成功匹配的次数（Matches 中存放的是搜索结果的字节索引）
function FindMatchesBoyerMoore(const Text: PChar; SearchText: PChar;
  TextSize, SearchSize: SizeInt; out Matches: TSizeIntArray; MatchCount: SizeInt = 0;
  IgnoreCase: Boolean = False): SizeInt;
const
  // The length of the mapping table | 映射表的长度
  ALPHABET_LENGHT=256;
  // Incremental value when expanding Matches | 扩展 Matches 时的增量值
  MATCHESCOUNTRESIZER=100;
var
  // The initialization length of Matches | Matches 的初始化长度
  MatchesAllocatedLimit: SizeInt = 1;
  // Number of matched | 已匹配的数量
  FoundCount: Integer;
type
  AlphabetArray=array [0..ALPHABET_LENGHT-1] of SizeInt;

  function Max(a1,a2: SizeInt): SizeInt; inline;
  begin
    if a1>a2 then Result:=a1 else Result:=a2;
  end;

  // Calculate "Bad Character" jump table.
  // "Bad Character" is the character that does not match when the pattern
  // is compared to the source string.
  // 计算“坏字符”跳转表
  // “坏字符”是Pattern与源字符串进行比较时，不匹配的那个字符
  procedure MakeDeltaJumpTable1(out DeltaJumpTable1: AlphabetArray;
    const APattern: PChar; APatternSize: SizeInt);
  var
    i: SizeInt;
  begin
    // If encounter a character that does not exist in the pattern,
    // jump to the length of the Pattern to continue to compare
    // 如果遇到 Pattern 中不存在的字符，则跳转到 Pattern 长度之后继续比较
    for i := 0 to ALPHABET_LENGHT-1 do
      DeltaJumpTable1[i]:=APatternSize;
    // If encounter a character that exists in the Pattern, then align the character
    // with the corresponding character in the Pattern, and then compare it again.
    // The last character does not need to be processed because the BoyerMoore algorithm
    // compares from the tail of Pattern. It doesn't need to align the source character
    // with the last character again.
    // 如果遇到 Pattern 中存在的字符，则使该字符与 Pattern 中相应的字符对齐后，重新
    // 比较，最后一个字符无需处理因为 BoyerMoore 算法是从 Pattern 尾部向前进行比较
    // 的，无论如何不需要将当前比较的源字符再次与最后一个字符对齐
    for i := 0 to APatternSize - 1 - 1 do
      DeltaJumpTable1[Ord(APattern[i])]:=APatternSize - 1 - i;
  end;

  // Check whether the "substring" ending with APos is at the end of the Pattern
  // and returns the length of the "substring"
  // 判断以 APos 结尾的“子串”是否在 Pattern 的尾部，并返回“子串”的长度
  function SuffixLength(const APattern: PChar;
    APatternSize, APos: SizeInt): SizeInt; inline;
  var
    i: SizeInt;
  begin
    i:=0;
    while (APattern[APos-i] = APattern[APatternSize-1-i]) and (i <= APos) do
      inc(i);
    Result:=i;
  end;

  // Calculate the "Good Suffix" jump table
  // "Good Suffix" is the matched characters when the Pattern compared to the source string
  // 计算“好后缀”跳转表
  // “好后缀”是 Pattern 与源字符串进行比较时，相匹配的那些字符
  procedure MakeDeltaJumpTable2(var DeltaJumpTable2: TSizeIntArray;
    const aPattern: PChar; aPatternSize: SizeInt);
  var
    Position: SizeInt;
    SuffixLengthValue: SizeInt;
  begin
    FillByte(DeltaJumpTable2[0], Length(DeltaJumpTable2) * Sizeof(SizeInt), 0);

    // The last character is not processed and remains 0, so that when comparing with the
    // "Bad Character" rule, always select the "Bad Character" rule.
    // 最后一个字符不做处理，保持为 0，这样当与“坏字符”规则进行比较时，始终选择“坏字符”规则
    for Position := 0 to aPatternSize - 2 do begin
      // If the jump value of current character is not set, set it to normal jump value first
      // 如果当前字符未设置，则先设置为普通跳转值
      if DeltaJumpTable2[Position] = 0 then
        DeltaJumpTable2[Position] := aPatternSize + (aPatternSize - 1 - Position);
      // Check whether the "Substring" that ends with the current character matches the
      // "Good Suffix" and obtain the matching "Substring" length
      // 判断以当前字符结尾的“子串”是否与“好后缀”匹配，并获取匹配的“子串”长度
      SuffixLengthValue:=SuffixLength(aPattern,aPatternSize,Position);
      // If such a "Substring" exists, the jump position of the previous character of
      // the "Good Suffix" is modified to be the first character position of the "Substring".
      // 如果存在这样的“子串”，则修改“好后缀”前一个字符的跳转位置为“子串”的首字符位置
      if SuffixLengthValue > 0 then
        DeltaJumpTable2[aPatternSize - 1 - SuffixLengthValue] :=
          aPatternSize - 1 - Position + SuffixLengthValue;
    end;
  end;

  // Dynamically expand the length of the match,It doesn't need to add the
  // inline keyword because the loop executes this code not every time.
  // 动态扩展 Matches 的长度，不是每次循环都会执行这里的代码，所以不必
  // 添加 inline 关键字
  procedure ResizeAllocatedMatches;
  begin
    MatchesAllocatedLimit:=FoundCount+MATCHESCOUNTRESIZER;
    SetLength(Matches,MatchesAllocatedLimit);
  end;

  // Add a search result | 添加一个查找结果
  procedure AddMatch(APosition: SizeInt); inline;
  begin
    // Dynamically expand the length of Matches as needed
    // 根据需要动态扩展 Matches 的长度
    if FoundCount = MatchesAllocatedLimit then
      ResizeAllocatedMatches;
    // Add search result | 添加查找结果
    Matches[FoundCount]:=APosition;
    // Number of matched, used for comparison with the parameter MatchCount
    // 已匹配的数量，用于与参数 MatchCount 进行比较
    inc(FoundCount);
  end;

var
  CurPos,SubPos: SizeInt;
  DeltaJumpTable1: array [0..ALPHABET_LENGHT-1] of SizeInt;
  DeltaJumpTable2: TSizeIntArray;

  // Return False means continue search, return True means finish search
  // 返回 False 表示继续查找，返回 True 表示查找完毕
  function CompareOnce: boolean; inline;
  begin
    // No bad character, exact match | 没有坏字符，完全匹配
    if (SubPos<0) then begin
      AddMatch(CurPos+2);
      Result := FoundCount = MatchCount;
      inc(CurPos,SearchSize);
      inc(CurPos,SearchSize);
    end else begin
      // Compare the jump length of "Bad Character" rule and the jump length of
      // "Good Suffix" rule, return the longer value of the jump length.
      // 比较“坏字符”规则的跳转长度和“好后缀”规则的跳转长度，返回跳转更长的值
      CurPos:=CurPos + Max(DeltaJumpTable1[Ord(Text[CurPos])],DeltaJumpTable2[SubPos]);
      Result := False;
    end;
  end;

var
  LowerPattern: string;

begin
  SetLength(Matches, MatchesAllocatedLimit);

  // Must check if the SearchText is empty, otherwise an exception will occur
  // when calculating the "good suffix" jump table
  // 必须检查 SearchText 是否为空，否则在计算“好后缀”跳转表时会出现异常
  if (SearchSize=0) then begin
    SetLength(Matches, 0);
    Result := 0;
    Exit;
  end;

  if IgnoreCase then begin
    // Create a lower case pattern for "case insensitive" comparisons
    // 创建一个小写的 Pattern，用来进行“不区分大小写”的比较
    SetLength(LowerPattern, SearchSize);
    for CurPos:= 0 to Pred(SearchSize) do
      LowerPattern[CurPos+1] := LowerCaseArray[Ord(SearchText[CurPos])];
    SearchText := @LowerPattern[1];
  end;

  // Create "bad character" jump table | 创建“坏字符”跳转表
  MakeDeltaJumpTable1(DeltaJumpTable1,SearchText,SearchSize);
  // Change the jump value of the uppercase character in the "bad character" jump table
  // to be the same as the lowercase character so that capitalization can be ignored.
  // 将“坏字符”跳转表中大写字符的跳转值改成与小写字符相同，这样就可以忽略大小写了
  if IgnoreCase then
    for CurPos := Ord('A') to Ord('Z') do
      DeltaJumpTable1[CurPos] := DeltaJumpTable1[CurPos + 32];

  // Create "good suffix" jump table | 创建“好后缀”跳转表
  SetLength(DeltaJumpTable2,SearchSize);
  MakeDeltaJumpTable2(DeltaJumpTable2,SearchText,SearchSize);

  FoundCount := 0;
  // Compare forwards from the tail of the Pattern
  // 从 Pattern 的尾部向前进行比较
  CurPos := SearchSize - 1;
  if IgnoreCase then begin
    while (CurPos < TextSize) do begin
      SubPos:=SearchSize-1;
      // Search for the bad character | 查找坏字符
      while (SubPos>=0) and (LowerCaseArray[Ord(Text[CurPos])] = SearchText[SubPos]) do begin
        dec(CurPos);
        dec(SubPos);
      end;
      if CompareOnce then break;
    end;
  end else begin
    while (CurPos < TextSize) do begin
      SubPos:=SearchSize-1;
      // Search for the bad character | 查找坏字符
      while (SubPos>=0) and (Text[CurPos] = SearchText[SubPos]) do begin
        dec(CurPos);
        dec(SubPos);
      end;
      if CompareOnce then break;
    end;
  end;

  SetLength(Matches, FoundCount);
  Result := FoundCount;
end;

function FindMatchesBoyerMoore(const Text, SearchText: string;
  out Matches: TSizeIntArray; MatchCount: SizeInt = 0;
  IgnoreCase: Boolean = False): SizeInt; inline;
begin
  Result := FindMatchesBoyerMoore(PChar(Text), PChar(SearchText),
    Length(Text), Length(SearchText), Matches, MatchCount, IgnoreCase);
end;

{ ============================================================ }
{ Combinatorial Function 组合函数                              }
{ ============================================================ }

// Search string(byte), it will automatically select efficient algorithms
// based on the length of SearchText.
// Return Value: Number of matched(Matches contain the byte index of the search result)
// 搜索字符串（byte），根据 SearchText 的长度自动选择高效算法
// 返回值：成功匹配的次数（Matches 中存放的是搜索结果的字节索引）
function FindMatches(const Text: string; SearchText: string;
  out Matches: TSizeIntArray; MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): SizeInt; inline;
begin
  if Length(SearchText) = 1 then
    Result := FindMatchesChar(PChar(Text), SearchText[1], Length(Text), Matches, MatchCount, IgnoreCase)
  else
    Result := FindMatchesBoyerMoore(PChar(Text), PChar(SearchText), Length(Text),
      Length(SearchText), Matches, MatchCount, IgnoreCase);
end;

// Search string(UTF8), it will automatically select efficient algorithms
// based on the length of SearchText.
// Return Value: Number of matched(Matches contain the UTF8 character index of the search result)
// 搜索字符串（UTF8），根据 SearchText 的长度自动选择高效算法
// 返回值：成功匹配的次数（Matches 中存放的是搜索结果的 UTF8 字符索引）
function UTF8FindMatches(const Text: string; SearchText: string;
  out Matches: TSizeIntArray; MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): SizeInt; inline;
begin
  if Length(SearchText) = 1 then
    Result := FindMatchesChar(PChar(Text), SearchText[1], Length(Text), Matches, MatchCount, IgnoreCase)
  else
    Result := FindMatchesBoyerMoore(PChar(Text), PChar(SearchText), Length(Text),
      Length(SearchText), Matches, MatchCount, IgnoreCase);
  ByteMatchesToUTF8Matches(Text, Matches);
end;

// Replace string(byte), it will automatically select efficient algorithms
// based on the length of SearchText.
// Return Value: Number of replaced(Matches contain the byte index of the replace result)
// 替换字符串（byte），根据 SearchText 的长度自动选择高效算法
// 返回值：替换结果（Matches 中存放的是替换结果的字节索引）
function ReplaceMatches(const Text, SearchText, ReplaceText: string;
  out Matches: TSizeIntArray; MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): string;
var
  ASearchSize: SizeInt;
  AReplaceSize: SizeInt;
  MatchesCount: SizeInt;
  MatchIndex: SizeInt;
  MatchTarget: SizeInt;
  MatchInternal: SizeInt;
  AdvanceIndex: SizeInt;
begin
  // Search for results first and then replace them one by one
  // 先搜索出结果，再逐个替换
  MatchesCount := FindMatches(Text, SearchText, Matches, MatchCount, IgnoreCase);

  if MatchesCount = 0 then begin
    Result := Text;
    Exit;
  end;

  ASearchSize := Length(SearchText);
  AReplaceSize := Length(ReplaceText);

  SetLength(Result, Length(Text)+(AReplaceSize-ASearchSize)*MatchesCount);

  MatchIndex:=1;
  MatchTarget:=1;

  for MatchInternal := 0 to Pred(MatchesCount) do begin
    // The number of bytes between the last search result and this search result,
    // excluding the search result itself
    // 上次搜索结果和本次搜索结果之间的字节数，不包括搜索结果本身
    AdvanceIndex:=Matches[MatchInternal]-MatchIndex;
    // Write content between search results into Result
    // 将搜索结果之间的内容写入 Result 中
    move(Text[MatchIndex],Result[MatchTarget],AdvanceIndex);
    // Correct the "Search Result" index to the "Replacement Result" index
    // 将“查找结果”的索引值修正为“替换结果”的位置
    Matches[MatchInternal] := MatchTarget;
    // Position after this search result
    // 本次搜索结果之后的位置
    inc(MatchIndex,AdvanceIndex + ASearchSize);
    // The starting position of this replacement result
    // 本次替换结果的起始位置
    inc(MatchTarget,AdvanceIndex);
    // If the replacement result is not empty, then write it into the Result
    // 如果替换内容不为空，则将替换内容写入 Result 中
    if AReplaceSize>0 then begin
      move(ReplaceText[1],Result[MatchTarget],AReplaceSize);
      // The position after this replacement result
      // 本次替换结果之后的位置
      inc(MatchTarget,AReplaceSize);
    end;
  end;
  // Write the content after the last search result into the Result
  // 将最后一个搜索结果之后的内容写入 Result 中
  if MatchTarget<=Length(Result) then
    move(Text[MatchIndex],Result[MatchTarget],Length(Result)-MatchTarget+1);
end;

// Replace string(UTF8), it will automatically select efficient algorithms
// based on the length of SearchText.
// Return Value: Number of replaced(Matches contain the UTF8 character index of the replacement result)
// 替换字符串（UTF8），根据 SearchText 的长度自动选择高效算法
// 返回值：替换结果（Matches 中存放的是替换结果的字节索引）
function UTF8ReplaceMatches(const Text, SearchText, ReplaceText: string;
  out Matches: TSizeIntArray; MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): string;
var
  ASearchSize: SizeInt;
  AReplaceSize: SizeInt;
  MatchesCount: SizeInt;
  MatchIndex: SizeInt;
  MatchTarget: SizeInt;
  MatchInternal: SizeInt;
  AdvanceIndex: SizeInt;
  MarchTargetU: SizeInt;
  AReplaceULen: SizeInt;
begin
  MatchesCount := FindMatches(Text, SearchText, Matches, MatchCount, IgnoreCase);

  if MatchesCount = 0 then begin
    Result := Text;
    Exit;
  end;

  ASearchSize:=Length(SearchText);
  AReplaceSize:=Length(ReplaceText);

  SetLength(Result,Length(Text)+(AReplaceSize-ASearchSize)*MatchesCount);

  MatchIndex:=1;
  MatchTarget:=1;
  MarchTargetU:=1;
  AReplaceULen:=UTF8LengthFast(PChar(ReplaceText), Length(ReplaceText));

  for MatchInternal := 0 to Pred(MatchesCount) do begin
    // The number of bytes between the last search result and this search result,
    // excluding the search result itself
    // 上次搜索结果和本次搜索结果之间的字节数，不包括搜索结果本身
    AdvanceIndex:=Matches[MatchInternal]-MatchIndex;
    // Write content between search results into Result
    // 将搜索结果之间的内容写入 Result 中
    move(Text[MatchIndex],Result[MatchTarget],AdvanceIndex);
    // Correct the "Search Result" index to the "Replacement Result" index
    // 将“查找结果”的索引值修正为“替换结果”的位置
    Inc(MarchTargetU, UTF8LengthFast(@Text[MatchIndex], AdvanceIndex));
    Matches[MatchInternal] := MarchTargetU;
    Inc(MarchTargetU, AReplaceULen);
    // Position after this search result
    // 本次搜索结果之后的位置
    inc(MatchIndex,AdvanceIndex + ASearchSize);
    // The starting position of this replacement result
    // 本次替换结果的起始位置
    inc(MatchTarget,AdvanceIndex);
    // If the replacement result is not empty, then write it into the Result
    // 如果替换内容不为空，则将替换内容写入 Result 中
    if AReplaceSize>0 then begin
      move(ReplaceText[1],Result[MatchTarget],AReplaceSize);
      // The position after this replacement result
      // 本次替换结果之后的位置
      inc(MatchTarget,AReplaceSize);
    end;
  end;
  // Write the content after the last search result into the Result
  // 将最后一个搜索结果之后的内容写入 Result 中
  if MatchTarget<=Length(Result) then
    move(Text[MatchIndex],Result[MatchTarget],Length(Result)-MatchTarget+1);
end;

// Regular expression search(UTF8)
// Return Value: Number of matched(Matches store "the UTF8 character index of search resule"
// and "search result length" information pairs)
// 正则表达式搜索（UTF8）
// 返回值：成功匹配的次数（Matches 中存放的是“结果位置”的 UTF8 字符索引和“结果长度”信息对）
function RegExprFindMatches(const Text: string; SearchText: string;
  out Matches: TSizeIntArray; MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): SizeInt;
const
  // Incremental value when expanding Matches | 扩展 Matches 时的增量值
  MATCHESCOUNTRESIZER = 200;
var
  // The initialization length of Matches | Matches 的初始化长度
  MatchesAllocatedLimit: SizeInt = 2;
  // Number of matched | 已匹配的数量
  FoundCount: Integer;

  // Dynamically expand the length of the match. It doesn't need to add the
  // inline keyword because the loop executes this code not every time.
  // 动态扩展 Matches 的长度，不是每次循环都会执行这里的代码，
  // 所以不必添加 inline 关键字
  procedure ResizeAllocatedMatches;
  begin
    MatchesAllocatedLimit:=FoundCount * 2 + MATCHESCOUNTRESIZER;
    SetLength(Matches,MatchesAllocatedLimit);
  end;

  // Add a search result | 添加一个查找结果
  procedure AddMatch(const APosition, ALength: SizeInt); inline;
  begin
    // Dynamically expand the length of Matches as needed
    // 根据需要动态扩展 Matches 的长度
    if FoundCount * 2 >= MatchesAllocatedLimit then ResizeAllocatedMatches;
    // Add search result | 添加查找结果
    Matches[FoundCount * 2] := APosition;
    Matches[FoundCount * 2 + 1] := ALength;
    // Number of matched, used for comparison with the parameter MatchCount
    // 已匹配的数量，用于与参数 MatchCount 进行比较
    inc(FoundCount);
  end;

var
  Expr: TRegExpr;

  // Whether the current match successful | 当前匹配是否成功
  Found: Boolean;
begin
  // SearchText is empty will cause the regular expression exception
  // SearchText 为空将导致正则表达式异常
  if (SearchText = '') then begin
    SetLength(Matches, 0);
    Result := 0;
    Exit;
  end;

  SetLength(Matches, MatchesAllocatedLimit);

  if IgnoreCase then
    SearchText := '(?i)' + SearchText;

  Expr := TRegExpr.Create(SearchText);

  FoundCount := 0;
  Found := Expr.Exec(Text);
  while Found do begin
    // String index for regular expression processing starts at 1
    // 正则表达式对字符串的索引从 1 开始
    AddMatch(Expr.MatchPos[0], Expr.MatchLen[0]);
    if FoundCount = MatchCount then Break;
    Found := Expr.ExecNext;
  end;
  Expr.Free;
  SetLength(Matches, FoundCount * 2);
  Result := FoundCount;
end;

// Regular expression replace(UTF8)
// Return Value: Number of matched(Matches store "the UTF8 character index of replacement resule"
// and "replacement result length" information pairs)
// 正则表达式替换（UTF8）
// 返回值：成功匹配的次数（Matches 中存放的是“结果位置”的 UTF8 字符索引和“结果长度”信息对）
function RegExprReplaceMatches(const Text: string; SearchText, ReplaceText: string;
  out Matches: TSizeIntArray; MatchCount: SizeInt = 0;
  IgnoreCase: boolean = False): string;
const
  // Incremental value when expanding Matches | 扩展 Matches 时的增量值
  MATCHESCOUNTRESIZER = 200;
var
  // The initialization length of Matches | Matches 的初始化长度
  MatchesAllocatedLimit: SizeInt = 2;
  // Number of matched | 已匹配的数量
  FoundCount: Integer;
  // Stored result buffer | 存放结果的缓冲区
  ResultBuffer: TStringBuffer;

  // Dynamically expand the length of the match. It doesn't need to add the
  // inline keyword because the loop executes this code not every time.
  // 动态扩展 Matches 的长度，不是每次循环都会执行这里的代码，
  // 所以不必添加 inline 关键字
  procedure ResizeAllocatedMatches;
  begin
    MatchesAllocatedLimit:=FoundCount * 2 + MATCHESCOUNTRESIZER;
    SetLength(Matches,MatchesAllocatedLimit);
  end;

  // Add a search result | 添加一个查找结果
  procedure AddMatch(const APosition, ALength: SizeInt); inline;
  begin
    // Dynamically expand the length of Matches as needed
    // 根据需要动态扩展 Matches 的长度
    if FoundCount * 2 >= MatchesAllocatedLimit then ResizeAllocatedMatches;
    // Add search result | 添加查找结果
    Matches[FoundCount * 2] := APosition;
    Matches[FoundCount * 2 + 1] := ALength;
    // Number of matched, used for comparison with the parameter MatchCount
    // 已匹配的数量，用于与参数 MatchCount 进行比较
    inc(FoundCount);
  end;

var
  Expr: TRegExpr;
  // Whether the current match successful
  // 当前匹配是否成功
  Found: Boolean;
  // The content between the previous match result and the current match result
  // 前一匹配结果和当前匹配结果之间的内容
  MiddleString: string;
  // The current replaced content
  // 当前匹配结果替换后的内容
  CurrReplaced: string;
  // The index of the character which is after the previous match
  // 前一匹配结果之后的一个字符
  Start: SizeInt;
  // The start position of the current replacement result in the entire result string
  // 当前替换结果在整个结果字符串中的起始位置
  ReplacedStart: SizeInt;
  // The length of current replacement result
  // 当前替换结果的长度
  ReplacedLength: SizeInt;
begin
  // SearchText is empty will cause the regular expression exception
  // SearchText 为空将导致正则表达式异常
  if (SearchText = '') then begin
    SetLength(Matches, 0);
    Result := Text;
    Exit;
  end;

  SetLength(Matches, MatchesAllocatedLimit);

  if IgnoreCase then
    SearchText := '(?i)' + SearchText;

  Result := '';
  // String index for regular expression processing starts at 1
  // 正则表达式对字符串的索引从 1 开始
  Start := 1;
  ReplacedStart := 1;
  ReplacedLength := 0;

  Expr := TRegExpr.Create(SearchText);

  ResultBuffer := TStringBuffer.Create;
  Found := Expr.Exec(Text);
  FoundCount := 0;
  while Found do begin
    // Get the string between matched strings
    // 获取匹配串之间的字符串
    MiddleString := UTF8Copy(Text, Start, Expr.MatchPos[0] - Start);

    // Get current replacement result
    // 获取当前替换结果
    CurrReplaced := ReplaceRegExpr(SearchText, Expr.Match[0], ReplaceText, True);

    // The position and length of the current replacement result
    // 当前替换结果的位置和长度
    ReplacedStart := ReplacedStart + UTF8LengthFast(MiddleString);
    ReplacedLength := UTF8LengthFast(CurrReplaced);
    AddMatch(ReplacedStart, ReplacedLength);

    // Write this replacement result
    // 写入本次替换结果
    ResultBuffer.WriteString(MiddleString);
    ResultBuffer.WriteString(CurrReplaced);
    if FoundCount = MatchCount then Break;

    // Update the start position of find and replace
    // 更新查找和替换的起始位置
    Start := Expr.MatchPos[0] + Expr.MatchLen[0];
    ReplacedStart := ReplacedStart + ReplacedLength;

    Found := Expr.ExecNext;
  end;
  // Write the content of the last search result into the Result
  // 将最后一个查找结果之后的内容写入 Result 中
  MiddleString := UTF8Copy(Text, Start, MaxInt);
  ResultBuffer.WriteString(MiddleString);
  Result := ResultBuffer.DataString;

  SetLength(Matches, FoundCount * 2);

  Expr.Free;
  ResultBuffer.Free;
end;

// Search string
// Return value: last matched byte index
// 搜索字符串
// 返回值：最后一次匹配的字节位置
function StringPos(const Text, SearchText: String; MatchCount: SizeInt = 1;
  IgnoreCase: Boolean = False): SizeInt; inline;
var
  Matches: TSizeIntArray;
begin
  if FindMatches(Text, SearchText, Matches, MatchCount, IgnoreCase) > 0 then
    Result := Matches[High(Matches)]
  else
    Result := 0;
end;

// Replace string
// Return value: Replaced string
// 替换字符串
// 返回值：替换之后的字符串
function StringReplace(const Text, SearchText, ReplaceText: String;
  const MatchCount: SizeInt = 0; const IgnoreCase: Boolean = False): string; inline;
var
  Matches: TSizeIntArray;
begin
  Result := ReplaceMatches(Text, SearchText, ReplaceText, Matches, MatchCount, IgnoreCase);
end;

{ ============================================================ }
{ Auxiliary Function 辅助函数                                  }
{ ============================================================ }

// Invert the case of C
// 反转 C 的大小写状态
function InvertCase(const C: Char): Char;
begin
  case C of
    'a'..'z': Result := Chr(Ord(C) - 32);
    'A'..'Z': Result := Chr(Ord(C) + 32);
    else Result := C;
  end;
end;

// Invert the case of Str, if Str does not contain letters, an empty string is returned
// 反转 Str 的大小写状态，如果 Str 中不包含字母，则返回空字符串
function InvertCase(const Str: PChar; Size: SizeInt): string;
var
  i: Integer;
  NotChanged: Boolean;
begin
  NotChanged := True;
  SetLength(Result, Size);
  for i := 0 to Pred(Size) do
    case Str[i] of
      'a'..'z': begin Result[i+1] := Chr(Ord(Str[i]) - 32); NotChanged := False; end;
      'A'..'Z': begin Result[i+1] := Chr(Ord(Str[i]) + 32); NotChanged := False; end;
      else Result[i+1] := Str[i];
    end;
  if NotChanged then Result := '';
end;

function InvertCase(const Str: String): string; inline;
begin
  Result := InvertCase(PChar(Str), Length(Str));
end;

// Convert the byte index in Matches to the UTF8 character index, indexing from 1
// 将 Matches 中的字节索引转换为 UTF8 字符索引，索引都是从 1 开始
procedure ByteMatchesToUTF8Matches(const Str: string; var Matches: TSizeIntArray);
var
  i: SizeInt;
  BPos: SizeInt;
  UPos: SizeInt;
begin
  BPos:=1;
  UPos:=1;

  for i := 0 to High(Matches) do begin
    Inc(UPos, UTF8LengthFast(@Str[BPos], Matches[i] - BPos));
    BPos := Matches[i];
    Matches[i] := UPos;
  end;
end;

// Get the number of UTF8 characters in Str
// 获取 Str 中 UTF8 字符的个数
function UTF8LengthFast(const Str: PChar; Size: SizeInt): SizeInt;
var
  CurPos: Integer;
begin
  Result := 0;
  CurPos := 0;
  while CurPos < Size do begin
    case Str[CurPos] of
        // #0  ..#127: Inc(CurPos);
        #192..#223: Inc(CurPos, 2);
        #224..#239: Inc(CurPos, 3);
        #240..#247: Inc(CurPos, 4);
        else Inc(CurPos);
    end;
    Inc(Result);
  end;
end;

function UTF8LengthFast(const Str: String): SizeInt; inline;
begin
  Result := UTF8LengthFast(PChar(Str), Length(Str));
end;

end.


