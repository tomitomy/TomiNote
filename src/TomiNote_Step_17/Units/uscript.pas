unit uScript;

{$mode objfpc}{$H+}

// String constants and string literals require this option
// 字符串常量和字符串字面量需要此选项
{$codepage UTF8}

interface

uses
  Classes, SysUtils, RegExpr, uCommon, uSearch;

type
  // Operation type: normal replace, regular expression replace, one-to-one normal replace
  // 操作类型：普通替换，正则表达式替换，一对一普通替换
  TReplaceMode = (rmUnknow, rmGeneral, rmRegExpr, rmOneToOne);

  // One replace operation in scarip.
  // 脚本中的单个替换操作
  PStep = ^TStep;

  TStep = record
    Mode                     : TReplaceMode;
    SearchText               : string;
    ReplaceText              : string;
    Loop                     : Integer;
    IgnoreCase               : boolean;
  end;

  { TScript }

  TScript = class
  private
    FSteps        : TList;
    FExpr         : TRegExpr;
    FTextChanged  : boolean;

    procedure AddStep(Mode: TReplaceMode; SearchText, ReplaceText: string;
      Loop: integer; IgnoreCase: boolean);
    function  GetStep(AIndex: Integer): PStep;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ParseScript(ScriptText: string);
    function  ExecScript(Text: string): string;
    procedure Clear;
    property  TextChanged: boolean read FTextChanged;
  end;

implementation

{ TScript }

constructor TScript.Create;
begin
  inherited Create;
  FSteps := TList.Create;
  FExpr := TRegExpr.Create;
end;

destructor TScript.Destroy;
begin
  FExpr.Free;
  Clear;
  FSteps.Free;
  inherited Destroy;
end;

// Add a replace operation | 添加一个替换操作
procedure TScript.AddStep(Mode: TReplaceMode; SearchText, ReplaceText: string;
  Loop: integer; IgnoreCase: boolean);
var
  Step: PStep;
begin
  Step := new(PStep);
  Step^.Mode         := Mode;
  case Mode of
    rmRegExpr: begin
      if IgnoreCase then
        Step^.SearchText := '(?i)' + RegExprUnEscape(SearchText)
      else
        Step^.SearchText := '(?-i)' + RegExprUnEscape(SearchText);
      Step^.ReplaceText  := RegExprUnEscape(ReplaceText);
    end;
    rmGeneral: begin
      Step^.SearchText   := UnEscape(SearchText);
      Step^.ReplaceText  := UnEscape(ReplaceText);
    end;
    else begin
      Step^.SearchText   := SearchText;   // \x7C|abc
      Step^.ReplaceText  := ReplaceText;
    end;
  end;
  Step^.Loop         := Loop;
  Step^.IgnoreCase   := IgnoreCase;
  FSteps.Add(Step);
end;

function TScript.GetStep(AIndex: Integer): PStep; inline;
begin
  Result := PStep(FSteps[AIndex]);
end;

procedure TScript.Clear;
var
  i: integer;
  Step: PStep;
begin
  for i := FSteps.Count - 1 downto 0 do begin
    Step := GetStep(i);
    Step^.SearchText := '';
    Step^.ReplaceText := '';
    dispose(Step);
  end;
  FSteps.Clear;
end;

// Parse script, Convert text-formatted scripts to list for easy reading
// 解析脚本，将文本格式的脚本转换为列表形式，便于反复读取
procedure TScript.ParseScript(ScriptText: string);
var
  Strs        : TStringList;
  Line        : string;

  Mode        : TReplaceMode;
  SearchText  : string;
  ReplaceText : string;
  Loop        : integer;
  IgnoreCase  : boolean;
begin
  Clear;
  Strs := TStringList.Create;
  Strs.Text := ScriptText;

  // Initialize to default | 初始化为默认值
  Mode := rmGeneral;
  SearchText := '';
  ReplaceText := '';
  Loop := 0;
  IgnoreCase := False;

  // Start to parse script | 开始解析脚本
  for Line in Strs do begin
    if Length(Line) >= 5 then begin
      case LowerCase(Copy(Line, 1, 5)) of
        // Mode of replace | 替换模式
        'mode=':
          case LowerCase(TrimBlank(Copy(Line, 6))) of
            'regexpr'  : Mode := rmRegExpr;
            'onetoone' : Mode := rmOneToOne;
            else         Mode := rmGeneral;
          end;
        // Ignore Case | 区分大小写
        'case=': IgnoreCase := LowerCase(TrimBlank(Copy(Line, 6))) = 'false';
        // Loop count | 循环次数
        'loop=': Loop := StrToIntDef(Copy(Line, 6), 1);
        // Search Text | 搜索内容
        'srch=': SearchText := Copy(Line, 6);
        // Replace Text | 替换内容
        'repl=': if SearchText <> '' then begin
          ReplaceText := Copy(Line, 6);
          AddStep(Mode, SearchText, ReplaceText, Loop, IgnoreCase);
          // Reset | 复位
          SearchText := '';
          ReplaceText := '';
          Loop := 0;
        end;
      end;
    end;
  end;
  // Whether or not there is Loop= at the end, still process the previous Loop
  // 无论结尾有没有 Loop=，依然处理前一个 Loop 信息
  AddStep(rmUnknow, '', '', 1, False);
  Strs.Free;
end;

// Execute a script | 执行一次脚本
// Text        : The text to search in | 要搜索的文本
// Return Value: Whether there is content being replaced | 是否有内容被替换
function TScript.ExecScript(Text: string): string;
var
  Step                     : PStep;
  i, Index, LoopHead, Loop : integer;
  SearchText, ReplaceText  : string;
  Ones1, Ones2             : TStringList;
  Found                    : boolean;
begin
  FTextChanged := False;
  Index        := 0;
  LoopHead     := -1;
  Loop         := 1;
  Found        := False;

  // Loop through the script | 循环执行脚本中的操作
  while Index <= FSteps.Count - 1 do begin
    Step := GetStep(Index);

    // The script will perform multiple replace operations between "loop=n"
    // and "loop=n" (n>0). Loop=end is equivalent to Loop=1(means execute once
    // the later script, without looping)

    // 脚本将执行“loop=n”和“loop=n”(n>0)之间的替换操作多次，
    // Loop=end 相当于 Loop=1（意味着执行一次后面的脚本，不进行循环）

    // ----------
    // loop=10
    // ...
    // loop=5
    // ...
    // loop=end
    // ...
    // ----------

    // Meet Loop flag(Loop>0), if the flag is not at the head of the loop,
    // jump to the head to achieve loop execution.
    // 遇到 Loop 标志(Loop>0)，如果该标志不是循环头，
    // 则跳转到循环头实现循环执行。
    if (Step^.Loop > 0) and (Index <> LoopHead) then begin
      // "Found=False" means no more results and no longer need to continue the loop
      // “Found = False”表示没有更多结果，不再需要继续循环
      if Found and (Loop > 1) then begin
        Dec(Loop);              // Finish one search | 完成一次搜索
        Index    := LoopHead;   // Jump to loop head | 跳转到循环头
        Found    := False;      // Reset for next search | 为下次循环复位
      end else begin
        LoopHead := Index;      // Mark next loop head | 标记下个循环头
        Loop     := Step^.Loop; // Mark next loop count | 标记下个循环次数
        Found    := False;      // Reset for next search | 为下次循环复位
      end;
    end;

    // Start one search 开始一次搜索
    case Step^.Mode of

      // General replace | 普通替换
      rmGeneral: begin
        // Check whether need to replace | 判断是否需要替换
        if StringPos(Text, Step^.SearchText, 1, Step^.IgnoreCase) > 0 then begin
          Text := StringReplace(Text, Step^.SearchText, Step^.ReplaceText, 0, Step^.IgnoreCase);
          FTextChanged := True;
          Found := True;
        end;
      end;

      // Regular expression replace | 正则表达式替换
      rmRegExpr: begin
        FExpr.Expression := Step^.SearchText;
        // Check whether need to replace | 判断是否需要替换
        if FExpr.Exec(Text) then begin
          Text := FExpr.Replace(Text, Step^.ReplaceText, True);
          FTextChanged := True;
          Found := True;
        end;
      end;

      // One-to-one replace | 一对一替换
      rmOneToOne: begin
        Ones1 := TStringList.Create;
        Ones2 := TStringList.Create;

        Ones1.Delimiter := '|';
        Ones1.StrictDelimiter := True;
        Ones1.DelimitedText := Step^.SearchText;

        Ones2.Delimiter := '|';
        Ones2.StrictDelimiter := True;
        Ones2.DelimitedText := Step^.ReplaceText;

        // To replace each element in turn
        // 依次对各个元素进行替换操作
        for i := 0 to Pred(Ones1.Count) do begin
          SearchText := UnEscape(Ones1[i]);
          if i < Ones2.Count then
            ReplaceText := UnEscape(Ones2[i])
          else
            ReplaceText := '';
          // Check whether need to replace | 判断是否需要替换
          if StringPos(Text, SearchText, 1, Step^.IgnoreCase) > 0 then begin
            Text := StringReplace(Text, SearchText, ReplaceText, 0, Step^.IgnoreCase);
            FTextChanged := True;
            Found := True;
          end;
        end;

      end;
    end;
    Inc(Index);
  end;
  Result := Text;
end;

end.

