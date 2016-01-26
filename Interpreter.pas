{==============================================================================}
{  Модуль      : Interpreter.pas                                               }
{  Разработчик : Денис Артёмов (denis.artyomov@gmail.com)                      }
{===============================================================================
                                                                              
Простой интерпретатор языка, похожего на паскаль. 

Возможности языка:
  * Динамическая типизация. Тип переменной задаётся автоматически в момент присваивания ей значения
  * Поддержка типов Integer, Single, String
  * Поддержка массивов любой размерности
  * Поддержка процедур и функций в скрипте
  * Поддержка вызова процедур и функций из вызываемого кода
  * Поддержка внешних констант
  * Поддержка внешних переменных
  * Поддержка операторов для выражений: *, /, +, -, ^
  * Поддержка целочисленных операторов: div, shl, mod, or, and xor
  * Поддержка логических операторов: not, or, and, xor
  * Поддержка операторов сравнения >, >=, <, <=, =, <>
  * Поддержка операторв: := , for, while, if, repeat, break, continue, case, inc, dec  
  * Встроенные константы nil, true, false

Возможности отладчика:
  * Пошаговое исполнение (в работе)
  * Точки останова
  * Просмотр значений переменных и аргументов ф-ции
  * Просмотр стека вызовов

Примеры использования:

==== (1) ======================================================================

	procedure Test1();
	var
		Int : TInterpreter;
	begin
		try
			Int := TInterpreter.Create();
			Int.CompileExpression('10+23/10+12');
			Int.Execute();
			WriteLn('result=', Int.Result.FloatValue);
		finally
			FreeAndNil(Int);
		end;
	end;

==== (2) ======================================================================

	procedure Test2();
	var
		Int : TInterpreter;
		a : TValueWrapper;
	begin
		try
			Int := TInterpreter.Create();
			a := Int.UserVariables.Add('a');
			a.IntValue := 5;
			Int.CompileScript('for i := 1 to 10 do Inc(a)');
			Int.Execute();
			WriteLn('a=', a.IntValue);
		finally
			FreeAndNil(Int);
		end;
	end;

===============================================================================}

// TODO: Eval
// TODO: Показывать место в коде, где произошла runtime-ошибка
// TODO: Улучшить дизасемблер кода виртуальной машины
// TODO: Юнит-тесты для отладчика

unit Interpreter;

interface

uses
	Generics.Collections, SysUtils, Classes, Types;

type

// Целочисленный тип и тип с плавающей точкой
TIntType = Integer;
TFloatType = Single;

// fwd.
TParser = class;
TMemoryManager = class;
TScriptFunctions = class;
TStringValue = class;
TArrayValue = class;

// Исключения
TInterpreterException = class(Exception);
TRuntimeException = class(TInterpreterException);
TTypeCastException = class(TRuntimeException);
TZeroDivException = class(TRuntimeException);
TStackCorruptedException = class(TRuntimeException);
TDebbugerException = class(TRuntimeException);
TCantContinueException = class(TDebbugerException);
TBreakpointNotFoundException = class(TDebbugerException);
TCodeNotFoundForLineException = class(TDebbugerException);
TInternalInterpreterException = class(TInterpreterException);


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TValueType - Возможные типы значений, которыми оперирует виртуальная машина }
TValueType = (
	vtNil,
	vtInt,
	vtFloat,
	vtBool,
	vtString,
	vtConstStr,
	vtArray,
    vtFunStackFrame
);

{ TValue - Значение, с которым оперирует виртуальная машина (для внутреннего использования) }
TValue = record
	function ToString : String;
	function ToInt : Integer; inline;
	function ToFloat : TFloatType; inline;
	function ToBool : Boolean; inline;

	function GetChar(Index : Integer) : TValue; inline;
var
	Typ   : TValueType;               // Тип
	case byte of
		0: ( Int   : TIntType;     ); // Целое значение
		1: ( Float : TFloatType;   ); // Значение с плавающей точкой
		2: ( Bool  : Boolean;      ); // Булевое значение
		3: ( Str   : TStringValue; ); // Строка
		4: ( Arr   : TArrayValue;  ); // Массив
		5: ( CStr  : PString       ); // Строковая константа
		6: ( SF    : Integer       ); // Фрейм стека ф-ции

end;
PValue = ^TValue;

TValueDynArray = array of TValue;
PValueDynArray = ^TValueDynArray;

TValueArray = array[0..65535] of TValue;
PValueArray = ^TValueArray;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TValueWrapper - Обёртка над TValue }
TValueWrapper = class
private
	FValues : PValueDynArray;
	FIndex : Integer;
	FMM : TMemoryManager;

	function GetValue() : TValue; inline;
	function GetType() : TValueType; inline;

	function ToStr() : String;
	procedure SetString(const Value : String); inline;

	function ToInt() : Integer; inline;
	procedure SetInt(Value : Integer); inline;

	function ToFloat() : Single; inline;
	procedure SetFloat(Value : Single); inline;

	function ToBool() : Boolean; inline;
	procedure SetBool(Value : Boolean); inline;

	function TypeIsNil() : Boolean; inline;

public
	constructor Create(Values : PValueDynArray; Index : Integer; MM : TMemoryManager);

	property ValueType : TValueType read GetType;
	property Value : TValue read GetValue;

	property IsNil : boolean read TypeIsNil;
	procedure SetNil; inline;

	property StrValue : string read ToStr write SetString;
	property IntValue : Integer read ToInt write SetInt;
	property FloatValue : Single read ToFloat write SetFloat;
	property BoolValue : Boolean read ToBool write SetBool;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TUserFunctionArgs - аргументы пользовательской функции }
TUserFunctionArgs = class
private
	FItems : array of TValueWrapper;
	FSize : Integer;

	function GetItem(Index : Integer) : TValueWrapper; inline;
	class procedure RaiseErrIndexExceedRange(Index, Size : Integer);

public
	constructor Create();
	destructor Destroy(); override;

	procedure Clear();
	function Add(Values : PValueDynArray; Index : Integer; MM : TMemoryManager) : TValueWrapper;

	property Count : Integer read FSize;
	property Items[Index : Integer] : TValueWrapper read GetItem; default;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TInterpreterFuncCallBack - Тип callback-функции для обработки вызова внешней функций }
TInterpreterFuncCallBack = reference to procedure(Context : Pointer; Args : TUserFunctionArgs; Ret : TValueWrapper);

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TUserFunction - Внешняя функция интерпретатора }
TUserFunction = class
private
	FName : String;
	FArgCount : Integer;
	FContext : Pointer;
	FCallback : TInterpreterFuncCallBack;
	FIndex : Integer;

public
	constructor Create(Index : Integer; const Name : String; ArgCount : Integer; Context : Pointer; Callback : TInterpreterFuncCallBack);

	property Name : String read FName;
	property ArgCount : Integer read FArgCount;
	property Context : Pointer read FContext;
	property Callback : TInterpreterFuncCallBack read FCallback;
	property Index : Integer read FIndex;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TUserFunctions - Список внешних функций интерпретатора }
TUserFunctions = class
private
	FItems : TObjectList<TUserFunction>;

	function GetCount() : Integer; inline;
	function GetItem(Index : Integer) : TUserFunction; inline;

	procedure RegisterStd();

public
	constructor Create();
	destructor Destroy(); override;

	procedure Clear();

	function Add(
		const Name : String;
		ArgCount   : Integer;
		Context    : Pointer;
		Callback   : TInterpreterFuncCallBack
	) : Integer;

	function FindByName(const Name : String) : TUserFunction;
	function IndexByName(const Name : String) : Integer;

	property Count : Integer read GetCount;
	property Items[Index : Integer] : TUserFunction read GetItem; default;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TUserVariables - Пользовательские переменные }
TUserVariables = class
private
	FValuesArray : PValueDynArray;
	FVarNames : TStringList;
	FMM : TMemoryManager;

public
	constructor Create(ValuesArray : PValueDynArray; MM : TMemoryManager);
	destructor Destroy(); Override;

	procedure Clear();

	function Add(const Name : string) : TValueWrapper;
	function GetIndex(const Name : string) : Integer;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TUserConsts - Пользовательские константы }
TUserConsts = class
private
	FIntItems : TDictionary<String, Integer>;
	FSingleItems : TDictionary<String, Single>;
	FStrItems : TDictionary<String, String>;
public
	constructor Create();
	destructor Destroy(); override;

	procedure Clear();
	
	procedure AddStr(const Name, Value : String);
	procedure AddInt(const Name : String; Value : Integer);
	procedure AddSingle(const Name : String; Value : Single);

	function GetStr(const Name : String) : String;
	function GetInt(const Name : String) : Integer;
	function GetSingle(const Name : String) : Single;
	
	function GetConstType(const Name : String) : TValueType;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TTokenFlag - Возможные флаги токена }
TTokenFlag = (
	ltStatementWord,
	ltStatementSubWord,
	ltExprContent,
	ltExprOperator,
	ltConst,
	ltName,
	ltSeq,
	ltStatement,
	ltEndOfText
);
TTokenFlags = set of TTokenFlag;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Тип токена }
TTokenType = (
	ttUnknown,   ttSeq,       ttIntConst,    ttFloatConst,  ttBoolConst,
	ttStrConst,  ttName,      ttSqOpBracket, ttSqClBracket,
	ttAssig,     ttProcedure, ttFunction,    ttBoolean,     ttMult,     ttDiv,
	ttTrue,      ttFalse,     ttBegin,       ttEnd,         ttExit,     ttFor,
	ttWhile,     ttIf,        ttRepeat,      ttBreak,       ttCont,     ttCase,
	ttInc,       ttDec,       ttDo,          ttTo,          ttDownto,   ttThen,
	ttElse,      ttUntil,     ttTry,         ttFinally,
	ttOf,        ttIDiv,      ttMod,         ttNot,
	ttOr,        ttAnd,       ttXor,         ttNil,         ttComma,    ttPower,
	ttPlus,      ttMinus,     ttLess,        ttLessEq,      ttGreater,  ttGreaterEq,
	ttEquals,    ttNotEq,     ttClBracket,   ttOpBracket,   ttSemi,     ttColon,
	ttShl,       ttShr,       ttDot,         ttEOF
);

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TToken - один токен скрипта }
TToken = class
private
	FType : TTokenType;
	FIntConst : Integer;
	FFloatConst : Single;
	FBoolConst : Boolean;
	FCol : Integer;
	FLine : Integer;
	FText : string;
	FFlags : TTokenFlags;

	procedure Init(Flags : TTokenFlags; Col, Line : Integer; TType : TTokenType);

public
	constructor Create(const Text : string; Flags : TTokenFlags; Col, Line : Integer; TType : TTokenType = ttUnknown);
	constructor CreateInt(Value : Integer; Col : Integer = -1; Line : Integer = -1);
	constructor CreateFloat(Value : Single; Col : Integer = -1; Line : Integer = -1);
	constructor CreateBool(Value : Boolean; Col : Integer = -1; Line : Integer = -1);
	constructor CreateStr(const Text : String; Col : Integer = -1; Line : Integer = -1);

	property TokenType : TTokenType read FType;

	property Line : Integer read FLine;
	property Col : Integer read FCol;

	property Text : string read FText;
	property Flags : TTokenFlags read FFlags;

	property IntValue : Integer read FIntConst;
	property FloatValue : Single read FFloatConst;
	property BoolValue : Boolean read FBoolConst;
	property StrValue : String read FText;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TTokenCursor - текущая позиция разбора на токены }
TTokenCursor = record
	Pos  : Integer;
	Col  : Integer;
	Line : Integer;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TTokenizerException - исключение при разбиении на токены }
TTokenizerException = class(TInterpreterException)
private
	FCol : Integer;
	FLine : Integer;
public
	constructor CreateFmt(const Text : String; const Args: array of const; Col, Line : Integer);
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TTokenizer - класс для разбиения текста на токены }
TTokenizer = class
private
	FItems : TObjectList<TToken>;
	FPos : Integer;
	FTokTypesByName : TDictionary<String, TTokenType>;

	function AddToken(const Text : string; Col, Line : Integer; lc : boolean; Flags : TTokenFlags = []; TokenType : TTokenType = ttUnknown) : TToken;
	function GetTokenFlags(TokenType : TTokenType) : TTokenFlags;

	function GetItem(Index : Integer) : TToken; inline;
	function GetCount() : Integer; inline;

	function SkipSingleComment(const Text : string; const Cur : TTokenCursor; Len : Integer) : TTokenCursor;
	function SkipMultiComment (const Text : string; const Cur : TTokenCursor; Len : Integer; Dbl : Boolean) : TTokenCursor;
	function TockenizeName    (const Text : string; const Cur : TTokenCursor; Len : Integer) : TTokenCursor;
	function TockenizeValue   (const Text : string; const Cur : TTokenCursor; Len : Integer) : TTokenCursor;
	function TockenizeComplSym(const Text : string; const Cur : TTokenCursor; Len : Integer) : TTokenCursor;
	function TockenizeString  (const Text : string; const Cur : TTokenCursor; Len : Integer) : TTokenCursor;
	function GetTypeByText(const Name : String) : TTokenType;

	function GetTokenStrByType(TT : TTokenType) : string;
	function GetTokenStrByTypes(const TTs : array of TTokenType) : string;

public
	constructor Create();
	destructor Destroy(); override;

	procedure Start();
	function Get(ExceptIfEnd : Boolean) : TToken;
	procedure Next();
	function Expect(TokenType : TTokenType) : TToken;
	function ExpectAnyOf(const TokenTypes : array of TTokenType) : TToken;

	procedure Tockenize(const Text : string; Consts : TUserConsts);

	function EOF : Boolean; inline;

	property Pos : Integer read FPos;
	property Items[Index : Integer] : TToken read GetItem; default;
	property Count : Integer read GetCount;
end;


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TASTNode - узел AST-дерева }
TASTNode = class
private
	FItems : TList<TASTNode>;
	FToken : TToken;
	FParent : TASTNode;
	FOwnerOfToken : Boolean;
	FParser : TParser;

	function GetCount() : Integer; inline;
	function GetItem(Index : Integer) : TASTNode; inline;
	procedure SetItem(Index : Integer; Item : TASTNode); inline;

protected
	function GetEnumerable: TEnumerable<TASTNode>;

public
	constructor CreateSpecial(Parser : TParser; Parent : TASTNode; TokenType : TTokenType; const Text : string); overload;
	constructor CreateSpecial(Parser : TParser; Parent : TASTNode; TokenType : TTokenType; const Text : string; Arg1 : TASTNode); overload;
	constructor CreateSpecial(Parser : TParser; Parent : TASTNode; TokenType : TTokenType; const Text : string; Arg1, Arg2 : TASTNode); overload;
	constructor Create(Parser : TParser; Tocken : TToken; Parent : TASTNode); overload;
	constructor Create(Parser : TParser; Tocken : TToken; Parent : TASTNode; OwnerOfToken : Boolean); overload;
	constructor Create(Parser : TParser; Tocken : TToken; Parent : TASTNode; Arg1 : TASTNode); overload;
	constructor Create(Parser : TParser; Tocken : TToken; Parent : TASTNode; Arg1, Arg2 : TASTNode); overload;

	destructor Destroy(); override;

	property Items : TEnumerable<TASTNode> read GetEnumerable;
	property Count : Integer read GetCount;
	property Childs[Index : Integer] : TASTNode read GetItem write SetItem; default;

	property Parser : TParser read FParser;
	property Token : TToken read FToken;
	property Parent : TASTNode read FParent;

	procedure Add(Item : TASTNode);

	function UniqStr() : string;

	procedure Swap(OtherAST : TASTNode);

	function IsArrayExpr() : Boolean;

end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TScriptFunction - процедура или функция скрипта }
TScriptFunction = class
private
	FTopNode : TASTNode;
	FBodyNode : TASTNode;
	FName : String;
	FArgList : TStringList;
	FVars : TStringList;
	FIsFunction : Boolean;

	procedure ReadFuncInfo();

	function GetArgByIndex(Index : Integer) : String;
	function GetVarByIndex(Index : Integer) : String;

public
	constructor Create(Node : TASTNode; IsFunction : Boolean);
	destructor Destroy(); override;

	procedure FindLocalVariables(
		UserVars    : TUserVariables;
		UserFuncs   : TUserFunctions;
		UserConsts  : TUserConsts;
		ScriptFuncs : TScriptFunctions
	);

	property BodyAST : TASTNode read FBodyNode;
	property Name : String read FName;
	property IsFunction : Boolean read FIsFunction;

	function VariablesCount : Integer;
	function VarIndex(const VarName : string) : Integer;
	property Vars[Index : Integer] : String read GetVarByIndex;

	function ArgsCount : Integer;
	function ArgIndex(const ArgName : string) : Integer;
	property Args[Index : Integer] : String read GetArgByIndex;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TScriptFunctions - список объявленных функций в скрипте }
TScriptFunctions = class
private
	FItems : TObjectList<TScriptFunction>;

	function GetCount() : Integer; inline;
	function GetItem(Index : Integer) : TScriptFunction; inline;

public
	constructor Create();
	destructor Destroy(); override;

	procedure Clear();
	procedure Add(Func : TScriptFunction);
	function FindByName(const Name : String) : TScriptFunction;

	property Count : Integer read GetCount;
	property Items[Index : Integer] : TScriptFunction read GetItem; default;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TParserException - Исключение при парсинге текста }
TParserException = class(TInterpreterException)
private
	FToken : TToken;

public
	constructor Create(Token : TToken; const Text : String);
	constructor CreateFmt(Token : TToken; const Text : String; const Args: array of const);
	destructor Destroy(); override;

	property Token : TToken read FToken;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TPasrser - парсер скриптов и выражений }
TParser = class
private
	FAllAstNodes : TObjectList<TASTNode>;

	function ParseExprSeq(Parent : TASTNode; Tokens : TTokenizer; DelimTT, EndTT : TTokenType) : TToken;
	function ParseExpr(Tokens : TTokenizer; LastOperPriority : Integer; CanBeNull : Boolean) : TASTNode;
	function ParseStatement(Parent : TASTNode; Tokens : TTokenizer) : TASTNode;
	function ParseStatements(Parent : TASTNode; Tokens : TTokenizer) : TASTNode;
	procedure ParseProcedureArgsDecl(Func : TASTNode; Tokens : TTokenizer);
	function ParseProcedureDecl(Tokens : TTokenizer) : TASTNode;
	function ParseName(NameToken : TToken; Tokens : TTokenizer) : TASTNode;
	function ParseWhile(WhileToken : TToken; Parent : TASTNode; Tokens : TTokenizer) : TASTNode;
	function ParseIf(IfToken : TToken; Parent : TASTNode; Tokens : TTokenizer) : TASTNode;
	function ParseFor(ForToken : TToken; Parent : TASTNode; Tokens : TTokenizer) : TASTNode;
	function ParseRepeat(RepeatToken : TToken; Parent : TASTNode; Tokens : TTokenizer) : TASTNode;
	function ParseIncDec(Token : TToken; Tokens : TTokenizer) : TASTNode;
	function ParseCase(CaseToken : TToken; Parent : TASTNode; Tokens : TTokenizer) : TASTNode;
	function ParseCaseItem(Parent : TASTNode; Tokens : TTokenizer) : TASTNode;
	function ParseTryFinally(TryToken : TToken; Parent : TASTNode; Tokens : TTokenizer) : TASTNode;

	function GetOperPrioryty(Token : TToken) : Integer;

public
	constructor Create();
	destructor Destroy(); override;

	procedure ParseScript(Tokens : TTokenizer; Functions : TScriptFunctions);
	procedure ParseExpression(Tokens : TTokenizer; Expression : TASTNode);
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Инструкции виртуальной машины }
TCodeInstr = (
	ciPushInt,
	ciPushFloat,
	ciPushBool,
	ciPushStr,
	ciPushNil,
	ciPushLocalValue,
	ciPushGlobalValue,
	ciPushLocalArray,
	ciPushGlobalArray,
	ciPushAcc,
	ciPopLocalValue,
	ciPopGlobalValue,
	ciPopLocalArray,
	ciPopGlobalArray,
	ciPopAcc,
	ciFunFrameBegin,
	ciFunFrameEnd,
	ciDecrStack,
	ciAdd,
	ciSub,
	ciMult,
	ciDiv,
	ciIDiv,
	ciMod,
	ciShl,
	ciShr,
	ciPower,
	ciOr,
	ciAnd,
	ciXor,
	ciEqual,
	ciLess,
	ciLessEq,
	ciGreater,
	ciGreaterEq,
	ciNot,
	ciNeg,
	ciJmp,
	ciJmpIf,
	ciJmpIfNot,
	ciReturn,
	ciCall,
	ciUserFunc,
	ciExit,
	ciBreakpoint
);

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TBinaryCodeItem - Одна инструкция }
TBinaryCodeItem = record
	case byte of
		0: (Instr : TCodeInstr);
		1: (Int   : Integer);
		2: (Float : Single);
		3: (Bool  : Boolean);
end;

PBinaryCodeItem = ^TBinaryCodeItem;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TSrcPos - Позиция исполняемой инструкции в коде.
  Используется для определения места, где произошла ошибка }
TSrcPos = record
	Col  : Integer;
	Line : Integer;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TBreakpoint - точка останова }
TBreakpoint = record
	LineNum : Integer;
	Instr   : TCodeInstr;
	CodePos : Integer;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TFunDebugInfo - хранение отладочной инфы для одной ф-ции }
TFunDebugInfo = class
private
	FFunName : String;
	FArgs : TStrings;
	FVars : TStrings;

public
	constructor Create(ScriptFun : TScriptFunction);
	destructor Destroy(); override;

	property FunName : String read FFunName;

	property Args : TStrings read FArgs;
	property Vars : TStrings read FVars;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TDebugInfo - отладочная информация }
TDebugInfo = class
private
	FSrcPositions : TDictionary<Integer,TSrcPos>;
	FFunDebugItems : TObjectList<TFunDebugInfo>;

public
	constructor Create();
	destructor Destroy(); override;

	procedure Clear();

	function AddFunDebugInfo(ScriptFun : TScriptFunction) : Integer;
	procedure AddSrcPos(CodePos : Integer; SrcPos : TSrcPos);

	function GetFunDebugItem(FunIndex : Integer) : TFunDebugInfo;
	function GetCodePosBySrcLine(LineNum : Integer) : Integer;
	function GetSrcPosByAddr(Addr : Integer) : TSrcPos;

	procedure ToStrings(Text : TStrings);
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TBinaryCode - Бинарный код, выполняемый виртуальной машиной }
TBinaryCode = class
private
	FCode : TList<TBinaryCodeItem>;
	FStrings : TList<PString>;
	FBreakpoins : TList<TBreakpoint>;
	FChachedCodeBegin : PBinaryCodeItem;
	FCachedCodeArr : TArray<TBinaryCodeItem>;

	procedure CreateCachedCodeArr();

	procedure DisassembleCode(Text : TStrings; DebugInfo : TDebugInfo);
	procedure DisassembleStrings(Text : TStrings);

public
	constructor Create();
	destructor Destroy(); override;

	function Size : Integer;

	procedure Clear();

	procedure AddInstr(Instr : TCodeInstr; Token : TToken; DebugInfo : TDebugInfo);
	procedure AddInteger(Value : Integer);
	procedure AddFloat(Value : Single);
	procedure AddBool(Value : Boolean);
	function AddStr(const Str : String) : Integer;

	procedure SetInteger(Pos, Value : Integer);

	function GetInstrBegin : PBinaryCodeItem; inline;
	function GetString(Index : Integer) : PString; inline;

	procedure AddBreakPoint(LineNum : Integer; DebugInfo : TDebugInfo);
	procedure RemoveBreakpoint(LineNum : Integer; DebugInfo : TDebugInfo);
	function GetBreakPointIndexByLineNum(LineNum : Integer) : Integer;
	function GetBreakPointIndexByCodePos(CodePos : Integer) : Integer;
	procedure RevertInstructionForBreakpoint(CodePos : Integer);
	procedure SetInstructionsForAllBreakpoints();

	procedure Disassemble(DestText : TStrings; DebugInfo : TDebugInfo);

	procedure SaveToStream(Stream : TStream);
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TCodeGenerator - Генератор бинарного кода для выполнения }
TCodeGenerator = class
private
	FDebugInfo : TDebugInfo;
	FForVariables : TStringList;
	FScriptFuncs : TScriptFunctions;
	FUserFunctions : TUserFunctions;
	FUserConsts : TUserConsts;
	FCode : TBinaryCode;
	FLabels : TDictionary<String, Integer>;
	FLabelsUse : TDictionary<Integer, String>;
	FUserVars : TUserVariables;
	FWasOptimized : Boolean;

	procedure Expr(Func : TScriptFunction; Ast : TASTNode);
	procedure NameExpr(Func : TScriptFunction; Ast : TASTNode);
	procedure ArrExpr(Func : TScriptFunction; Ast : TASTNode; VarIndex : Integer; VarIsGlobal : Boolean);
	procedure SomeFuncCall(Func : TScriptFunction; Ast : TASTNode);
	procedure UserFuncCall(Func : TScriptFunction; Ast : TASTNode; UserFuncIndex : Integer);
	procedure ScriptFuncCall(Func : TScriptFunction; Ast : TASTNode; ScriptFunc : TScriptFunction);
	procedure BinOperExpr(Func : TScriptFunction; Ast : TASTNode);
	procedure UnOperExpr(Func : TScriptFunction; Ast : TASTNode);
	procedure ConstExpr(Ast : TASTNode);
	procedure Fun(Func : TScriptFunction; UserConsts : TUserConsts; AddReturn : Boolean);
	procedure Statement(Func : TScriptFunction; Ast : TASTNode);
	procedure StAssign(Func : TScriptFunction; Ast : TASTNode);
	procedure AssignPop(Func : TScriptFunction; Ast : TASTNode);
	procedure AssignPopArrItem(Func : TScriptFunction; Ast : TASTNode; VarIndex : Integer; VarIsGlobal : Boolean);
	procedure IncDec(Func : TScriptFunction; StAst : TASTNode; D : boolean); overload;
	procedure IncDec(Func : TScriptFunction; NameAst, ValueAst : TASTNode; D : boolean); overload;
	procedure StWhile(Func : TScriptFunction; Ast : TASTNode);
	procedure StIf(Func : TScriptFunction; Ast : TASTNode);
	procedure StFor(Func : TScriptFunction; Ast : TASTNode);
	procedure StRepeat(Func : TScriptFunction; Ast : TASTNode);
	procedure StBreakCont(Func : TScriptFunction; Ast : TASTNode; const Prefix : string);
	procedure StExit(Func : TScriptFunction; Ast : TASTNode);
	procedure StCase(Func : TScriptFunction; Ast : TASTNode);
	procedure StTryFinally(Func : TScriptFunction; Ast : TASTNode);

	procedure SubstituteConsts(var RootAst : TAstNode);
	procedure OptimizeConsts(var RootAst : TAstNode);
	procedure FoldConst(var AST : TASTNode);

	procedure AddLabel(Ast : TASTNode; const Prefix : String); overload;
	procedure AddLabel(const Name : String); overload;
	procedure AddLabelPos(const Name : String);
	procedure AddString(const Str : String);

	function GetLabelPos(const Name : String) : Integer;
	procedure FillLabelPositions();

public
	constructor Create();
	destructor Destroy(); override;

	procedure GenerateFunctions(
		Functions     : TScriptFunctions;
		UserFunctions : TUserFunctions;
		UserVars      : TUserVariables;
		UserConsts    : TUserConsts;
		DebugInfo     : TDebugInfo;
		Code          : TBinaryCode
	);

	procedure GenerateExpression(
		Ast           : TASTNode;
		UserFunctions : TUserFunctions;
		UserVars      : TUserVariables;
		UserConsts    : TUserConsts;
		DebugInfo     : TDebugInfo;
		Code          : TBinaryCode
	);
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TBinOperException - исключение при выполнении бинарной операции }
TBinOperException = class(TRuntimeException)
public
	constructor Create(const Value1, Value2 : TValue; const Oper : string);
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TUnaryOperException - исключение при выполнении унарной операции }
TUnaryOperException = class(TRuntimeException)
public
	constructor Create(const Value : TValue; const Oper : string);
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TWrongTypeException - Исключении при использовании операнда недопустимого для опеарции типа }
TWrongTypeException = class(TRuntimeException)
public
	constructor Create(const Value : TValue; const ExpectedType : string); overload;
	constructor Create(const Value : TValueWrapper; const ExpectedType : string); overload;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TMemoryManagerObject - базовый объект менеджера памяти }
TMemoryManagerObject = class
private
	FUsed : Boolean;

public
	property Used : Boolean read FUsed write FUsed;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TStringValue - объект менеджера памяти - СТРОКА }
TStringValue = class(TMemoryManagerObject)
private
	FString : String;

public
	function GetPStr() : PString;
	property Value : String read FString write FString;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

TVisitAllValuesProc = reference to procedure (const Value : TValue);

{ TArrayValue - объект менеджера памяти - МАССИВ }
TArrayValue = class(TMemoryManagerObject)
private
	FMultiItems : TDictionary<TIntegerDynArray, TValue>;
	FItems : TDictionary<Integer, TValue>;

public
	constructor Create();
	destructor Destroy; override;

	function GetValue_MultiDim(const Args : TIntegerDynArray; out Value : TValue) : Boolean; inline;
	procedure PutValue_MultiDim(const Args : TIntegerDynArray; const Value : TValue); inline;

	function GetValue_OneDim(Index : Integer; out Value : TValue) : Boolean; inline;
	procedure PutValue_OneDim(Index : Integer; const Value : TValue); inline;

	procedure Clear();

	procedure ForEach(Proc : TVisitAllValuesProc);

	function Count : Integer;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TMemoryManager - менеджер памяти. Создаёт объекты, производит сбоку мусора }
TMemoryManager = class
private
	FObjects : TObjectList<TMemoryManagerObject>;
	FAllocCounter : Integer;

	class procedure MarkObjectAsUsed(const Value : TValue);
	procedure TryToCollectGarbage(Stack : PValueDynArray; StkEnd : PValue; GlobalVars : PValueDynArray);

public
	constructor Create();
	destructor Destroy(); override;

	function GetString(Stack : PValueDynArray; StkEnd : PValue; GlobalVars : PValueDynArray) :  TStringValue;
	function GetArray() : TArrayValue;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TCallStackVariable - одна переменная стека вызовов }
TCallStackVariable = class
private
	FValueWrapper : TValueWrapper;
	FName : String;

public
	constructor Create(const Name : String; Values : PValueDynArray; ValueIndex : Integer; MM : TMemoryManager);
	destructor Destroy(); override;

	property Name : String read FName;
	property ValueWrapper : TValueWrapper read FValueWrapper;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TCallStackVariables - набор переменных или аргументов элемента стека вызовов }
TCallStackVariables = class
private
	FValues : TValueDynArray;
	FWrappers : TObjectList<TCallStackVariable>;

	function GetCount : Integer;
	function GetItem(Index : Integer) : TCallStackVariable;

public
	constructor Create(Names : TStrings; Values : TValueDynArray; MM : TMemoryManager);
	destructor Destroy(); override;

	property Count : Integer read GetCount;
	property Items[Index : Integer] : TCallStackVariable read GetItem; default;

	function GetEnumerator() : TList<TCallStackVariable>.TEnumerator;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TCallStackItem - один элемент стека вызовов }
TCallStackItem = class
private
	FFun : TFunDebugInfo;
	FArgs : TCallStackVariables;
	FVars : TCallStackVariables;

public
	constructor Create(Fun : TFunDebugInfo; Args : TValueDynArray; Vars : TValueDynArray; MM : TMemoryManager);
	destructor Destroy(); override;

	property Fun : TFunDebugInfo read FFun;
	property Args : TCallStackVariables read FArgs;
	property Vars : TCallStackVariables read FVars;

	function GetFunAndArgsString() : String;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TCallStackItems - стек вызовов }
TCallStackItems = class
private
	FItems : TObjectList<TCallStackItem>;

public
	constructor Create();
	destructor Destroy(); override;

	procedure Add(Item : TCallStackItem);
	procedure ToStrings(Strings : TStrings);
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TVirtualMachine - виртуальная машина для выполнения программы }
TVirtualMachine = class
private
	FStack : TValueDynArray;
	FArgs : TUserFunctionArgs;
	FAcc : TValueDynArray;
	FAccWrapper : TValueWrapper;
	FErrorPos : Integer;

	FStoredCodePtr : PBinaryCodeItem;
	FStoredStackPtr : PValue;
	FStoredLocPtr : PValueArray;

	class procedure UnaryOperException(const Value : TValue; Oper : PChar);
	class procedure BinOperException(const Value1, Value2 : TValue; Oper : PChar);
	class procedure ZeroDivException();
	class procedure WrongTypeException(const Value : TValue; ExpectedType : PChar);
	class procedure UnknownInstrError(InstrCode : Integer);
	class procedure PushArrItemIntoStack(var Stack : PValue; const Variable : TValue; ArgCount : Integer; var Indexes : TIntegerDynArray);
	class procedure PopArrItemFromStack(var Stack : PValue; var Variable : TValue; ArgCount : Integer; MM : TMemoryManager; var Indexes : TIntegerDynArray);

	procedure IncreaseStack(var Stk, StackEnd : PValue; var Loc : PValueArray);
	procedure CallUserFun(var Stk : PValue; UserFuncs : TUserFunctions; var CodePtr : PBinaryCodeItem; MM : TMemoryManager);
	procedure AddStrings(Stk, Stk2, StkEnd : PValue; UserVarValues : PValueDynArray; MM : TMemoryManager);

public
	constructor Create();
	destructor Destroy(); override;

	procedure Execute(BinCode : TBinaryCode; var UserVariables : TValueDynArray; UserFuncs : TUserFunctions; MM : TMemoryManager; ContinueExec : Boolean);

	procedure ClearStoredState();

	function GetStk() : PValueDynArray;
	procedure GetCallStack(DebugInfo : TDebugInfo; MM : TMemoryManager; Result : TCallStackItems);

	property ErrorPos : Integer read FErrorPos;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ TInterpreter - Интерпретатор }
TInterpreter = class
private
	FUserFunctions : TUserFunctions;
	FVirtualMachine : TVirtualMachine;
	FBinaryCode : TBinaryCode;
	FUserVariables : TUserVariables;
	FUserVarValues : TValueDynArray;
	FUserConsts : TUserConsts;
	FMemoryManager : TMemoryManager;
	FDebugInfo : TDebugInfo;
	FResult : TValueWrapper;
	FErrorCol : Integer;
	FErrorLine : Integer;

public
	constructor Create();
	destructor Destroy(); override;

	// Ф-ции компиляции
	procedure CompileScript(const Source : String);
	procedure CompileScriptFile(const FileName : String);
	procedure CompileExpression(const Source : String);

	// Ф-ции запуска
	procedure Execute(Continue : Boolean = false);

	// Пользовательские функции
	property UserFunctions : TUserFunctions read FUserFunctions;

	// Пользователские переменные
	property UserVariables : TUserVariables read FUserVariables;

	// Пользователские константы
	property UserConsts : TUserConsts read FUserConsts;

	// Результат выполнения
	property Result : TValueWrapper read FResult;

	// Отладчик
	procedure AddBreakpoint(LineNum : Integer);
	procedure RemoveBreakpoint(LineNum : Integer);
	procedure GetCallStack(Result : TCallStackItems);
	procedure Disassemble(Text : TStrings);

	// Работа с кодом
	procedure SaveToFile(const FileName : String);
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

resourcestring
	lngUnclosedComment     = 'Незакрытый комментарий';
	lngCantTranslToStr     = 'Не могу перевести значение в строку. Неизвестный тип значения: %u';
	lngCantTranslToInt     = 'Не могу привести значение %s к типу Integer';
	lngCantTranslToSingle  = 'Не могу привести значение %s к типу Single';
	lngCantTranslToBool    = 'Не могу привести значение %s к типу Boolean';
	lngIndexExceedRange    = 'Выход индекса за пределы index=%d, size=%d';
	lngFuncAlreadyReg      = 'Функция %s уже зарегестрирована';
	lngVarAlreadyReg       = 'Переменная %s уже зарегистрирована';
	lngTryToAccAfterLastT  = 'Попытка обращения за последний токен';
	lngExpSButFoundEOF     = 'Ожидается "%s", но получен конец текста';
	lngUnexpectedEOF       = 'Неожиданный конец текста';
	lngExpSButFoundS       = 'Ожидается "%s", но получено "%s"';
	lngExpEOFButFoundS     = 'Ожидается конец скрипта, а найден %s';
	lngExpExprEndButFoundS = 'Ожидается конец выражения, а найден %s';
	lngMainFuncNotFound    = 'Главная функция не найдена';
	lngCantFindOperPrio    = 'Не могу выяснить приоритет операции для %s';
	lngWrongFuncArgS       = 'Недопустимый аргумент функции "%s"';
	lngWrongFuncNameS      = 'Недопустимое имя функции "%s"';
	lngWrongArgCntForS     = 'Неправильное количество аргументов у %s';
	lngUnexpConstInExpr    = 'Непредвиденное появление константы %s в выражении';
	lngUnexpBracketInExpr  = 'Непредвиденное появление скобки ( в выражении';
	lngUnexpSInExpr        = 'Непредвиденное появление %s в выражении';
	lngImpToUseBOperInExpr = 'Нельзя использовать оператор %s в унарном выражении';
	lngStartForValNotSet   = 'Не задано начальное значение цикла';
	lngWrongOperInForStart = 'Неправильный оператор в начальном условии цикла: %s';
	lngSIsNotOper          = '%s - это не оператор';
	lngWrongOper           = 'Неправильный оператор: %s';
	lngLabSNotFound        = 'Метка %s не найдена';
	lngDontKnowHTGenForS   = 'Не знаю как генерировать код для %s';
	lngImpToChangeForVar   = 'Нельзя изменять переменную цикла %s внутри цикла';
	lngCantGenAssgiForS    = 'Не могу генерировать присваивание для %s';
	lngOpenSInOutsdCycle   = 'Оператор %s применён за пределами цикла';
	lngCantGenExprForS     = 'Не могу генерировать выражение для %s';
	lngSIsNotFunc          = '%s - не функция';
	lngWrongCntOfArgs      = 'Количество аргументов функции (%u) не совпадает (%u) для вызова %s';
	lngCantGenUnExptForS   = 'Не знаю как генерировать унарное выражение для %s';
	lngCantGenBinExptForS  = 'Не знаю как генерировать бинарное выражение для %s';
	lngCantGenConstS       = 'Не знаю как генерировать константу %s';
	lngCantCalcSSS         = 'Невозможно вычислить "%s %s %s"';
	lngCantCalcSS          = 'Невозможно вычислить "%s %s"';
	lngWrongTypeExpSForS   = 'Неверный тип. Ожидается тип %s для значения %s';
	lngUnknownInstr        = 'Неизвестная инструкция: %u';
	lngZeroDiv             = 'Деление на ноль';
	lngNoIntArrInd         = 'Индекс массива должен быть только целым числом';
	lngVarInNotArr         = 'Переменная не содержит массив';
	lngArrItemNotFnd       = 'Не найден элемент массива с указанным индексом';
	lngCantContNoConext    = 'Не могу продолжить, т.к. нет сохраннённого контекста';
	lngStackCorrupted      = 'Стек разрушен';
	lngCodeForLineNF       = 'Не найден код для строки %d';
	lngBPNotFoundAtLine    = 'Точка останова не найдена в строке %d';

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

implementation

uses Math, ZLib;

type
	TVisitProc = reference to procedure(var AST : TASTNode);

const
	strMult      = '*';
	strDiv       = '/';
	strPlus      = '+';
	strMinus     = '-';
	strPower     = '^';
	strAssig     = ':=';
	strOpBracket = '(';
	strClBracket = ')';
	strSqOpBracket = '[';
	strSqClBracket = ']';
	strGreaterEq = '>=';
	strGreater   = '>';
	strLessEq    = '<=';
	strNotEq     = '<>';
	strLess      = '<';
	strEquals    = '=';
	strSemi      = ';';
	strColon     = ':';
	strComma     = ',';
	strDot       = '.';
	strProcedure = 'procedure';
	strFunction  = 'function';
	strTrue      = 'true';
	strFalse     = 'false';
	strBegin     = 'begin';
	strEnd       = 'end';
	strExit      = 'exit';
	strFor       = 'for';
	strWhile     = 'while';
	strIf        = 'if';
	strRepeat    = 'repeat';
	strBreak     = 'break';
	strCont      = 'continue';
	strCase      = 'case';
	strOf        = 'of';
	strInc       = 'inc';
	strDec       = 'dec';
	strDo        = 'do';
	strTo        = 'to';
	strDownto    = 'downto';
	strThen      = 'then';
	strElse      = 'else';
	strUntil     = 'until';
	strIDiv      = 'div';
	strShl       = 'shl';
	strShr       = 'shr';
	strMod       = 'mod';
	strNot       = 'not';
	strOr        = 'or';
	strAnd       = 'and';
	strXor       = 'xor';
	strResult    = 'result';
	strNil       = 'nil';
	strTry       = 'try';
	strFinally   = 'finally';

	strEOF        = '<конец текста>';
	strBoolean    = 'boolean';
	strSeq        = '$seq$';
	strContLabel  = '$cont_';
	strBreakLabel = '$break_';
	strFuncEnd    = '$func_end_';
	strMainFunc   = '$main_func';
	strFinallyBeg = '$fin_beg';

	NilVal : TValue = (Typ : vtNil; Int : 0);

	NameChars =
		'_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
		'абвгдеёжзийклмнопрстуфхцчшщъыьэюя'+
		'АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ';

	DigChars           = '0123456789';
	SingleSymbolChars  = ';,.+-*\/()[]^=';
	ComplexSimbolChars = ':><';

	cArgsOffset = 4;

procedure VisitAll(var AST : TASTNode; Proc : TVisitProc);
var
	i : Integer;
	AstItem : TASTNode;
begin
	if AST = nil then Exit;

	AstItem := AST;
	Proc(AstItem);
	if AstItem <> AST then AST := AstItem;

	for i := 0 to AST.Count-1 do begin
		AstItem := AST[i];
		VisitAll(AstItem, Proc);
		if AstItem <> AST[i] then AST[i] := AstItem;
	end;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// record TValue

function TValue.ToString : String;
begin
	case Typ of
		vtNil:
			Result := strNil;

		vtInt:
			Result := IntToStr(Int);

		vtFloat:
			Result := FloatToStr(Float);

		vtString:
			Result := Str.Value;

		vtConstStr:
			Result := CStr^;

		vtBool: begin
			if Bool then Result := strTrue
			else Result := strFalse;
		end;

		vtArray:
			Result := Format('Array %p', [Arr]);
	else
		raise TRuntimeException.CreateFmt(lngCantTranslToStr, [Cardinal(Typ)]);
	end;
end;

function TValue.ToInt : Integer;
begin
	case Typ of
		vtInt:
			Result := Int;

		vtFloat:
			Result := Round(Float);

		vtBool: begin
			if Bool then Result := 1
			else Result := 0;
		end;

		vtString, vtConstStr: begin
			try
				Result := StrToInt(ToString);
			except
				raise TInterpreterException.CreateFmt(lngCantTranslToInt, [ToString]);
			end;
		end;
	else
		raise TTypeCastException.CreateFmt(lngCantTranslToInt, [ToString]);
	end;
end;

function TValue.ToFloat : TFloatType;
begin
	case Typ of
		vtInt:
			Result := Int;

		vtFloat:
			Result := Float;

		vtBool: begin
			if Bool then Result := 1
			else Result := 0;
		end;

		vtString, vtConstStr: begin
			try
				Result := StrToFloat(ToString);
			except
				raise TTypeCastException.CreateFmt(lngCantTranslToSingle, [ToString]);
			end;
		end;
	else
		raise TTypeCastException.CreateFmt(lngCantTranslToSingle, [ToString]);
	end;
end;

function TValue.ToBool : Boolean;
begin
	case Typ of
		vtBool: Result := Bool;
		vtInt: Result := Int <> 0;
	else
		raise TTypeCastException.CreateFmt(lngCantTranslToBool, [ToString]);
	end;
end;

function TValue.GetChar(Index : Integer) : TValue;
begin
	case Typ of
		vtString: begin
			Result.Int := Integer(Str.Value[Index]);
			Result.Typ := vtInt;
		end;

		vtConstStr: begin
			if (Index <= 0) or (Index >= length(CStr^)) then
				Result.Typ := vtNil
			else begin
				Result.Int := Integer((CStr^)[Index]);
				Result.Typ := vtInt;
			end;
		end;
	else
		Result.Typ := vtNil;
	end;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

constructor TValueWrapper.Create(Values : PValueDynArray; Index : Integer; MM : TMemoryManager);
begin
	FValues := Values;
	FIndex := Index;
	FMM := MM;
end;

function TValueWrapper.GetValue() : TValue;
begin
	Result := (FValues^)[FIndex];
end;

function TValueWrapper.GetType() : TValueType;
begin
	Result := Value.Typ;
end;

function TValueWrapper.ToStr() : String;
begin
	Result := Value.ToString;
end;

function TValueWrapper.ToInt() : Integer;
begin
	Result := Value.ToInt;
end;

function TValueWrapper.ToFloat() : Single;
begin
	Result := Value.ToFloat;
end;

function TValueWrapper.ToBool() : Boolean;
begin
	Result := Value.ToBool;
end;

function TValueWrapper.TypeIsNil() : Boolean;
begin
	Result := Value.Typ = vtNil;
end;

procedure TValueWrapper.SetInt(Value : Integer);
var
	ValPtr : PValue;
begin
	ValPtr := @(FValues^)[FIndex];
	ValPtr.Typ := vtInt;
	ValPtr.Int := Value;
end;

procedure TValueWrapper.SetFloat(Value : Single);
var
	ValPtr : PValue;
begin
	ValPtr := @(FValues^)[FIndex];
	ValPtr.Typ := vtFloat;
	ValPtr.Float := Value;
end;

procedure TValueWrapper.SetBool(Value : Boolean);
var
	ValPtr : PValue;
begin
	ValPtr := @(FValues^)[FIndex];
	ValPtr.Typ := vtBool;
	ValPtr.Bool := Value;
end;

procedure TValueWrapper.SetString(const Value : String);
var
	ValPtr : PValue;
begin
	ValPtr := @(FValues^)[FIndex];
	if ValPtr.Typ<>vtString then begin
		ValPtr.Str := FMM.GetString(nil, nil, nil);
		ValPtr.Typ := vtString;
	end;
	ValPtr.Str.Value := Value;
end;

procedure TValueWrapper.SetNil;
begin
	(FValues^)[FIndex].Typ := vtNil;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TUserFunctionArgs }

constructor TUserFunctionArgs.Create();
begin
	FSize := 0;
end;

destructor TUserFunctionArgs.Destroy();
var
	Wrapper : TValueWrapper;
begin
	for Wrapper in FItems do Wrapper.Free;
	Inherited;
end;

procedure TUserFunctionArgs.Clear();
begin
	FSize := 0;
end;

function TUserFunctionArgs.Add(Values : PValueDynArray; Index : Integer; MM : TMemoryManager) : TValueWrapper;
begin
	if FSize >= Length(FItems) then begin
		Result := TValueWrapper.Create(Values, Index, MM);
		SetLength(FItems, FSize+1);
		FItems[FSize] := Result;
	end
	else begin
		Result := FItems[FSize];
		Result.FValues := Values;
		Result.FIndex := Index;
	end;
	Inc(FSize);
end;

class procedure TUserFunctionArgs.RaiseErrIndexExceedRange(Index, Size : Integer);
begin
	raise TRuntimeException.CreateFmt(lngIndexExceedRange, [Index, Size]);
end;

function TUserFunctionArgs.GetItem(Index : Integer) : TValueWrapper;
begin
	if (Index < 0) or (Index >= FSize) then RaiseErrIndexExceedRange(Index, FSize);

		
	Result := FItems[Index];
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TUserFunction }

constructor TUserFunction.Create(
	Index      : Integer;
	const Name : String;
	ArgCount   : Integer;
	Context    : Pointer;
	Callback   : TInterpreterFuncCallBack
);
begin
	FIndex    := Index;
	FName     := Name;
	FArgCount := ArgCount;
	FContext  := Context;
	FCallback := Callback;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TUserFunctions }

constructor TUserFunctions.Create();
begin
	FItems := TObjectList<TUserFunction>.Create();
	RegisterStd();
end;

destructor TUserFunctions.Destroy();
begin
	FreeAndNil(FItems);
	Inherited;
end;

function TUserFunctions.GetCount() : Integer;
begin
	Result := FItems.Count;
end;

function TUserFunctions.GetItem(Index : Integer) : TUserFunction;
begin
	Result := FItems.Items[Index];
end;

function TUserFunctions.FindByName(const Name : String) : TUserFunction;
var
	Index : Integer;
begin
	Index := IndexByName(Name);
	if Index <> -1 then
		Result := Items[Index]
	else
		Result := nil;
end;

function TUserFunctions.IndexByName(const Name : String) : Integer;
begin
	for Result := 0 to FItems.Count-1 do
		if AnsiCompareText(FItems[Result].Name, Name) = 0 then Exit;
	Result := -1;
end;

function TUserFunctions.Add(
	const Name : String;
	ArgCount   : Integer;
	Context    : Pointer;
	Callback   : TInterpreterFuncCallBack
) : Integer;
var
	Func : TUserFunction;
begin
	Func := FindByName(Name);
	if Func <> nil then
		raise TInterpreterException.CreateFmt(lngFuncAlreadyReg, [Name]);

	Result := FItems.Count;
	Func := TUserFunction.Create(Result, Name, ArgCount, Context, Callback);
	FItems.Add(Func);
end;

procedure TUserFunctions.Clear();
begin
	FItems.Clear();
end;

procedure TUserFunctions.RegisterStd();
begin
	Add('IntToStr', 1, nil,
		procedure(Context : Pointer; Args : TUserFunctionArgs; Ret : TValueWrapper)
		begin Ret.StrValue := IntToStr(Args[0].IntValue); end
	);

	Add('StrToInt', 1, nil,
		procedure(Context : Pointer; Args : TUserFunctionArgs; Ret : TValueWrapper)
		begin
			try
				Ret.IntValue := StrToInt(Args[0].StrValue);
			except
				Ret.SetNil;
			end;
		end
	);

	Add('FloatToStr', 1, nil,
		procedure(Context : Pointer; Args : TUserFunctionArgs; Ret : TValueWrapper)
		begin Ret.StrValue := FloatToStr(Args[0].FloatValue); end
	);

	Add('StrToFloat', 1, nil,
		procedure(Context : Pointer; Args : TUserFunctionArgs; Ret : TValueWrapper)
		begin
			try
				Ret.FloatValue := StrToFloat(Args[0].StrValue);
			except
				Ret.SetNil;
			end;
		end
	);

	Add('Sin', 1, nil,
		procedure(Context : Pointer; Args : TUserFunctionArgs; Ret : TValueWrapper)
		begin Ret.FloatValue := Sin(Args[0].FloatValue); end
	);

	Add('Cos', 1, nil,
		procedure(Context : Pointer; Args : TUserFunctionArgs; Ret : TValueWrapper)
		begin Ret.FloatValue := Cos(Args[0].FloatValue); end
	);

	Add('Tan', 1, nil,
		procedure(Context : Pointer; Args : TUserFunctionArgs; Ret : TValueWrapper)
		begin Ret.FloatValue := Tan(Args[0].FloatValue); end
	);

	Add('Pos', 2, nil,
		procedure(Context : Pointer; Args : TUserFunctionArgs; Ret : TValueWrapper)
		begin Ret.IntValue := Pos(Args[0].StrValue, Args[1].StrValue); end
	);

	Add('Length', 1, nil,
		procedure(Context : Pointer; Args : TUserFunctionArgs; Ret : TValueWrapper)
		begin
			case Args[0].ValueType of
				vtString, vtConstStr:
					Ret.IntValue := Length(Args[0].StrValue);

				vtArray:
					Ret.IntValue := Args[0].Value.Arr.Count;
			else
				Ret.SetNil; // TODO: Сдедать исключение!
			end;
		end
	);
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

constructor TUserVariables.Create(ValuesArray : PValueDynArray; MM : TMemoryManager);
begin
	FMM := MM;
	FValuesArray := ValuesArray;
	FVarNames := TStringList.Create();
end;

destructor TUserVariables.Destroy();
begin
	Clear();
	FreeAndNil(FVarNames);
	Inherited;
end;

function TUserVariables.Add(const Name : string) : TValueWrapper;
var
	NameLC : String;
	Index : Integer;
	NewSize : Integer;
	OldLen : Integer;
begin
	NameLC := AnsiLowerCase(Trim(Name));
	Index := FVarNames.IndexOf(NameLC);
	if Index<>-1 then
		raise TInterpreterException.CreateFmt(lngVarAlreadyReg, [Name]);

	NewSize := FVarNames.Count+1;
	OldLen := length(FValuesArray^);
	if OldLen < NewSize then SetLength(FValuesArray^, 2*NewSize);

	Index := NewSize-1;

	(FValuesArray^)[Index] := NilVal;

	Result := TValueWrapper.Create(FValuesArray, Index, FMM);
	FVarNames.AddObject(NameLC, Result);
end;

function TUserVariables.GetIndex(const Name : string) : Integer;
begin
	Result := FVarNames.IndexOf(Name);
end;

procedure TUserVariables.Clear();
var
	i : Integer;
begin
	for i := 0 to FVarNames.Count-1 do
		FVarNames.Objects[i].Free;

	FVarNames.Clear();
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TUserConsts }

constructor TUserConsts.Create();
begin
	FIntItems := TDictionary<string, Integer>.Create;
	FSingleItems := TDictionary<String, Single>.Create;
	FStrItems := TDictionary<String, String>.Create;
end;

destructor TUserConsts.Destroy();
begin
	FreeAndNil(FStrItems);
	FreeAndNil(FSingleItems);
	FreeAndNil(FIntItems);
	Inherited;
end;

procedure TUserConsts.Clear();
begin
	FStrItems.Clear();
	FSingleItems.Clear();
	FIntItems.Clear();
end;

procedure TUserConsts.AddStr(const Name, Value : String);
begin
	FStrItems.Add(LowerCase(Name), Value);
end;

procedure TUserConsts.AddInt(const Name : String; Value : Integer);
begin
	FIntItems.Add(LowerCase(Name), Value);
end;

procedure TUserConsts.AddSingle(const Name : String; Value : Single);
begin
	FSingleItems.Add(LowerCase(Name), Value);
end;

function TUserConsts.GetConstType(const Name : String) : TValueType;
var
	NameLC : String;
begin
	Result := vtNil;
	NameLC := LowerCase(Name);
	if      FStrItems.   ContainsKey(NameLC) then Result := vtString
	else if FIntItems.   ContainsKey(NameLC) then Result := vtInt
	else if FSingleItems.ContainsKey(NameLC) then Result := vtFloat
end;

function TUserConsts.GetStr(const Name : String) : String;
begin
	Result := FStrItems[LowerCase(Name)];
end;

function TUserConsts.GetInt(const Name : String) : Integer;
begin
	Result := FIntItems[LowerCase(Name)];
end;

function TUserConsts.GetSingle(const Name : String) : Single;
begin
	Result := FSingleItems[LowerCase(Name)];
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TToken }

constructor TToken.Create(const Text : string; Flags : TTokenFlags; Col, Line : Integer; TType : TTokenType);
begin
	Init(Flags, Col, Line, TType);
	FText  := Text;
end;

constructor TToken.CreateInt(Value : Integer; Col, Line : Integer);
begin
	Init([ltExprContent, ltConst], Col, Line, ttIntConst);
	FIntConst := Value;
end;

constructor TToken.CreateFloat(Value : Single; Col, Line : Integer);
begin
	Init([ltExprContent, ltConst], Col, Line, ttFloatConst);
	FFloatConst := Value;
end;

constructor TToken.CreateBool(Value : Boolean; Col, Line : Integer);
begin
	Init([ltExprContent, ltConst], Col, Line, ttBoolConst);
	FBoolConst := Value;
end;

constructor TToken.CreateStr(const Text : String; Col : Integer = -1; Line : Integer = -1);
begin
	Init([ltExprContent, ltConst], Col, Line, ttStrConst);
	FText := Text;
end;

procedure TToken.Init(Flags : TTokenFlags; Col, Line : Integer; TType : TTokenType);
begin
	FFlags      := Flags;
	FType       := TType;
	FLine       := Line;
	FCol        := Col;
	FIntConst   := 0;
	FFloatConst := 0;
	FBoolConst  := false;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

constructor TTokenizerException.CreateFmt(const Text : String; const Args: array of const; Col, Line : Integer);
begin
	Inherited CreateFmt(Text, Args);
	FCol := Col;
	FLine := Line;
end;


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TTokenizer }

constructor TTokenizer.Create();
begin
	FItems := TObjectList<TToken>.Create();
	FPos := 0;

	FormatSettings.DecimalSeparator := '.';

	FTokTypesByName := TDictionary<String, TTokenType>.Create();

	FTokTypesByName.Add(strEOF,         ttEOF        );
	FTokTypesByName.Add(strAssig,       ttAssig      );
	FTokTypesByName.Add(strSeq,         ttSeq        );
	FTokTypesByName.Add(strProcedure,   ttProcedure  );
	FTokTypesByName.Add(strFunction,    ttFunction   );
	FTokTypesByName.Add(strBoolean,     ttBoolean    );
	FTokTypesByName.Add(strTrue,        ttTrue       );
	FTokTypesByName.Add(strFalse,       ttFalse      );
	FTokTypesByName.Add(strBegin,       ttBegin      );
	FTokTypesByName.Add(strEnd,         ttEnd        );
	FTokTypesByName.Add(strExit,        ttExit       );
	FTokTypesByName.Add(strFor,         ttFor        );
	FTokTypesByName.Add(strWhile,       ttWhile      );
	FTokTypesByName.Add(strIf,          ttIf         );
	FTokTypesByName.Add(strRepeat,      ttRepeat     );
	FTokTypesByName.Add(strBreak,       ttBreak      );
	FTokTypesByName.Add(strCont,        ttCont       );
	FTokTypesByName.Add(strCase,        ttCase       );
	FTokTypesByName.Add(strInc,         ttInc        );
	FTokTypesByName.Add(strDec,         ttDec        );
	FTokTypesByName.Add(strDo,          ttDo         );
	FTokTypesByName.Add(strTo,          ttTo         );
	FTokTypesByName.Add(strDownto,      ttDownto     );
	FTokTypesByName.Add(strThen,        ttThen       );
	FTokTypesByName.Add(strElse,        ttElse       );
	FTokTypesByName.Add(strUntil,       ttUntil      );
	FTokTypesByName.Add(strOf,          ttOf         );
	FTokTypesByName.Add(strIDiv,        ttIDiv       );
	FTokTypesByName.Add(strShl,         ttShl        );
	FTokTypesByName.Add(strShr,         ttShr        );
	FTokTypesByName.Add(strMod,         ttMod        );
	FTokTypesByName.Add(strNot,         ttNot        );
	FTokTypesByName.Add(strOr,          ttOr         );
	FTokTypesByName.Add(strAnd,         ttAnd        );
	FTokTypesByName.Add(strXor,         ttXor        );
	FTokTypesByName.Add(strNil,         ttNil        );
	FTokTypesByName.Add(strClBracket,   ttClBracket  );
	FTokTypesByName.Add(strOpBracket,   ttOpBracket  );
	FTokTypesByName.Add(strSqOpBracket, ttSqOpBracket);
	FTokTypesByName.Add(strSqClBracket, ttSqClBracket);
	FTokTypesByName.Add(strSemi,        ttSemi       );
	FTokTypesByName.Add(strColon,       ttColon      );
	FTokTypesByName.Add(strPlus,        ttPlus       );
	FTokTypesByName.Add(strMinus,       ttMinus      );
	FTokTypesByName.Add(strLess,        ttLess       );
	FTokTypesByName.Add(strLessEq,      ttLessEq     );
	FTokTypesByName.Add(strGreater,     ttGreater    );
	FTokTypesByName.Add(strGreaterEq,   ttGreaterEq  );
	FTokTypesByName.Add(strEquals,      ttEquals     );
	FTokTypesByName.Add(strNotEq,       ttNotEq      );
	FTokTypesByName.Add(strMult,        ttMult       );
	FTokTypesByName.Add(strDiv,         ttDiv        );
	FTokTypesByName.Add(strPower,       ttPower      );
	FTokTypesByName.Add(strComma,       ttComma      );
	FTokTypesByName.Add(strDot,         ttDot        );
	FTokTypesByName.Add(strTry,         ttTry        );
	FTokTypesByName.Add(strFinally,     ttFinally    );
end;

destructor TTokenizer.Destroy();
begin
	FreeAndNil(FTokTypesByName);
	FreeAndNil(FItems);
	Inherited;
end;

function TTokenizer.GetTokenStrByType(TT : TTokenType) : string;
var
	Key : String;
begin
	Result := '';
	if not FTokTypesByName.ContainsValue(TT) then Exit;

	for Key in FTokTypesByName.Keys do
		if FTokTypesByName[Key] = TT then begin
			Result := Key;
			Exit;
		end;
end;

function TTokenizer.GetTokenStrByTypes(const TTs : array of TTokenType) : string;
var
	TT : TTokenType;
begin
	Result := '';
	for TT in TTs do begin
		if Result <> '' then Result := Result + ', ';
		Result := Result + GetTokenStrByType(TT);
	end;
end;

function TTokenizer.GetTypeByText(const Name : String) : TTokenType;
var
	Found : Boolean;
begin
	Found := FTokTypesByName.TryGetValue(Name, Result);
	if not Found then Result := ttUnknown;
end;

function TTokenizer.GetItem(Index : Integer) : TToken;
begin
	Result := FItems[Index];
end;

function TTokenizer.GetCount() : Integer;
begin
	Result := FItems.Count;
end;

function TTokenizer.EOF : Boolean;
begin
	Result := Pos >= Count;
end;

function TTokenizer.AddToken(
	const Text : string;
	Col        : Integer;
	Line       : Integer;
	lc         : boolean;
	Flags      : TTokenFlags;
	TokenType  : TTokenType
) : TToken;
var
	TextToAdd : string;
begin
	Result := nil;
	if (Text = '') and (Flags = []) then Exit;
	if lc then TextToAdd := AnsiLowerCase(Text)
	else TextToAdd := Text;

	if TokenType = ttUnknown then TokenType := GetTypeByText(TextToAdd);
	if Flags = [] then Flags := GetTokenFlags(TokenType);

	if TokenType in [ttTrue, ttFalse] then
		Result := TToken.CreateBool(StrToBool(TextToAdd), Col, Line)
	else
		Result := TToken.Create(TextToAdd, Flags, Col, Line, TokenType);

	FItems.Add(Result);
end;

function IsNameChar(C : Char) : boolean;
begin
	Result := Pos(C, NameChars) <> 0;
end;

function IsDigitChar(C : Char) : boolean;
begin
	Result := Pos(C, DigChars) <> 0;
end;

function IsSingleSimbol(C : Char) : boolean;
begin
	Result := Pos(C, SingleSymbolChars) <> 0;
end;

function IsComplexSimbol(C : Char) : boolean;
begin
	Result := Pos(C, ComplexSimbolChars) <> 0;
end;

procedure TTokenizer.Tockenize(const Text : string; Consts : TUserConsts);
var
	Cur : TTokenCursor;
	Len : Integer;
	C : Char;
begin
	FItems.Clear();

	Cur.Pos := 1;
	Cur.Col := 1;
	Cur.Line := 1;

	Len := Length(Text);
	while Cur.Pos <= Len do begin
		C := Text[Cur.Pos];
		if IsNameChar(C) then
			Cur := TockenizeName(Text, Cur, Len)

		else if IsDigitChar(C) then
			Cur := TockenizeValue(Text, Cur, Len)

		else if (C = '/') and (Cur.Pos+1 <= Len) and (Text[Cur.Pos+1] = '/') then
			Cur := SkipSingleComment(Text, Cur, Len)

		else if (C = '(') and (Cur.Pos+1 <= Len) and (Text[Cur.Pos+1] = '*') then
			Cur := SkipMultiComment(Text, Cur, Len, true)

		else if C = '{' then
			Cur := SkipMultiComment(Text, Cur, Len, false)

		else if C = '''' then
			Cur := TockenizeString(Text, Cur, Len)

		else if IsComplexSimbol(C) then
			Cur := TockenizeComplSym(Text, Cur, Len)

		else if IsSingleSimbol(C) then begin
			AddToken(C, Cur.Col, Cur.Line, false);
			Inc(Cur.Pos);
			Inc(Cur.Col);
		end
		else if C = #10 then begin
			Inc(Cur.Pos);
			Inc(Cur.Line);
			Cur.Col := 1;
		end
		else begin
			Inc(Cur.Pos);
			Inc(Cur.Col);
        end;
	end;
	AddToken(strEOF, Cur.Col, Cur.Line, false, [ltEndOftext]);
end;

function TTokenizer.TockenizeName(const Text : string; const Cur : TTokenCursor; Len : Integer) : TTokenCursor;
var
	Token : String;
	TokenLen : Integer;
	C : Char;
begin
	Result := Cur;
	while Result.Pos <= Len do begin
		C := Text[Result.Pos];
		if (not IsNameChar(C)) and (not IsDigitChar(C)) then break;
		Inc(Result.Pos);
	end;

	TokenLen := Result.Pos - Cur.Pos;
	Result.Col := Result.Col + TokenLen;

	Token := Copy(Text, Cur.Pos, TokenLen);

	AddToken(Token, Cur.Col, Cur.Line, true);
end;

function TTokenizer.TockenizeValue(const Text : string; const Cur : TTokenCursor; Len : Integer) : TTokenCursor;
var
	TokenText : String;
	C : Char;
	IsFloat : Boolean;
	Token : TToken;
	TokenLen : Integer;
begin
	C := #0;

	Result := Cur;

	while Result.Pos <= Len do begin
		C := Text[Result.Pos];
		if (not IsDigitChar(C)) and (C <> '.') then Break;
		Inc(Result.Pos);
	end;

	if (C = 'e') or (C = 'E') then begin
		TokenText := TokenText + C;
		Inc(Result.Pos);

		if Result.Pos <= Len then begin
			C := Text[Result.Pos];
			if (C = strPlus) or (C = strMinus) then Inc(Result.Pos);
		end;

		while Result.Pos <= Len do begin
			if not IsDigitChar(Text[Result.Pos]) then Break;
			Inc(Result.Pos);
		end;
	end;

	TokenLen := Result.Pos - Cur.Pos;
	Result.Col := Result.Col + TokenLen;
	TokenText := Copy(Text, Cur.Pos, TokenLen);

	IsFloat :=
		(System.Pos('.', TokenText) <> 0) or
		(System.Pos('E', TokenText) <> 0) or
		(System.Pos('e', TokenText) <> 0);

	if IsFloat then
		Token := TToken.CreateFloat(StrToFloat(TokenText), Cur.Col, Cur.Line)
	else
		Token := TToken.CreateInt(StrToInt(TokenText), Cur.Col, Cur.Line);

	FItems.Add(Token);
end;

function TTokenizer.TockenizeComplSym(const Text : string; const Cur : TTokenCursor; Len : Integer) : TTokenCursor;
var
	Token : String;
begin
	Result := Cur;
	try
		if (Result.Pos+1) <= Len then begin
			Token := Text[Result.Pos] + Text[Result.Pos+1];
			if (Token = strAssig)
			or (Token = strLessEq)
			or (Token = strNotEq)
			or (Token = strGreaterEq) then begin
				Inc(Result.Pos);
				AddToken(Token, Cur.Col, Cur.Line, false);
				Exit;
			end;
		end;
		AddToken(Text[Result.Pos], Cur.Col, Cur.Line, false);
	finally
		Inc(Result.Pos);
		Inc(Result.Col, Result.Pos-Cur.Pos);
	end;
end;

function TTokenizer.TockenizeString(const Text : string; const Cur : TTokenCursor; Len : Integer) : TTokenCursor;
var
	TokenText : String;
	C : Char;
	Token : TToken;
begin
	Result := Cur;
	TokenText := '';
	Inc(Result.Pos);
	while Result.Pos <= Len do begin
		C := Text[Result.Pos];
		Inc(Result.Pos);
		if C = '''' then break;
		TokenText := TokenText + C;
	end;

	Token := TToken.Create(TokenText, [ltConst, ltExprContent], Cur.Col, Cur.Line, ttStrConst);
	FItems.Add(Token);

	Inc(Result.Col, Result.Pos-Cur.Pos);
end;

function TTokenizer.SkipSingleComment(const Text : string; const Cur : TTokenCursor; Len : Integer) : TTokenCursor;
begin
	Result := Cur;

	try
		while Result.Pos <= Len do begin
			if Text[Result.Pos] = #10 then begin
				Inc(Result.Pos);
				Exit;
			end;
			Inc(Result.Pos);
		end;
	finally
		Inc(Result.Line);
		Result.Col := 1;
	end;
end;

function TTokenizer.SkipMultiComment(const Text : string; const Cur : TTokenCursor; Len : Integer; Dbl : Boolean) : TTokenCursor;
begin
	Result := Cur;
	while Result.Pos <= Len do begin
		if Text[Result.Pos] = #10 then begin
			Inc(Result.Pos);
			Inc(Result.Line);
			Result.Col := 1;
		end
		else begin
			if ((not Dbl) and (Text[Result.Pos] = '}')) then
			begin
				Inc(Result.Pos);
				Inc(Result.Col);
				Exit;
			end
			else if (Dbl and (Text[Result.Pos] = '*') and (Result.Pos+1 <= Len) and (Text[Result.Pos+1] = ')'))
			then begin
				Inc(Result.Pos, 2);
				Inc(Result.Col, 2);
				Exit;
			end;
			Inc(Result.Pos);
			Inc(Result.Col);
        end;
	end;

	raise TTokenizerException.CreateFmt(lngUnclosedComment, [], Cur.Col, Cur.Line);
end;

function TTokenizer.GetTokenFlags(TokenType : TTokenType) : TTokenFlags;
begin
	case TokenType of
		ttSeq:
			Result := [ltSeq, ltStatement];

		ttOpBracket, ttClBracket,
		ttSqOpBracket, ttSqClBracket:
			Result := [ ltExprOperator ];

		ttDo, ttTo, ttDownto, ttThen, ttElse, ttUntil, ttOf:
			Result := [ltStatementSubWord, ltStatement];

		ttSemi:
			Result := [];

		ttProcedure, ttExit, ttFor, ttWhile, ttIf,
		ttRepeat, ttCase, ttAssig, ttInc, ttDec,
		ttBreak, ttCont, ttTry, ttFinally:
			Result := [ltStatementWord, ltStatement];

		ttMult, ttDiv, ttPlus, ttMinus, ttPower,
		ttIDiv, ttMod, ttAnd, ttXor, ttGreater,
		ttGreaterEq, ttLess, ttLessEq, ttEquals,
		ttNotEq, ttNot, ttOr, ttShl, ttShr:
			Result := [ltExprOperator, ltExprContent];

		ttNil, ttTrue, ttFalse:
			Result := [ltConst, ltExprContent];
	else
		Result := [ltName, ltExprContent];
	end;
end;

procedure TTokenizer.Start();
begin
	FPos := 0;
end;

function TTokenizer.Get(ExceptIfEnd : Boolean) : TToken;
begin
	if FPos >= Count then
		raise TInternalInterpreterException.Create(lngTryToAccAfterLastT);

	Result := Items[FPos];
	if (ltEndOfText in Result.Flags) and ExceptIfEnd then
		raise TParserException.Create(Result, lngUnexpectedEOF);
end;

procedure TTokenizer.Next();
begin
	Inc(FPos);
end;

function TTokenizer.Expect(TokenType : TTokenType) : TToken;
type
	Ptr = ^AnsiString;
begin
	if FPos >= FItems.Count then
		raise TParserException.CreateFmt(
			Items[Count-1],
			lngExpSButFoundEOF,
			[GetTokenStrByType(TokenType)]
		);

	Result := Items[FPos];

	if Result.TokenType <> TokenType then
		raise TParserException.CreateFmt(
			Result,
			lngExpSButFoundS,
			[GetTokenStrByType(TokenType), Result.Text]
		);

	Inc(FPos);
end;

function TTokenizer.ExpectAnyOf(const TokenTypes : array of TTokenType) : TToken;
var
	TT : TTokenType;
	Found : Boolean;
begin
	if FPos >= FItems.Count then
		raise TParserException.CreateFmt(
			Items[Count-1],
			lngExpSButFoundEOF,
			[GetTokenStrByTypes(TokenTypes)]
		);

	Result := Items[FPos];

	Found := false;
	for TT in TokenTypes do
		if TT = Result.TokenType then Found := true;

	if not Found then
		raise TParserException.CreateFmt(
			Result,
			lngExpSButFoundS,
			[GetTokenStrByTypes(TokenTypes), Result.Text]
		);

	Inc(FPos);
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TASTNode }

constructor TASTNode.CreateSpecial(Parser : TParser; Parent : TASTNode; TokenType : TTokenType; const Text : string);
begin
	Parser.FAllAstNodes.Add(self);
	FParser := Parser;
	FParent := Parent;
	FItems := TList<TASTNode>.Create();
	FToken := TToken.Create(Text, [], -1, -1, TokenType);
	FOwnerOfToken := true;
end;

constructor TASTNode.CreateSpecial(Parser : TParser; Parent : TASTNode; TokenType : TTokenType; const Text : string; Arg1 : TASTNode);
begin
	CreateSpecial(Parser, Parent, TokenType, Text);
	Add(Arg1);
end;

constructor TASTNode.CreateSpecial(Parser : TParser; Parent : TASTNode; TokenType : TTokenType; const Text : string; Arg1, Arg2 : TASTNode);
begin
	CreateSpecial(Parser, Parent, TokenType, Text, Arg1);
	Add(Arg2);
end;

constructor TASTNode.Create(Parser : TParser; Tocken : TToken; Parent : TASTNode);
begin
	Create(Parser, Tocken, Parent, false);
end;

constructor TASTNode.Create(Parser : TParser; Tocken : TToken; Parent : TASTNode; OwnerOfToken : Boolean);
begin
	Parser.FAllAstNodes.Add(self);
	FParser := Parser;
	FParent := Parent;
	FItems := TList<TASTNode>.Create();
	FToken := Tocken;
	FOwnerOfToken := OwnerOfToken;
end;

constructor TASTNode.Create(Parser : TParser; Tocken : TToken; Parent : TASTNode; Arg1 : TASTNode);
begin
	Create(Parser, Tocken, Parent);
	Add(Arg1);
end;

constructor TASTNode.Create(Parser : TParser; Tocken : TToken; Parent : TASTNode; Arg1, Arg2 : TASTNode);
begin
	Create(Parser, Tocken, Parent, Arg1);
	Add(Arg2);
end;

destructor TASTNode.Destroy();
begin
	if FOwnerOfToken then FreeAndNil(FToken);
	FreeAndNil(FItems);
	Inherited;
end;

procedure TASTNode.Add(Item : TASTNode);
begin
	FItems.Add(Item);
end;

function TASTNode.GetItem(Index : Integer) : TASTNode;
begin
	Result := FItems[Index];
end;

procedure TASTNode.SetItem(Index : Integer; Item : TASTNode);
begin
	FItems[Index] := Item;
end;

function TASTNode.GetCount() : Integer;
begin
	Result := FItems.Count;
end;

function TASTNode.GetEnumerable: TEnumerable<TASTNode>;
begin
	Result := FItems;
end;

function TASTNode.UniqStr() : string;
begin
	Result := Format('%p', [Pointer(self)]);
end;

procedure TASTNode.Swap(OtherAST : TASTNode);
var
	TmpItems : TList<TASTNode>;
	TmpToken : TToken;
	TmpParent : TASTNode;
	TmpOwnerOfToken : Boolean;
	TmpParser : TParser;
begin
	TmpItems := FItems;
	FItems := OtherAST.FItems;
	OtherAST.FItems := TmpItems;

	TmpToken := FToken;
	FToken := OtherAST.FToken;
	OtherAST.FToken := TmpToken;

	TmpParent := FParent;
	FParent := OtherAST.FParent;
	OtherAST.FParent := TmpParent;

	TmpOwnerOfToken := FOwnerOfToken;
	FOwnerOfToken := OtherAST.FOwnerOfToken;
	OtherAST.FOwnerOfToken := TmpOwnerOfToken;

	TmpParser := FParser;
	FParser := OtherAST.FParser;
	OtherAST.FParser := TmpParser;
end;

function TASTNode.IsArrayExpr() : Boolean;
begin
	Result := false;
	if Count < 2 then Exit;
	Result :=
			(Childs[0].Token.TokenType = ttSqOpBracket)
		and (Childs[Count-1].Token.TokenType = ttSqClBracket);
end;


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TScriptFunction }

constructor TScriptFunction.Create(Node : TASTNode; IsFunction : Boolean);
begin
	FArgList := TStringList.Create();
	FVars := TStringList.Create();
	FVars.Sorted := true;
	FVars.Duplicates := dupIgnore;
	FTopNode := Node;
	FIsFunction := IsFunction;
	ReadFuncInfo();
end;

destructor TScriptFunction.Destroy();
begin
	FreeAndNil(FVars);
	FreeAndNil(FArgList);
	Inherited;
end;

procedure TScriptFunction.ReadFuncInfo();
var
	i : Integer;
	ArgAst : TASTNode;
begin
	FName := FTopNode.Token.Text;
	for i := 0 to FTopNode.Count-2 do begin
		ArgAst := FTopNode[i];
		FArgList.Add(ArgAst.Token.Text);
	end;
	FBodyNode := FTopNode[FTopNode.Count-1];
end;

function TScriptFunction.GetArgByIndex(Index : Integer) : String;
begin
	Result := FArgList[Index];
end;

function TScriptFunction.GetVarByIndex(Index : Integer) : String;
begin
	Result := FVars[Index];
end;

procedure TScriptFunction.FindLocalVariables(
	UserVars    : TUserVariables;
	UserFuncs   : TUserFunctions;
	UserConsts  : TUserConsts;
	ScriptFuncs : TScriptFunctions
);
begin
	FVars.Clear();
	VisitAll(
		FBodyNode, 
		procedure(var AST : TASTNode) 
		begin
			if not (ltName in AST.Token.Flags) then Exit;
		
			if AST.IsArrayExpr 
			and (ArgIndex(AST.Token.Text) = -1) then
				FVars.Add(AST.Token.Text)
				
			else if (UserVars.GetIndex(AST.Token.Text) = -1)
			and (UserFuncs.IndexByName(AST.Token.Text) = -1)
			and (ScriptFuncs.FindByName(AST.Token.Text) = nil)
			and (ArgIndex(AST.Token.Text) = -1)
			and (UserConsts.GetConstType(AST.Token.Text) = vtNil)
			then FVars.Add(AST.Token.Text)
		end
	);
end;

function TScriptFunction.VariablesCount : Integer;
begin
	Result := FVars.Count;
end;

function TScriptFunction.VarIndex(const VarName : string) : Integer;
begin
	Result := FVars.IndexOf(VarName);
end;

function TScriptFunction.ArgsCount() : Integer;
begin
	Result := FArgList.Count;
end;

function TScriptFunction.ArgIndex(const ArgName : string) : Integer;
begin
	Result := FArgList.IndexOf(ArgName);
end;


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TScriptFunctions }

constructor TScriptFunctions.Create();
begin
	FItems := TObjectList<TScriptFunction>.Create();
end;

destructor TScriptFunctions.Destroy();
begin
	FreeAndNil(FItems);
	Inherited;
end;

procedure TScriptFunctions.Clear();
begin
	FItems.Clear();
end;

procedure TScriptFunctions.Add(Func : TScriptFunction);
begin
	FItems.Add(Func);
end;

function TScriptFunctions.FindByName(const Name : String) : TScriptFunction;
begin
	for Result in FItems do if Result.Name = Name then Exit;
	Result := nil;
end;

function TScriptFunctions.GetCount() : Integer;
begin
	Result := FItems.Count;
end;

function TScriptFunctions.GetItem(Index : Integer) : TScriptFunction;
begin
	Result := FItems.Items[Index];
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TInterpreterException }


constructor TParserException.Create(Token : TToken; const Text : String);
begin
	inherited Create(Text);
	FToken := TToken.Create(Token.Text, Token.Flags, Token.Col, Token.Line, Token.TokenType);
	FToken.FLine := Token.FLine;
	FToken.FCol := Token.FCol;
end;

constructor TParserException.CreateFmt(Token : TToken; const Text : String; const Args: array of const);
begin
	inherited CreateFmt(Text, Args);
	FToken := TToken.Create(Token.Text, Token.Flags, Token.Col, Token.Line);
	FToken.FLine := Token.FLine;
	FToken.FCol := Token.FCol;
end;

destructor TParserException.Destroy();
begin
	FreeAndNil(FToken);
	Inherited;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TParser }

constructor TParser.Create();
begin
	FAllAstNodes := TObjectList<TASTNode>.Create();
end;

destructor TParser.Destroy();
begin
	FreeAndNil(FAllAstNodes);
	Inherited;
end;

procedure TParser.ParseScript(Tokens : TTokenizer; Functions : TScriptFunctions);
var
	Token : TToken;
	Item : TASTNode;
	AfterToken : TToken;
begin
	while true do begin
		Token := Tokens.Get(false);
		if ltEndOfText in Token.Flags then Exit;

		if Token.TokenType = ttProcedure then begin
			Item := ParseProcedureDecl(Tokens);
			Functions.Add(TScriptFunction.Create(Item, false));
		end

		else if Token.TokenType = ttFunction then begin
			Item := ParseProcedureDecl(Tokens);
			Functions.Add(TScriptFunction.Create(Item, true));
		end

		else begin
			Item := ParseStatements(nil, Tokens);
			Functions.Add( TScriptFunction.Create(TASTNode.CreateSpecial(self, nil, ttName, strMainFunc, Item), false) );
			break;
		end;
	end;

	if Tokens.Pos <> Tokens.Count then begin
		AfterToken := Tokens.Get(false);
		if not (ltEndOfText in AfterToken.Flags) then
			raise TParserException.CreateFmt(AfterToken, lngExpEOFButFoundS, [AfterToken.Text]);
	end;
end;

procedure TParser.ParseExpression(Tokens : TTokenizer; Expression : TASTNode);
var
	AfterToken : TToken;
	ResultExpression : TASTNode;
begin
	ResultExpression := ParseExpr(Tokens, 0, false);
	if Tokens.Pos <> Tokens.Count then begin
		AfterToken := Tokens.Get(false);
		if not (ltEndOfText in AfterToken.Flags) then
			raise TParserException.CreateFmt(AfterToken, lngExpExprEndButFoundS, [AfterToken.Text]);
		Expression.Swap(ResultExpression);
	end;
end;

function TParser.GetOperPrioryty(Token : TToken) : Integer;
begin
	case Token.TokenType of
		ttOr, ttAnd, ttXor:
			Result := 1;

		ttNot:
			Result := 2;

		ttLess, ttLessEq, ttGreater, ttGreaterEq, ttEquals, ttNotEq:
			Result := 3;

		ttPlus, ttMinus:
			Result := 4;

		ttDiv, ttMult, ttIDiv, ttMod:
			Result := 5;

		ttShl, ttShr:
			Result := 6;

		ttPower:
			Result := 7;
	else
		raise TInternalInterpreterException.CreateFmt(
			lngCantFindOperPrio,
			[Token.Text]
		);
	end;
end;

procedure TParser.ParseProcedureArgsDecl(Func : TASTNode; Tokens : TTokenizer);
var
	Token : TToken;
begin
	while true do begin
		Token := Tokens.Get(true);
		if Token.TokenType = ttClBracket then Exit;
		if not (ltName in Token.Flags) then
			raise TParserException.CreateFmt(Token, lngWrongFuncArgS, [Token.Text]);

		Func.Add(TASTNode.Create(self, Token, nil));
		Tokens.Next();

		Token := Tokens.Get(true);
		if Token.TokenType <> ttComma then Break;
		Tokens.Next();
	end;
end;

function TParser.ParseProcedureDecl(Tokens : TTokenizer) : TASTNode;
var
	NameToken : TToken;
	FuncToken : TToken;
	Token : TToken;
begin
	FuncToken := Tokens.Get(true);
	Tokens.Next();

	NameToken := Tokens.Get(true);

	if not (ltName in NameToken.Flags) then
		raise TParserException.CreateFmt(FuncToken, lngWrongFuncNameS, [NameToken.Text]);

	Tokens.Next();
	Result := TASTNode.Create(self, NameToken, nil);

	Token := Tokens.Get(true);
	if Token.TokenType = ttOpBracket then begin
		Tokens.Next();
		ParseProcedureArgsDecl(Result, Tokens);
		Tokens.Expect(ttClBracket);
	end;
	
	Tokens.Expect(ttSemi);
	Tokens.Expect(ttBegin);
	Result.Add( ParseStatements(nil, Tokens) );
	Tokens.Expect(ttEnd);
	Tokens.Expect(ttSemi);
end;

function TParser.ParseExprSeq(Parent : TASTNode; Tokens : TTokenizer; DelimTT, EndTT : TTokenType) : TToken;
var
	Arg : TASTNode;
begin
	Result := Tokens.Get(false);
	if Assigned(Result) and (Result.TokenType = EndTT) then begin
		Tokens.Next;
		Exit;
	end;

	while true do begin
		Arg := ParseExpr(Tokens, 0, false);
		Parent.Add(Arg);
		Result := Tokens.ExpectAnyOf([DelimTT, EndTT]);
		if Result.TokenType = EndTT then Break;
	end;
end;

function TParser.ParseName(NameToken : TToken; Tokens : TTokenizer) : TASTNode;
var
	NextToken : TToken;
	FieldToken : TToken;
	FieldNode : TASTNode;
begin
	Tokens.Next();
	NextToken := Tokens.Get(false);

	case NextToken.TokenType of
		ttOpBracket: begin // ( ... )
			Tokens.Next;
			Result := TASTNode.Create(self, NameToken, nil);
			ParseExprSeq(Result, Tokens, ttComma, ttClBracket);
		end;

		ttSqOpBracket: begin // [ ... ]
			Tokens.Next();
			Result := TASTNode.Create(self, NameToken, nil);
			Result.Add(TASTNode.Create(self, NextToken, nil));
			NextToken := ParseExprSeq(Result, Tokens, ttComma, ttSqClBracket);
			Result.Add(TASTNode.Create(self, NextToken, nil));
		end;
	else
		Result := TASTNode.Create(self, NameToken, nil);
	end;

	while True do begin
		if NextToken = nil then break;
		if NextToken.TokenType = ttDot then begin // NAME.FILED
			Tokens.Next;
			FieldToken := Tokens.Get(false);
			FieldNode := TASTNode.Create(self, FieldToken, nil);
			Tokens.Next;
			Result := TASTNode.Create(self, NextToken, nil, Result, FieldNode);
		end
		else Break;
		NextToken := Tokens.Get(false);
	end;
end;

function TParser.ParseExpr(Tokens : TTokenizer; LastOperPriority : Integer; CanBeNull : Boolean) : TASTNode;
var
	Token : TToken;
	Prior : Integer;
	Second : TASTNode;
begin
	Result := nil;

	while true do begin
		Token := Tokens.Get(false);

		if (ltEndOfText in Token.Flags)
		or (not (ltExprContent in Token.Flags) and (Token.TokenType <> ttOpBracket))
		then begin
			if (not CanBeNull) and (Result = nil) then
				raise TParserException.CreateFmt(Token, lngUnexpSInExpr, [Token.Text]);
			Exit;
		end;

		if ltName in Token.Flags then begin
			if Result<>nil then Exit;
			Result := ParseName(Token, Tokens);
		end

		else if ltConst in Token.Flags then begin
			Tokens.Next();
			if Result<>nil then raise TParserException.CreateFmt(Token, lngUnexpConstInExpr, [Token.Text]);
			
			Result := TASTNode.Create(self, Token, nil);
		end

		else if Token.TokenType = ttOpBracket then begin
			Tokens.Next();
			if Result<>nil then raise TParserException.Create(Token, lngUnexpBracketInExpr);
			
			Result := ParseExpr(Tokens, 0, CanBeNull);
			Tokens.Expect(ttClBracket);
		end

		else if ltExprOperator in Token.Flags then begin
			if (Result = nil)
			and not (Token.TokenType in [ttMinus, ttPlus, ttNot]) then
				raise TParserException.CreateFmt(Token, lngImpToUseBOperInExpr, [Token.Text]);

			Prior := GetOperPrioryty(Token);
			if (Result <> nil) and (Prior <= LastOperPriority) then Exit;
			Tokens.Next();
			Second := ParseExpr(Tokens, Prior, CanBeNull);
			if Result = nil then Result := TASTNode.Create(self, Token, nil, Second)
			else Result := TASTNode.Create(self, Token, nil, Result, Second);
		end

		else
			raise TParserException.CreateFmt(Token, lngUnexpSInExpr, [Token.Text]);
	end;
end;

function TParser.ParseWhile(WhileToken : TToken; Parent : TASTNode; Tokens : TTokenizer) : TASTNode;
var
	Cond, Code : TASTNode;
begin
	Tokens.Next();
	Result := TASTNode.Create(self, WhileToken, Parent);
	Cond := ParseExpr(Tokens, 0, false);
	Tokens.Expect(ttDo);
	Code := ParseStatement(Result, Tokens);
	Result.Add(Cond);
	Result.Add(Code);
end;

function TParser.ParseIf(IfToken : TToken; Parent : TASTNode; Tokens : TTokenizer) : TASTNode;
var
	Cond, IfCode, ElseCode : TASTNode;
	Token : TToken;
begin
	Tokens.Next();
	Cond := ParseExpr(Tokens, 0, false);
	Tokens.Expect(ttThen);

	Result := TASTNode.Create(self, IfToken, Parent);

	IfCode := ParseStatement(Result, Tokens);
	Token := Tokens.Get(false);
	if Token.TokenType = ttElse then begin
		Tokens.Next();
		ElseCode := ParseStatement(Result, Tokens);
	end
	else ElseCode := nil;

	Result.Add(Cond);
	Result.Add(IfCode);
	Result.Add(ElseCode);
end;

function TParser.ParseFor(ForToken : TToken; Parent : TASTNode; Tokens : TTokenizer) : TASTNode;
var
	StartSt, EndVal, ForCode : TASTNode;
	ForType : TToken;
begin
	Tokens.Next();
	
	StartSt := ParseStatement(nil, Tokens);

	if StartSt = nil then
		raise TParserException.Create(ForToken, lngStartForValNotSet);

	if StartSt.Token.TokenType <> ttAssig then
		raise TParserException.CreateFmt(StartSt.Token, lngWrongOperInForStart, [StartSt.Token.Text]);

	ForType := Tokens.Get(true);
	Tokens.Next();
	EndVal := ParseExpr(Tokens, 0, false);
	Tokens.Expect(ttDo);

	Result := TASTNode.Create(self, ForToken, Parent);

	ForCode := ParseStatement(Result, Tokens);

	Result.Add(StartSt);
	Result.Add(TASTNode.Create(self, ForType, nil));
	Result.Add(EndVal);
	Result.Add(ForCode);
end;

function TParser.ParseRepeat(RepeatToken : TToken; Parent : TASTNode; Tokens : TTokenizer) : TASTNode;
var
	BodyAst : TASTNode;
	CondAst : TASTNode;
begin
	Tokens.Next();
	Result := TASTNode.Create(self, RepeatToken, Parent);
	BodyAst := ParseStatement(Result, Tokens);
	Tokens.Expect(ttUntil);
	CondAst := ParseExpr(Tokens, 0, false);
	Result.Add(BodyAst);
	Result.Add(CondAst);
end;

function TParser.ParseIncDec(Token : TToken; Tokens : TTokenizer) : TASTNode;
begin
	Tokens.Next();
	Tokens.Expect(ttOpBracket);
	Result := TASTNode.Create(self, Token, nil);
	ParseExprSeq(Result, Tokens, ttComma, ttClBracket);
	if (Result.Count <> 1) and (Result.Count <> 2) then
		raise TParserException.CreateFmt(Token, lngWrongArgCntForS, [Token.Text]);
end;

function TParser.ParseCase(CaseToken : TToken; Parent : TASTNode; Tokens : TTokenizer) : TASTNode;
var
	ExprNode : TASTNode;
	CaseItemAst : TASTNode;
begin
	Tokens.Next();

	Result := TASTNode.Create(self, CaseToken, Parent);

	ExprNode := ParseExpr(Tokens, 0, false);
	Result.Add(ExprNode);

	Tokens.Expect(ttOf);

	while true do begin
		CaseItemAst := ParseCaseItem(Result, Tokens);
		if CaseItemAst = nil then break;
		Result.Add(CaseItemAst);
	end;

	Tokens.Expect(ttEnd);
end;

function TParser.ParseCaseItem(Parent : TASTNode; Tokens : TTokenizer) : TASTNode;
var
	Token : TToken;
begin
	Result := nil;
	Token := Tokens.Get(true);

	case Token.TokenType of
		ttEnd: Exit;

		ttElse: begin
			result := TASTNode.Create(self, Token, parent);
			Tokens.Next();
		end
	else
		result := TASTNode.CreateSpecial(self, Parent, ttSeq, strSeq);
		ParseExprSeq(result, Tokens, ttComma, ttColon);
	end;

	result.Add(ParseStatement(result, Tokens));
end;

function TParser.ParseTryFinally(TryToken : TToken; Parent : TASTNode; Tokens : TTokenizer) : TASTNode;
var
	TryBody : TASTNode;
	FinallyBody : TASTNode;
begin
	Tokens.Next();
	Result := TASTNode.Create(self, TryToken, Parent);
	TryBody := ParseStatements(Result, Tokens);
	Tokens.Expect(ttFinally);
	FinallyBody := ParseStatements(Result, Tokens);
	Tokens.Expect(ttEnd);
	result.Add(TryBody);
	result.Add(FinallyBody);
end;

function TParser.ParseStatements(Parent : TASTNode; Tokens : TTokenizer) : TASTNode;
var
	Token : TToken;
	AstItem : TASTNode;
begin
	Result := nil;
	while (true) do begin
		Token := Tokens.Get(false);

		if ltEndOfText in Token.Flags then Exit;
		if Token.TokenType in [ttEnd, ttFinally] then break;

		AstItem := ParseStatement(Parent, Tokens);
		if (AstItem=nil) and (Token.TokenType <> ttSemi) then
			raise TParserException.CreateFmt(Token, lngSIsNotOper, [Token.Text]);

		if Result = nil then Result := AstItem
		else Result := TASTNode.CreateSpecial(self, Parent, ttSeq, strSeq, Result, AstItem);
	end;
end;

function TParser.ParseStatement(Parent : TASTNode; Tokens : TTokenizer) : TASTNode;
var
	Token : TToken;
begin
	Result := nil;

	Token := Tokens.Get(false);
	if ltEndOfText in Token.Flags then Exit;

	if Token.TokenType = ttSemi then begin
		Tokens.Next();
		Exit;
	end;

	if not ((ltStatementWord in Token.Flags) or (ltName in Token.Flags)) then Exit;

	case Token.TokenType of
		ttBegin: begin
			Tokens.Next();
			Result := ParseStatements(Parent, Tokens);
			Tokens.Expect(ttEnd);
		end;

		ttBreak, ttCont, ttExit: begin
			Result := TASTNode.Create(self, Token, Parent);
			Tokens.Next();
		end;

		ttInc, ttDec:
			Result := ParseIncDec(Token, Tokens);

		ttWhile:  Result := ParseWhile     (Token, Parent, Tokens);
		ttIf:     Result := ParseIf        (Token, Parent, Tokens);
		ttFor:    Result := ParseFor       (Token, Parent, Tokens);
		ttRepeat: Result := ParseRepeat    (Token, Parent, Tokens);
		ttCase:   Result := ParseCase      (Token, Parent, Tokens);
		ttTry:    Result := ParseTryFinally(Token, Parent, Tokens);
	else
		if ltName in Token.Flags then begin
			Result := ParseName(Token, Tokens);
			Token := Tokens.Get(false);
			if Assigned(Token) and (Token.TokenType = ttAssig) then begin
				Tokens.Next;
				Result := TASTNode.Create(self, Token, nil, Result, ParseExpr(Tokens, -1, false));
			end;
		end
		else raise TParserException.CreateFmt(Token, lngWrongOper, [Token.Text]);
	end;

	if (not Tokens.EOF) and (Tokens.Get(false).TokenType = ttSemi) then Tokens.Next();
end;


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TFunDebugInfo }

constructor TFunDebugInfo.Create(ScriptFun : TScriptFunction);
var
	i : Integer;
begin
	FArgs := TStringList.Create;
	FVars := TStringList.Create;

	FFunName := ScriptFun.Name;
	for i := 0 to ScriptFun.ArgsCount-1 do FArgs.Add(ScriptFun.Args[i]);
	for i := 0 to ScriptFun.VariablesCount-1 do FVars.Add(ScriptFun.Vars[i]);
end;

destructor TFunDebugInfo.Destroy();
begin
	FreeAndNil(FVars);
	FreeAndNil(FArgs);
	Inherited;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TDebugInfo }

constructor TDebugInfo.Create();
begin
	FSrcPositions := TDictionary<Integer,TSrcPos>.Create();
	FFunDebugItems := TObjectList<TFunDebugInfo>.Create();
end;

destructor TDebugInfo.Destroy();
begin
	FreeAndNil(FFunDebugItems);
	FreeAndNil(FSrcPositions);
	Inherited;
end;

procedure TDebugInfo.Clear();
begin
	FFunDebugItems.Clear();
	FSrcPositions.Clear();
end;

function TDebugInfo.AddFunDebugInfo(ScriptFun : TScriptFunction) : Integer;
var
	FunDebugInfo : TFunDebugInfo;
begin
	Result := FFunDebugItems.Count;
	FunDebugInfo := TFunDebugInfo.Create(ScriptFun);
	FFunDebugItems.Add(FunDebugInfo);
end;

function TDebugInfo.GetFunDebugItem(FunIndex : Integer) : TFunDebugInfo;
begin
	Result := FFunDebugItems[FunIndex];
end;

procedure TDebugInfo.AddSrcPos(CodePos : Integer; SrcPos : TSrcPos);
begin
	FSrcPositions.Add(CodePos, SrcPos);
end;

function TDebugInfo.GetSrcPosByAddr(Addr : Integer) : TSrcPos;
var
	Found : Boolean;
begin
	Found := FSrcPositions.TryGetValue(Addr, Result);
	if not Found then begin
		Result.Col := -1;
		Result.Line := -1;
	end;
end;

function TDebugInfo.GetCodePosBySrcLine(LineNum : Integer) : Integer;
begin
	for Result in FSrcPositions.Keys do
		if FSrcPositions[Result].Line = LineNum then Exit;
	Result := -1;
end;

procedure TDebugInfo.ToStrings(Text : TStrings);
var
	Info : TFunDebugInfo;
	Index : Integer;

	function StringsToStr(Strings : TStrings) : String;
	var
		Item : String;
	begin
		Result := '';
		for Item in Strings do begin
			if Result <> '' then  Result := Result + ', ';
			Result := Result + item;
		end;
	end;

begin
	Index := 0;
	Text.Add('(Functions)');
	for Info in FFunDebugItems do begin
		Text.Add(Format(
			'%8d %-16s; arguments: %s; Variables: %s',
			[Index, Info.FunName, StringsToStr(Info.Args), StringsToStr(Info.Vars)]
		));
		Inc(Index);
	end;
end;


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TBinaryCode }

constructor TBinaryCode.Create();
begin
	FChachedCodeBegin := nil;
	FCode := TList<TBinaryCodeItem>.Create;
	FStrings := TList<PString>.Create;
	FBreakpoins := TList<TBreakpoint>.Create();
end;

destructor TBinaryCode.Destroy();
begin
	Clear();
	FreeAndNil(FBreakpoins);
	FreeAndNil(FStrings);
	FreeAndNil(FCode);
end;

procedure TBinaryCode.Clear();
var
	Str : PString;
begin
	FChachedCodeBegin := nil;
	for Str in FStrings do Dispose(Str);
	FStrings.Clear();

	FCode.Clear();
	FBreakpoins.Clear();
end;

function TBinaryCode.Size : Integer;
begin
	Result := FCode.Count;
end;

procedure TBinaryCode.AddInstr(Instr : TCodeInstr; Token : TToken; DebugInfo : TDebugInfo);
var
	CodePos : TSrcPos;
	Tmp : TBinaryCodeItem;
begin
	if Token <> nil then begin
		CodePos.Col := Token.Col;
		CodePos.Line := Token.Line;
		DebugInfo.AddSrcPos(FCode.Count, CodePos);
	end;

	Tmp.Instr := Instr;
	FCode.Add(Tmp);
	FChachedCodeBegin := nil;
end;

procedure TBinaryCode.AddInteger(Value : Integer);
var
	Tmp : TBinaryCodeItem;
begin
	Tmp.Int := Value;
	FCode.Add(Tmp);
	FChachedCodeBegin := nil;
end;

procedure TBinaryCode.AddFloat(Value : Single);
var
	Tmp : TBinaryCodeItem;
begin
	Tmp.Float := Value;
	FCode.Add(Tmp);
	FChachedCodeBegin := nil;
end;

procedure TBinaryCode.AddBool(Value : Boolean);
var
	Tmp : TBinaryCodeItem;
begin
	Tmp.Bool := Value;
	FCode.Add(Tmp);
	FChachedCodeBegin := nil;
end;

procedure TBinaryCode.SetInteger(Pos, Value : Integer);
var
	Tmp : TBinaryCodeItem;
begin
	Tmp.Int := Value;
	FCode[Pos] := Tmp;
	FChachedCodeBegin := nil;
end;

function TBinaryCode.AddStr(const Str : String) : Integer;
var
	NewStr : PString;
begin
	New(NewStr);
	NewStr^ := Str;
	Result := FStrings.Count;
	FStrings.Add(NewStr);
	FChachedCodeBegin := nil;
end;

procedure TBinaryCode.CreateCachedCodeArr();
begin
	if FChachedCodeBegin <> nil then Exit;
	FCachedCodeArr := FCode.ToArray;
	if Length(FCachedCodeArr) = 0 then FChachedCodeBegin := nil
	else FChachedCodeBegin := @FCachedCodeArr[0];
end;

function TBinaryCode.GetInstrBegin : PBinaryCodeItem;
begin
	CreateCachedCodeArr();
	Result := FChachedCodeBegin;
end;

function TBinaryCode.GetString(Index : Integer) : PString;
begin
	Result := FStrings[Index];
end;

procedure TBinaryCode.AddBreakPoint(LineNum : Integer; DebugInfo : TDebugInfo);
var
	Breakpoint : TBreakpoint;
	CodeItem : TBinaryCodeItem;
begin
	if GetBreakPointIndexByLineNum(LineNum) <> -1 then Exit;
	Breakpoint.LineNum := LineNum;
	Breakpoint.CodePos := DebugInfo.GetCodePosBySrcLine(LineNum);

	if Breakpoint.CodePos = -1 then
		raise TCodeNotFoundForLineException.CreateFmt(lngCodeForLineNF, [LineNum]);

	CreateCachedCodeArr();
	Breakpoint.Instr := FCode[Breakpoint.CodePos].Instr;
	CodeItem.Instr := ciBreakpoint;
	FCachedCodeArr[Breakpoint.CodePos] := CodeItem;
	FBreakpoins.Add(Breakpoint);
end;

procedure TBinaryCode.RemoveBreakpoint(LineNum : Integer; DebugInfo : TDebugInfo);
var
	BpIndex : Integer;
	Breakpoint : TBreakpoint;
	CodeItem : TBinaryCodeItem;
begin
	BpIndex := GetBreakPointIndexByLineNum(LineNum);
	if BpIndex = -1 then
		raise TBreakpointNotFoundException.CreateFmt(lngBPNotFoundAtLine, [LineNum]);

	CreateCachedCodeArr();
	Breakpoint := FBreakpoins[BpIndex];
	CodeItem.Instr := Breakpoint.Instr;
	FCachedCodeArr[Breakpoint.CodePos] := CodeItem;
	FBreakpoins.Delete(BpIndex);
end;

function TBinaryCode.GetBreakPointIndexByLineNum(LineNum : Integer) : Integer;
begin
	for Result := 0 to FBreakpoins.Count-1 do
		if FBreakpoins[Result].LineNum = LineNum then Exit;
	Result := -1;
end;

function TBinaryCode.GetBreakPointIndexByCodePos(CodePos : Integer) : Integer;
begin
	for Result := 0 to FBreakpoins.Count-1 do
		if FBreakpoins[Result].CodePos = CodePos then Exit;
	Result := -1;
end;

procedure TBinaryCode.RevertInstructionForBreakpoint(CodePos : Integer);
var
	BpIndex : Integer;
	Bp : TBreakpoint;
	CodeInstr : TBinaryCodeItem;
begin
	BpIndex := GetBreakPointIndexByCodePos(CodePos);
	Assert(BpIndex <> -1);
	Bp := FBreakpoins[BpIndex];
	CodeInstr.Instr := Bp.Instr;
	FCachedCodeArr[Bp.CodePos] := CodeInstr;
end;

procedure TBinaryCode.SetInstructionsForAllBreakpoints();
var
	Bp : TBreakpoint;
	CodeInstr : TBinaryCodeItem;
begin
	CreateCachedCodeArr();
	CodeInstr.Instr := ciBreakpoint;
	for Bp in FBreakpoins do FCachedCodeArr[Bp.CodePos] := CodeInstr;
end;

procedure TBinaryCode.Disassemble(DestText : TStrings; DebugInfo : TDebugInfo);
begin
	DestText.Add('Code:');
	DisassembleCode(DestText, DebugInfo);
	DestText.Add('');
	DestText.Add('Strings:');
	DisassembleStrings(DestText);
	DestText.Add('');
	DestText.Add('Debug information:');
	DebugInfo.ToStrings(DestText);
end;

procedure TBinaryCode.DisassembleCode(Text : TStrings; DebugInfo : TDebugInfo);
type
	TInstrInfoType = (
		iitSimple,
		iitInt,
		iitLoc,
		iitFloat,
		iitBool,
		iitStr,
		iitArr,
		iitAddr,
		iitUF,
		iitLocBeg
	);

	TInstrInfo = record
		Name : String;
		Tp   : TInstrInfoType;
	end;
var
	CodeItem : TBinaryCodeItem;
	Info : TInstrInfo;
	i : Integer;
	Items : TDictionary<TCodeInstr,TInstrInfo>;
	ItemArg : String;
	Comment : String;
	Line : String;
	Addr : Integer;
	CurFun : TFunDebugInfo;
	LocIndex : Integer;

	procedure AddItem(Instr : TCodeInstr; Name : PChar; Tp : TInstrInfoType);
	var
		Info : TInstrInfo;
	begin
		Info.Name := Name;
		Info.Tp := Tp;
		Items.Add(Instr, Info);
	end;

	function GetCommentForLoc(LocIndex : Integer) : String;
	begin
		if LocIndex >= 0 then
			Result := Format('var %s', [CurFun.Vars[LocIndex]])
		else
			Result := Format('arg %s', [CurFun.Args[-cArgsOffset-LocIndex]]);
	end;

begin
	Items := TDictionary<TCodeInstr,TInstrInfo>.Create();
	try
		AddItem(ciPushInt,         'push_int',        iitInt   );
		AddItem(ciPushFloat,       'push_float',      iitFloat );
		AddItem(ciPushBool,        'push_bool',       iitBool  );
		AddItem(ciPushStr,         'push_str',        iitStr   );
		AddItem(ciPushNil,         'push_nil',        iitSimple);
		AddItem(ciPushLocalValue,  'push_local',      iitLoc   );
		AddItem(ciPushAcc,         'push_acc',        iitSimple);
		AddItem(ciPopAcc,          'pop_acc',         iitSimple);
		AddItem(ciPopLocalValue,   'pop_local',       iitLoc   );
		AddItem(ciPushGlobalValue, 'push_global',     iitInt   );
		AddItem(ciPopGlobalValue,  'pop_global',      iitInt   );
		AddItem(ciPushLocalArray,  'push_array',      iitArr   );
		AddItem(ciPopLocalArray,   'pop_array',       iitArr   );
		AddItem(ciFunFrameBegin,   'fun_frame_begin', iitLocBeg);
		AddItem(ciFunFrameEnd,     'fun_frame_end',   iitInt   );
		AddItem(ciDecrStack,       'decr_stack',      iitInt   );
		AddItem(ciAdd,             'add',             iitSimple);
		AddItem(ciSub,             'sub',             iitSimple);
		AddItem(ciMult,            'mul',             iitSimple);
		AddItem(ciDiv,             'div',             iitSimple);
		AddItem(ciIDiv,            'idiv',            iitSimple);
		AddItem(ciMod,             'mod',             iitSimple);
		AddItem(ciShl,             'shl',             iitSimple);
		AddItem(ciShr,             'shr',             iitSimple);
		AddItem(ciPower,           'power',           iitSimple);
		AddItem(ciOr,              'or',              iitSimple);
		AddItem(ciAnd,             'and',             iitSimple);
		AddItem(ciXor,             'xor',             iitSimple);
		AddItem(ciEqual,           'equal',           iitSimple);
		AddItem(ciLess,            'less',            iitSimple);
		AddItem(ciLessEq,          'less_or_equal',   iitSimple);
		AddItem(ciGreater,         'greater',         iitSimple);
		AddItem(ciGreaterEq,       'greater_or_equal',iitSimple);
		AddItem(ciNot,             'not',             iitSimple);
		AddItem(ciNeg,             'neg',             iitSimple);
		AddItem(ciJmp,             'jump',            iitAddr  );
		AddItem(ciJmpIf,           'jump_if',         iitAddr  );
		AddItem(ciJmpIfNot,        'jump_if_not',     iitAddr  );
		AddItem(ciReturn,          'ret',             iitSimple);
		AddItem(ciCall,            'call',            iitAddr  );
		AddItem(ciUserFunc,        'user_fun',        iitUF    );
		AddItem(ciExit,            'exit',            iitSimple);

		i := 0;
		CurFun := nil;
		while i < FCode.Count do begin
			Addr := i;
			CodeItem := FCode[i];
			Inc(i);
			Info := Items[CodeItem.Instr];
			ItemArg := '';
			Comment := '';
			case Info.Tp of
				iitInt: begin
					ItemArg := Format('%d', [FCode[i].Int]);
					Inc(i);
				end;

				iitLoc: begin
					LocIndex := FCode[i].Int;
					ItemArg := Format('%d', [LocIndex]);
					Comment := GetCommentForLoc(LocIndex);
					Inc(i);
				end;

				iitFloat: begin
					ItemArg := Format('%f', [FCode[i].Float]);
					Inc(i);
				end;

				iitBool: begin
					ItemArg := Format('%s', [BoolToStr(FCode[i].Bool, true)]);
					Inc(i);
				end;

				iitStr: begin
					ItemArg := Format('%d', [FCode[i].Int]);
					Comment := Format('"%s"', [GetString(FCode[i].Int)^]);
					Inc(i);
				end;

				iitArr: begin
					LocIndex := FCode[i].Int;
					ItemArg := Format('%d, %d', [LocIndex, FCode[i+1].Int]);
					Comment := GetCommentForLoc(LocIndex);
					Inc(i, 2);
				end;

				iitAddr: begin
					ItemArg := Format('%.8x', [FCode[i].Int]);
					Inc(i);
				end;

				iitLocBeg: begin
					CurFun := DebugInfo.GetFunDebugItem(FCode[i].Int);
					ItemArg := Format('%d, %d', [FCode[i].Int, FCode[i+1].Int]);
					Comment := Format('fun %s', [CurFun.FFunName]);
					Inc(i, 2);
				end;

				iitUF: begin
					ItemArg := Format('%d, %d', [FCode[i].Int, FCode[i+1].Int]);
					Inc(i, 2);
				end;
			end;
			if ItemArg = '' then Line := Format('%.8x %-17s', [Addr, Info.Name])
			else begin
				if Comment = '' then Line := Format('%.8X %-17s %s', [Addr, Info.Name, ItemArg])
				else Line := Format('%.8X %-17s %-16s; %s', [Addr, Info.Name, ItemArg, Comment]);
			end;
			Text.Add(Trim(Line));
		end;
	finally
		FreeAndNil(Items);
	end;
end;

procedure TBinaryCode.DisassembleStrings(Text : TStrings);
var
	i : Integer;
begin
	for i := 0 to FStrings.Count-1 do begin
		Text.Add(Format('%8d "%s"', [i, FStrings[i]^]));
	end;
end;

procedure TBinaryCode.SaveToStream(Stream : TStream);
	procedure WriteInt(Value : Integer);
	begin
		Stream.Write(Value, sizeof(Value));
	end;

	procedure WriteStr(const Str : String);
	begin
		WriteInt(Length(Str));
		Stream.Write(Str[1], Length(Str)*sizeof(Char));
	end;
var
	Item : TBinaryCodeItem;
	Str : PString;
begin
	WriteInt(FCode.Count);
	for Item in FCode do WriteInt(Item.Int);
	WriteInt(FStrings.Count);
	for Str in FStrings do WriteStr(Str^);
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TCodeGenerator }

constructor TCodeGenerator.Create();
begin
	FUserVars      := nil;
	FCode          := nil;
	FUserFunctions := nil;
	FLabelsUse     := TDictionary<Integer, String>.Create();
	FLabels        := TDictionary<String, Integer>.Create();
	FForVariables  := TStringList.Create();
end;

destructor TCodeGenerator.Destroy();
begin
	FreeAndNil(FLabels);
	FreeAndNil(FLabelsUse);
	FreeAndNil(FForVariables);
	Inherited;
end;

procedure TCodeGenerator.OptimizeConsts(var RootAst : TAstNode);
begin
	repeat
		FWasOptimized := false;
		VisitAll(RootAst, FoldConst);
	until not FWasOptimized;
end;

procedure TCodeGenerator.SubstituteConsts(var RootAst : TAstNode);
begin
	VisitAll(
		RootAst, 
		procedure(var AST : TASTNode) 
		var
			New : TToken;
		begin
			if not (ltName in AST.Token.Flags) then Exit;
			if AST.Count <> 0 then Exit;
			New := nil;
			case self.FUserConsts.GetConstType(AST.Token.Text) of
				vtInt:    New := TToken.CreateInt  (self.FUserConsts.GetInt   (AST.Token.Text), AST.Token.Col, AST.Token.Line);
				vtFloat:  New := TToken.CreateFloat(self.FUserConsts.GetSingle(AST.Token.Text), AST.Token.Col, AST.Token.Line);
				vtString: New := TToken.CreateStr  (self.FUserConsts.GetStr   (AST.Token.Text), AST.Token.Col, AST.Token.Line);
			end;
			if New <> nil then
				Ast := TASTNode.Create(AST.Parser, New, nil, true);
		end
	);	
end;

procedure TCodeGenerator.FoldConst(var AST : TASTNode);
var
	T, T1, T2 : TToken;
	New : TToken;
	TwoTypes : Cardinal;
const
	Int_Int     = (Cardinal(ttIntConst)   shl 16) + Cardinal(ttIntConst);
	Float_Int   = (Cardinal(ttFloatConst) shl 16) + Cardinal(ttIntConst);
	Int_Float   = (Cardinal(ttIntConst)   shl 16) + Cardinal(ttFloatConst);
	Float_Float = (Cardinal(ttFloatConst) shl 16) + Cardinal(ttFloatConst);
	Bool_Bool   = (Cardinal(ttBoolConst)  shl 16) + Cardinal(ttBoolConst);
begin
	if not (ltExprContent in AST.Token.Flags) then Exit;

	New := nil;

	if AST.Count = 2 then begin
		T1 := AST[0].Token;
		T2 := AST[1].Token;

		TwoTypes := (Cardinal(T1.TokenType) shl 16) + Cardinal(T2.TokenType);

		case AST.Token.TokenType of
			ttPlus: case TwoTypes of
				Int_Int:     New := TToken.CreateInt  (T1.IntValue   + T2.IntValue,   T1.Col, T1.Line);
				Float_Int:   New := TToken.CreateFloat(T1.FloatValue + T2.IntValue,   T1.Col, T1.Line);
				Int_Float:   New := TToken.CreateFloat(T1.IntValue   + T2.FloatValue, T1.Col, T1.Line);
				Float_Float: New := TToken.CreateFloat(T1.FloatValue + T2.FloatValue, T1.Col, T1.Line);
			end;

			ttMinus: case TwoTypes of
				Int_Int:     New := TToken.CreateInt  (T1.IntValue   - T2.IntValue,   T1.Col, T1.Line);
				Float_Int:   New := TToken.CreateFloat(T1.FloatValue - T2.IntValue,   T1.Col, T1.Line);
				Int_Float:   New := TToken.CreateFloat(T1.IntValue   - T2.FloatValue, T1.Col, T1.Line);
				Float_Float: New := TToken.CreateFloat(T1.FloatValue - T2.FloatValue, T1.Col, T1.Line);
			end;

			ttMult: case TwoTypes of
				Int_Int:     New := TToken.CreateInt  (T1.IntValue   * T2.IntValue,   T1.Col, T1.Line);
				Float_Int:   New := TToken.CreateFloat(T1.FloatValue * T2.IntValue,   T1.Col, T1.Line);
				Int_Float:   New := TToken.CreateFloat(T1.IntValue   * T2.FloatValue, T1.Col, T1.Line);
				Float_Float: New := TToken.CreateFloat(T1.FloatValue * T2.FloatValue, T1.Col, T1.Line);
			end;

			ttDiv: case TwoTypes of
				Int_Int:     New := TToken.CreateFloat(T1.IntValue   / T2.IntValue,   T1.Col, T1.Line);
				Float_Int:   New := TToken.CreateFloat(T1.FloatValue / T2.IntValue,   T1.Col, T1.Line);
				Int_Float:   New := TToken.CreateFloat(T1.IntValue   / T2.FloatValue, T1.Col, T1.Line);
				Float_Float: New := TToken.CreateFloat(T1.FloatValue / T2.FloatValue, T1.Col, T1.Line);
			end;

			ttIDiv: case TwoTypes of
				Int_Int:     New := TToken.CreateInt(T1.IntValue   div T2.IntValue,   T1.Col, T1.Line);
			end;

			ttMod: case TwoTypes of
				Int_Int:     New := TToken.CreateInt(T1.IntValue   mod T2.IntValue,   T1.Col, T1.Line);
			end;

			ttPower: case TwoTypes of
				Int_Int:     New := TToken.CreateFloat(Power(T1.IntValue,   T2.IntValue  ), T1.Col, T1.Line);
				Float_Int:   New := TToken.CreateFloat(Power(T1.FloatValue, T2.IntValue  ), T1.Col, T1.Line);
				Int_Float:   New := TToken.CreateFloat(Power(T1.IntValue,   T2.FloatValue), T1.Col, T1.Line);
				Float_Float: New := TToken.CreateFloat(Power(T1.FloatValue, T2.FloatValue), T1.Col, T1.Line);
			end;

			ttLess: case TwoTypes of
				Int_Int:     New := TToken.CreateBool(T1.IntValue   < T2.IntValue,   T1.Col, T1.Line);
				Float_Int:   New := TToken.CreateBool(T1.FloatValue < T2.IntValue,   T1.Col, T1.Line);
				Int_Float:   New := TToken.CreateBool(T1.IntValue   < T2.FloatValue, T1.Col, T1.Line);
				Float_Float: New := TToken.CreateBool(T1.FloatValue < T2.FloatValue, T1.Col, T1.Line);
			end;

			ttLessEq: case TwoTypes of
				Int_Int:     New := TToken.CreateBool(T1.IntValue   <= T2.IntValue,   T1.Col, T1.Line);
				Float_Int:   New := TToken.CreateBool(T1.FloatValue <= T2.IntValue,   T1.Col, T1.Line);
				Int_Float:   New := TToken.CreateBool(T1.IntValue   <= T2.FloatValue, T1.Col, T1.Line);
				Float_Float: New := TToken.CreateBool(T1.FloatValue <= T2.FloatValue, T1.Col, T1.Line);
			end;

			ttGreater: case TwoTypes of
				Int_Int:     New := TToken.CreateBool(T1.IntValue   > T2.IntValue,   T1.Col, T1.Line);
				Float_Int:   New := TToken.CreateBool(T1.FloatValue > T2.IntValue,   T1.Col, T1.Line);
				Int_Float:   New := TToken.CreateBool(T1.IntValue   > T2.FloatValue, T1.Col, T1.Line);
				Float_Float: New := TToken.CreateBool(T1.FloatValue > T2.FloatValue, T1.Col, T1.Line);
			end;

			ttGreaterEq: case TwoTypes of
				Int_Int:     New := TToken.CreateBool(T1.IntValue   >= T2.IntValue,   T1.Col, T1.Line);
				Float_Int:   New := TToken.CreateBool(T1.FloatValue >= T2.IntValue,   T1.Col, T1.Line);
				Int_Float:   New := TToken.CreateBool(T1.IntValue   >= T2.FloatValue, T1.Col, T1.Line);
				Float_Float: New := TToken.CreateBool(T1.FloatValue >= T2.FloatValue, T1.Col, T1.Line);
			end;

			ttEquals: case TwoTypes of
				Int_Int:     New := TToken.CreateBool(T1.IntValue   = T2.IntValue,   T1.Col, T1.Line);
				Float_Int:   New := TToken.CreateBool(T1.FloatValue = T2.IntValue,   T1.Col, T1.Line);
				Int_Float:   New := TToken.CreateBool(T1.IntValue   = T2.FloatValue, T1.Col, T1.Line);
				Float_Float: New := TToken.CreateBool(T1.FloatValue = T2.FloatValue, T1.Col, T1.Line);
			end;

			ttNotEq: case TwoTypes of
				Int_Int:     New := TToken.CreateBool(T1.IntValue   <> T2.IntValue,   T1.Col, T1.Line);
				Float_Int:   New := TToken.CreateBool(T1.FloatValue <> T2.IntValue,   T1.Col, T1.Line);
				Int_Float:   New := TToken.CreateBool(T1.IntValue   <> T2.FloatValue, T1.Col, T1.Line);
				Float_Float: New := TToken.CreateBool(T1.FloatValue <> T2.FloatValue, T1.Col, T1.Line);
			end;

			ttAnd: case TwoTypes of
				Bool_Bool: New := TToken.CreateBool(T1.BoolValue and T2.BoolValue, T1.Col, T1.Line);
				Int_Int:   New := TToken.CreateInt (T1.IntValue  and T2.IntValue,  T1.Col, T1.Line);
			end;

			ttOr: case TwoTypes of
				Bool_Bool: New := TToken.CreateBool(T1.BoolValue or T2.BoolValue, T1.Col, T1.Line);
				Int_Int:   New := TToken.CreateInt (T1.IntValue  or T2.IntValue,  T1.Col, T1.Line);
			end;

			ttXor: case TwoTypes of
				Bool_Bool: New := TToken.CreateBool(T1.BoolValue xor T2.BoolValue, T1.Col, T1.Line);
				Int_Int:   New := TToken.CreateInt (T1.IntValue  xor T2.IntValue,  T1.Col, T1.Line);
			end;
		end;
	end
	else if AST.Count = 1 then begin
		T := AST[0].Token;

		case AST.Token.TokenType of
			ttMinus: case T.TokenType of
				ttIntConst:   New := TToken.CreateInt  (-T.IntValue,   T.Col, T.Line);
				ttFloatConst: New := TToken.CreateFloat(-T.FloatValue, T.Col, T.Line);
			end;

			ttPlus: case T.TokenType of
				ttIntConst:   New := TToken.CreateInt  (T.IntValue,   T.Col, T.Line);
				ttFloatConst: New := TToken.CreateFloat(T.FloatValue, T.Col, T.Line);
			end;

			ttNot: if T.TokenType = ttBoolConst then
					New := TToken.CreateBool(not T.BoolValue, T.Col, T.Line);
		end;
	end;

	if New <> nil then begin
		Ast := TASTNode.Create(AST.Parser, New, nil, true);
		FWasOptimized := true;
	end;
end;

procedure TCodeGenerator.AddLabel(const Name : String);
begin
	FLabels.Add(Name, FCode.Size);
end;

procedure TCodeGenerator.AddLabel(Ast : TASTNode; const Prefix : String);
begin
	FLabels.Add(Prefix+Ast.UniqStr, FCode.Size);
end;

function TCodeGenerator.GetLabelPos(const Name : String) : Integer;
var
	Found : Boolean;
begin
	Found := FLabels.TryGetValue(Name, Result);
	if not Found then
		raise TInternalInterpreterException.CreateFmt(lngLabSNotFound, [Name]);
end;

procedure TCodeGenerator.AddLabelPos(const Name : String);
begin
	FLabelsUse.Add(FCode.Size, Name);
	FCode.AddInteger(-1);
end;

procedure TCodeGenerator.AddString(const Str : String);
var
	StrIndex : Integer;
begin
	StrIndex := FCode.AddStr(Str);
	FCode.AddInteger(StrIndex);
end;

procedure TCodeGenerator.FillLabelPositions();
var
	Key : Integer;
	Pos : Integer;
begin
	for Key in FLabelsUse.Keys do begin
		Pos := GetLabelPos(FLabelsUse[Key]);
		FCode.SetInteger(Key, Pos);
	end;
end;

procedure TCodeGenerator.GenerateFunctions(
	Functions     : TScriptFunctions;
	UserFunctions : TUserFunctions;
	UserVars      : TUserVariables;
	UserConsts    : TUserConsts;
	DebugInfo     : TDebugInfo;
	Code          : TBinaryCode
);
var
	i : Integer;
	MainFunc : TScriptFunction;
begin
	FDebugInfo := DebugInfo;
	FScriptFuncs := Functions;
	FUserVars := UserVars;
	FUserFunctions := UserFunctions;
	FUserConsts := UserConsts;
	FCode := Code;

	FLabelsUse.Clear();
	FLabels.Clear();
	FCode.Clear();
	FDebugInfo.Clear();

	// Сначала генерируем основной код
	MainFunc := Functions.FindByName(strMainFunc);
	if MainFunc = nil then
		raise TInternalInterpreterException.Create(lngMainFuncNotFound);

	Fun(MainFunc, UserConsts, false);
	Code.AddInstr(ciExit, nil, FDebugInfo);

	// Затем все остальные функции
	for i := 0 to Functions.Count-1 do begin
		if Functions[i] = MainFunc then Continue;
		Fun(Functions[i], UserConsts, true);
	end;

	FillLabelPositions();
end;

// TCodeGenerator.Fun - Генерация кода одной ф-ции скрипта
procedure TCodeGenerator.Fun(Func : TScriptFunction; UserConsts : TUserConsts; AddReturn : Boolean);
var
	ResultIndex : Integer;
	FunIndex : Integer;
begin
	// Подставляем константы
	SubstituteConsts(Func.FBodyNode);

	// Оптимизируем
	OptimizeConsts(Func.FBodyNode);

	// Выясняем все локальные переменные ф-ции
	Func.FindLocalVariables(FUserVars, FUserFunctions, FUserConsts, FScriptFuncs);

	// Добавляем в код отладочную инфу о ф-ции
	FunIndex := FDebugInfo.AddFunDebugInfo(Func);

	// Метка начала ф-ции 
	AddLabel(Func.Name);

	// Пролог ф-ции
	FCode.AddInstr(ciFunFrameBegin, nil, FDebugInfo);
	FCode.AddInteger(FunIndex);
	FCode.AddInteger(Func.VariablesCount);

	// Тело ф-ции
	Statement(Func, Func.BodyAST);

	// Метка окончания ф-ции
	AddLabel(strFuncEnd+Func.Name);

	// Эпилог ф-ции
	FCode.AddInstr(ciFunFrameEnd, nil, FDebugInfo);
	FCode.AddInteger(Func.VariablesCount);

	// ... возвращаемое значение - в аккумулятор
	if Func.IsFunction then ResultIndex := Func.VarIndex(strResult)
	else ResultIndex := -1;
	if ResultIndex <> -1 then begin
		FCode.AddInstr(ciPushLocalValue, nil, FDebugInfo);
		FCode.AddInteger(ResultIndex);
	end
	else FCode.AddInstr(ciPushNil, nil, FDebugInfo);
	FCode.AddInstr(ciPopAcc, nil, FDebugInfo);

	// ... возврат к вызываемому коду
	if AddReturn then FCode.AddInstr(ciReturn, nil, FDebugInfo);
end;

procedure TCodeGenerator.Statement(Func : TScriptFunction; Ast : TASTNode);
var
	Child : TASTNode;
begin
	if Ast = nil then Exit;

	case Ast.Token.TokenType of
		ttSeq:    for Child in Ast.Items do Statement(Func, Child);
		ttAssig:  StAssign    (Func, Ast);
		ttInc:    IncDec      (Func, Ast, false);
		ttDec:    IncDec      (Func, Ast, true);
		ttWhile:  StWhile     (Func, Ast);
		ttIf:     StIf        (Func, Ast);
		ttFor:    StFor       (Func, Ast);
		ttRepeat: StRepeat    (Func, Ast);
		ttBreak:  StBreakCont (Func, Ast, strBreakLabel);
		ttCont:   StBreakCont (Func, Ast, strContLabel);
		ttExit:   StExit      (Func, Ast);
		ttCase:   StCase      (Func, Ast);
		ttTry:    StTryFinally(Func, Ast);
	else
		if ltName in Ast.Token.Flags  then SomeFuncCall(Func, Ast)
		else raise TInternalInterpreterException.CreateFmt(lngDontKnowHTGenForS, [Ast.Token.Text]);
	end;
end;

procedure TCodeGenerator.IncDec(Func : TScriptFunction; StAst : TASTNode; D : boolean);
var
	NameAst : TASTNode;
	ValueAst : TASTNode;
begin
	NameAst := StAst[0];
	if StAst.Count = 2 then ValueAst := StAst[1]
	else ValueAst := nil;
	IncDec(Func, NameAst, ValueAst, D);
end;

procedure TCodeGenerator.IncDec(Func : TScriptFunction; NameAst, ValueAst : TASTNode; D : boolean);
begin
	Expr(Func, NameAst);
	if ValueAst <> nil then Expr(Func, ValueAst)
	else begin
		FCode.AddInstr(ciPushInt, nil, FDebugInfo);
		FCode.AddInteger(1);
	end;
	if not D then FCode.AddInstr(ciAdd, NameAst.Token, FDebugInfo)
	else FCode.AddInstr(ciSub, NameAst.Token, FDebugInfo);
	AssignPop(Func, NameAst);
end;

procedure TCodeGenerator.StAssign(Func : TScriptFunction; Ast : TASTNode);
begin
	Expr(Func, Ast[1]);
	AssignPop(Func, Ast[0]);
end;

procedure TCodeGenerator.AssignPop(Func : TScriptFunction; Ast : TASTNode);
var
	VarName : String;
	UserVarIndex : Integer;
	FuncVarIndex : Integer;
	ArgIndex : Integer;
begin
	VarName := Ast.Token.Text;

	if FForVariables.IndexOf(VarName) <> -1 then
		raise TParserException.CreateFmt(Ast.Token, lngImpToChangeForVar, [VarName]);

	ArgIndex := Func.ArgIndex(VarName);
	UserVarIndex := FUserVars.GetIndex(VarName);
	FuncVarIndex := Func.VarIndex(VarName);

	if ArgIndex <> -1 then begin
		if Ast.IsArrayExpr then
			AssignPopArrItem(Func, Ast, -ArgIndex-cArgsOffset, false)

		else begin
			FCode.AddInstr(ciPopLocalValue, Ast.Token, FDebugInfo);
			FCode.AddInteger(-ArgIndex-cArgsOffset);
		end;
	end
	else if FuncVarIndex <> -1 then begin
		if Ast.IsArrayExpr then
			AssignPopArrItem(Func, Ast, FuncVarIndex, false)

		else begin
			FCode.AddInstr(ciPopLocalValue, Ast.Token, FDebugInfo);
			FCode.AddInteger(FuncVarIndex);
		end;
	end
	else if UserVarIndex <> -1 then begin
		if Ast.IsArrayExpr then
			AssignPopArrItem(Func, Ast, UserVarIndex, true)

		else begin
			FCode.AddInstr(ciPopGlobalValue, Ast.Token, FDebugInfo);
			FCode.AddInteger(UserVarIndex);
		end;
	end
	else
		raise TParserException.CreateFmt(Ast.Token, lngCantGenAssgiForS, [VarName]);
end;

procedure TCodeGenerator.AssignPopArrItem(Func : TScriptFunction; Ast : TASTNode; VarIndex : Integer; VarIsGlobal : Boolean);
var
	i : Integer;
begin
	for i := 1 to Ast.Count-2 do Expr(Func, Ast[i]);
	if VarIsGlobal then
		FCode.AddInstr(ciPopGlobalArray, Ast.Token, FDebugInfo)
	else
		FCode.AddInstr(ciPopLocalArray, Ast.Token, FDebugInfo);
	FCode.AddInteger(VarIndex);
	FCode.AddInteger(Ast.Count-2);
end;

procedure TCodeGenerator.StWhile(Func : TScriptFunction; Ast : TASTNode);
var
	AstCond : TASTNode;
	AstBody : TASTNode;
	AfterLabel : String;
	CondLabel : String;
begin
	AstCond := Ast[0];
	AstBody := Ast[1];

	CondLabel := '$wc_'+Ast.UniqStr;
	AfterLabel := '$wa_'+Ast.UniqStr;

	AddLabel(Ast, strContLabel);
	AddLabel(CondLabel);
	Expr(Func, AstCond);
	FCode.AddInstr(ciJmpIfNot, nil, FDebugInfo);
	AddLabelPos(AfterLabel);
	Statement(Func, AstBody);
	FCode.AddInstr(ciJmp, nil, FDebugInfo);
	AddLabelPos(CondLabel);
	AddLabel(AfterLabel);
	AddLabel(Ast, strBreakLabel);
end;

procedure TCodeGenerator.StIf(Func : TScriptFunction; Ast : TASTNode);
var
	CondAst : TASTNode;
	IfAst : TASTNode;
	ElseAst : TASTNode;
	ElseLabel : String;
	AfterLabel : String;
begin
	CondAst := Ast[0];
	IfAst   := Ast[1];
	ElseAst := Ast[2];

	Elselabel := '$ie_'+Ast.UniqStr;
	Afterlabel := '$ia_'+Ast.UniqStr;

	Expr(Func, CondAst);

	if (IfAst <> nil) and (ElseAst <> nil) then begin // if ... then OPER1 else OPER2;
		FCode.AddInstr(ciJmpIfNot, nil, FDebugInfo);
		AddLabelPos(Elselabel);
		Statement(Func, IfAst);
		FCode.AddInstr(ciJmp, nil, FDebugInfo);
		AddLabelPos(Afterlabel);
		AddLabel(Elselabel);
		Statement(Func, ElseAst);
		AddLabel(Afterlabel);
	end
	else if (IfAst <> nil) and (ElseAst = nil) then begin // if ... then OPER1;
		FCode.AddInstr(ciJmpIfNot, nil, FDebugInfo);
		AddLabelPos(Afterlabel);
		Statement(Func, IfAst);
		AddLabel(Afterlabel);
	end
	else if (IfAst = nil) and (ElseAst <> nil) then begin // if ... then else OPER2;
		FCode.AddInstr(ciJmpIf, nil, FDebugInfo);
		AddLabelPos(Afterlabel);
		Statement(Func, ElseAst);
		AddLabel(Afterlabel);
	end
	else if (IfAst = nil) and (ElseAst = nil) then begin // if ... then;
		FCode.AddInstr(ciDecrStack, nil, FDebugInfo);
		FCode.AddInteger(1);
	end;
end;

procedure TCodeGenerator.StFor(Func : TScriptFunction; Ast : TASTNode);
var
	BeginAst : TASTNode;
	IncAst : TASTNode;
	EndAst : TASTNode;
	BodyAst : TASTNode;
	VarAst : TASTNode;
	CondLabel : String;
	AfterLabel : String;
	Incr : Boolean;
begin
	BeginAst := Ast[0];
	IncAst := Ast[1];
	EndAst := Ast[2];
	BodyAst := Ast[3];
	VarAst := BeginAst[0];
	Incr := IncAst.Token.TokenType = ttTo;
	CondLabel := '$fc_'+Ast.UniqStr;
	AfterLabel := '$fa_'+Ast.UniqStr;

	StAssign(Func, BeginAst);
	AddLabel(CondLabel);
	Expr(Func, VarAst);
	Expr(Func, EndAst);
	if Incr then FCode.AddInstr(ciGreater, nil, FDebugInfo)
	else FCode.AddInstr(ciLess, nil, FDebugInfo);
	FCode.AddInstr(ciJmpIf, nil, FDebugInfo);
	AddLabelPos(AfterLabel);
	FForVariables.Add(VarAst.Token.Text);
	Statement(Func, BodyAst);
	FForVariables.Delete(FForVariables.IndexOf(VarAst.Token.Text));
	AddLabel(Ast, strContLabel);
	IncDec(Func, VarAst, nil, not Incr);
	FCode.AddInstr(ciJmp, nil, FDebugInfo);
	AddLabelPos(CondLabel);
	AddLabel(AfterLabel);
	AddLabel(Ast, strBreakLabel);
end;

procedure TCodeGenerator.StRepeat(Func : TScriptFunction; Ast : TASTNode);
var
	BodyAst : TASTNode;
	CondAst : TASTNode;
	BeginLabel : string;
begin
	BodyAst := Ast[0];
	CondAst := Ast[1];
	BeginLabel := '$rb_'+Ast.UniqStr;

	AddLabel(BeginLabel);
	AddLabel(Ast, strContLabel);
	Statement(Func, BodyAst);
	Expr(Func, CondAst);
	FCode.AddInstr(ciJmpIfNot, nil, FDebugInfo);
	AddLabelPos(BeginLabel);
	AddLabel(Ast, strBreakLabel);
end;

procedure TCodeGenerator.StBreakCont(Func : TScriptFunction; Ast : TASTNode; const Prefix : string);
var
	Token : TToken;
begin
	Token := Ast.Token;
	while Ast <> nil do begin
		if Ast.Token.TokenType in [ttFor, ttWhile, ttUntil] then Break;
		Ast := Ast.Parent;
	end;

	if Ast = nil then
		raise TParserException.CreateFmt(Token, lngOpenSInOutsdCycle, [Token.Text]);

	FCode.AddInstr(ciJmp, nil, FDebugInfo);
	AddLabelPos(Prefix + Ast.UniqStr);
end;

procedure TCodeGenerator.StExit(Func : TScriptFunction; Ast : TASTNode);
begin
	while Ast <> nil do begin
		if Ast.Token.TokenType = ttTry then Break;
		Ast := Ast.Parent;
	end;

	FCode.AddInstr(ciJmp, nil, FDebugInfo);
	if Ast = nil then AddLabelPos(strFuncEnd + Func.Name)
	else AddLabelPos(strFinallyBeg + Ast.UniqStr);
end;

procedure TCodeGenerator.StCase(Func : TScriptFunction; Ast : TASTNode);
var
	ExprAst : TASTNode;
	CaseItem : TASTNode;
	i, j : Integer;
	HasElseBlock : Boolean;
	AfterLabel : String;
const
	LabPrefix = '$cl_';
begin
	AfterLabel := LabPrefix + Ast.UniqStr;

	// Условия
	ExprAst := Ast[0];
	Expr(Func, ExprAst);
	FCode.AddInstr(ciPopAcc, nil, FDebugInfo);
	HasElseBlock := false;
	for i := 1 to Ast.Count-1 do begin
		CaseItem := Ast[i];
		if CaseItem.Token.TokenType = ttSeq then begin
			for j := 0 to CaseItem.Count-2 do begin
				Expr(Func, CaseItem[j]);
				FCode.AddInstr(ciPushAcc, nil, FDebugInfo);
				FCode.AddInstr(ciEqual, nil, FDebugInfo);
				FCode.AddInstr(ciJmpIf, nil, FDebugInfo);
				AddLabelPos(LabPrefix+CaseItem.UniqStr);
			end;
		end
		else if CaseItem.Token.TokenType = ttElse then begin
			FCode.AddInstr(ciJmp, nil, FDebugInfo);
			AddLabelPos(LabPrefix+CaseItem.UniqStr);
			HasElseBlock := true;
		end;
	end;
	if not HasElseBlock then begin
		FCode.AddInstr(ciJmp, nil, FDebugInfo);
		AddLabelPos(AfterLabel);
	end;

	// Операторы
	for i := 1 to Ast.Count-1 do begin
		CaseItem := Ast[i];
		AddLabel(LabPrefix+CaseItem.UniqStr);
		if CaseItem.Token.TokenType = ttSeq then
			Statement(Func, CaseItem[CaseItem.Count-1])
		else
			Statement(Func, CaseItem[0]);
		if i <> Ast.Count-1 then begin
			FCode.AddInstr(ciJmp, nil, FDebugInfo);
			AddLabelPos(AfterLabel);
		end;
	end;
	
	AddLabel(AfterLabel);
end;

procedure TCodeGenerator.StTryFinally(Func : TScriptFunction; Ast : TASTNode);
begin
	raise TParserException.CreateFmt(Ast.Token, 'Try ... Finally not implemented', []);
end;

procedure TCodeGenerator.GenerateExpression(
	Ast           : TASTNode;
	UserFunctions : TUserFunctions;
	UserVars      : TUserVariables;
	UserConsts    : TUserConsts;
	DebugInfo     : TDebugInfo;
	Code          : TBinaryCode
);
begin
	FDebugInfo := DebugInfo;
	FUserConsts := UserConsts;
	FScriptFuncs := nil;
	FUserVars := UserVars;
	FUserFunctions := UserFunctions;
	FCode := Code;

	FLabelsUse.Clear();
	FLabels.Clear();
	FCode.Clear();
	FDebugInfo.Clear();

	SubstituteConsts(Ast);
	OptimizeConsts(Ast);

	Expr(nil, Ast);
	FCode.AddInstr(ciExit, nil, FDebugInfo);

	FillLabelPositions();
end;

procedure TCodeGenerator.Expr(Func : TScriptFunction; Ast : TASTNode);
begin
	if (Ast.Count = 2) and (ltExprOperator in Ast.Token.Flags) then
		BinOperExpr(Func, Ast)

	else if (Ast.Count = 1) and (ltExprOperator in Ast.Token.Flags) then
		UnOperExpr(Func, Ast)

	else if (Ast.Count = 0) and (ltConst in Ast.Token.Flags) then
		ConstExpr(Ast)

	else if (ltName in Ast.Token.Flags) then
		NameExpr(Func, Ast)

	else
		raise TInternalInterpreterException.CreateFmt(
			lngCantGenExprForS,
			[Ast.Token.Text]
		);
end;

procedure TCodeGenerator.NameExpr(Func : TScriptFunction; Ast : TASTNode);
var
	UserVarIndex : Integer;
	FuncVarIndex : Integer;
	UserFuncIndex : Integer;
	ArgIndex : Integer;
	ScriptFuncObj : TScriptFunction;
	Name : String;
begin
	Name := Ast.Token.Text;

	if Func <> nil then ArgIndex := Func.ArgIndex(Name)
	else ArgIndex := -1;

	UserVarIndex := FUserVars.GetIndex(Name);

	if Func<>nil then FuncVarIndex := Func.VarIndex(Name)
	else FuncVarIndex := -1;

	UserFuncIndex := FUserFunctions.IndexByName(Name);

	if FScriptFuncs <> nil then ScriptFuncObj := FScriptFuncs.FindByName(Name)
	else ScriptFuncObj := nil;

	if (ArgIndex <> -1) and (Ast.Count = 0) then begin
		FCode.AddInstr(ciPushLocalValue, nil, FDebugInfo);
		FCode.AddInteger(-ArgIndex-cArgsOffset);
	end

	else if (ArgIndex <> -1) and Ast.IsArrayExpr then
		ArrExpr(Func, Ast, -ArgIndex-cArgsOffset, false)

	else if (FuncVarIndex <> -1) and Ast.IsArrayExpr then
		ArrExpr(Func, Ast, FuncVarIndex, false)

	else if (UserVarIndex <> -1) and Ast.IsArrayExpr then
		ArrExpr(Func, Ast, UserVarIndex, true)

	else if (FuncVarIndex <> -1) and (Ast.Count = 0) then begin
		FCode.AddInstr(ciPushLocalValue, nil, FDebugInfo);
		FCode.AddInteger(FuncVarIndex);
	end

	else if UserFuncIndex <> -1 then begin
		UserFuncCall(Func, Ast, UserFuncIndex);
		FCode.AddInstr(ciPushAcc, Ast.Token, FDebugInfo);
	end

	else if ScriptFuncObj <> nil then begin
		ScriptFuncCall(Func, Ast, ScriptFuncObj);
		FCode.AddInstr(ciPushAcc, Ast.Token, FDebugInfo);
	end

	else if (UserVarIndex <> -1) and (Ast.Count = 0) then begin
		FCode.AddInstr(ciPushGlobalValue, nil, FDebugInfo);
		FCode.AddInteger(UserVarIndex);
	end

	else
		raise TParserException.CreateFmt(
			Ast.Token,
			lngCantGenExprForS,
			[Name]
		);
end;

procedure TCodeGenerator.ArrExpr(Func : TScriptFunction; Ast : TASTNode; VarIndex : Integer; VarIsGlobal : Boolean);
var
	i : Integer;
begin
	for i := 1 to Ast.Count-2 do Expr(Func, Ast[i]);
	if VarIsGlobal then
		FCode.AddInstr(ciPushGlobalArray, Ast.Token, FDebugInfo)
	else
		FCode.AddInstr(ciPushLocalArray, Ast.Token, FDebugInfo);
	FCode.AddInteger(VarIndex);
	FCode.AddInteger(Ast.Count-2);
end;

procedure TCodeGenerator.SomeFuncCall(Func : TScriptFunction; Ast : TASTNode);
var
	UserFuncIndex : Integer;
	ScriptFuncObj : TScriptFunction;
begin
	UserFuncIndex := FUserFunctions.IndexByName(Ast.Token.Text);
	ScriptFuncObj := FScriptFuncs.FindByName(Ast.Token.Text);

	if UserFuncIndex <> -1 then
		UserFuncCall(Func, Ast, UserFuncIndex)

	else if ScriptFuncObj <> nil then
		ScriptFuncCall(Func, Ast, ScriptFuncObj)
		
	else
		raise TParserException.CreateFmt(
			Ast.Token,
			lngSIsNotFunc,
			[Ast.Token.Text]
		);
end;

procedure TCodeGenerator.UserFuncCall(Func : TScriptFunction; Ast : TASTNode; UserFuncIndex : Integer);
var
	i : Integer;
	UserFun : TUserFunction;
begin
	UserFun := FUserFunctions.Items[UserFuncIndex];
	if (UserFun.ArgCount <> -1)
	and (UserFun.ArgCount <> Ast.Count) then
		raise TParserException.CreateFmt(
			Ast.Token,
			lngWrongCntOfArgs,
			[UserFun.ArgCount, Ast.Count, Ast.Token.Text]
		);

	for i := Ast.Count-1 downto 0 do Expr(Func, Ast[i]);
	FCode.AddInstr(ciUserFunc, Ast.Token, FDebugInfo);
	FCode.AddInteger(Ast.Count);
	FCode.AddInteger(UserFuncIndex);
end;

procedure TCodeGenerator.ScriptFuncCall(Func : TScriptFunction; Ast : TASTNode; ScriptFunc : TScriptFunction);
var
	i : Integer;
begin
	if ScriptFunc.ArgsCount <> Ast.Count then
		raise TParserException.CreateFmt(
			Ast.Token,
			lngWrongCntOfArgs,
			[ScriptFunc.ArgsCount, Ast.Count, Ast.Token.Text]
		);

	for i := Ast.Count-1 downto 0 do Expr(Func, Ast[i]);
	FCode.AddInstr(ciCall, nil, FDebugInfo);
	AddLabelPos(ScriptFunc.Name);
	FCode.AddInstr(ciDecrStack, nil, FDebugInfo);
	FCode.AddInteger(Ast.Count);
end;

procedure TCodeGenerator.UnOperExpr(Func : TScriptFunction; Ast : TASTNode);
var
	Child : TASTNode;
begin
	for Child in Ast.Items do Expr(Func, Child);

	case Ast.Token.TokenType of
		ttMinus: FCode.AddInstr(ciNeg, Ast.Token, FDebugInfo);
		ttNot:   FCode.AddInstr(ciNot, Ast.Token, FDebugInfo);
	else
		raise TInternalInterpreterException.CreateFmt(
			lngCantGenUnExptForS,
			[Ast.Token.Text]
		);
	end;
end;

procedure TCodeGenerator.BinOperExpr(Func : TScriptFunction; Ast : TASTNode);
begin
	Expr(Func, Ast[0]);
	Expr(Func, Ast[1]);

	case Ast.Token.TokenType of
		ttNotEq:     begin FCode.AddInstr(ciEqual, Ast.Token, FDebugInfo); FCode.AddInstr(ciNot, Ast.Token, FDebugInfo); end;
		ttMinus:     FCode.AddInstr(ciSub,       Ast.Token, FDebugInfo);
		ttPlus:      FCode.AddInstr(ciAdd,       Ast.Token, FDebugInfo);
		ttDiv:       FCode.AddInstr(ciDiv,       Ast.Token, FDebugInfo);
		ttMult:      FCode.AddInstr(ciMult,      Ast.Token, FDebugInfo);
		ttIDiv:      FCode.AddInstr(ciIDiv,      Ast.Token, FDebugInfo);
		ttMod:       FCode.AddInstr(ciMod,       Ast.Token, FDebugInfo);
		ttShl:       FCode.AddInstr(ciShl,       Ast.Token, FDebugInfo);
		ttShr:       FCode.AddInstr(ciShr,       Ast.Token, FDebugInfo);
		ttPower:     FCode.AddInstr(ciPower,     Ast.Token, FDebugInfo);
		ttOr:        FCode.AddInstr(ciOr,        Ast.Token, FDebugInfo);
		ttAnd:       FCode.AddInstr(ciAnd,       Ast.Token, FDebugInfo);
		ttXor:       FCode.AddInstr(ciXor,       Ast.Token, FDebugInfo);
		ttEquals:    FCode.AddInstr(ciEqual,     Ast.Token, FDebugInfo);
		ttLess:      FCode.AddInstr(ciLess,      Ast.Token, FDebugInfo);
		ttLessEq:    FCode.AddInstr(ciLessEq,    Ast.Token, FDebugInfo);
		ttGreater:   FCode.AddInstr(ciGreater,   Ast.Token, FDebugInfo);
		ttGreaterEq: FCode.AddInstr(ciGreaterEq, Ast.Token, FDebugInfo);
	else
		raise TInternalInterpreterException.CreateFmt(
			lngCantGenBinExptForS,
			[Ast.Token.Text]
		);
	end;
end;

procedure TCodeGenerator.ConstExpr(Ast : TASTNode);
begin
	case Ast.Token.TokenType of
		ttNil:
			FCode.AddInstr(ciPushNil, nil, FDebugInfo);

		ttIntConst: begin
			FCode.AddInstr(ciPushInt, nil, FDebugInfo);
			FCode.AddInteger(Ast.Token.IntValue);
		end;

		ttFloatConst: begin
			FCode.AddInstr(ciPushFloat, nil, FDebugInfo);
			FCode.AddFloat(Ast.Token.FloatValue);
		end;

		ttBoolConst: begin
			FCode.AddInstr(ciPushBool, nil, FDebugInfo);
			FCode.AddBool(Ast.Token.BoolValue);
		end;

		ttStrConst: begin
			FCode.AddInstr(ciPushStr, nil, FDebugInfo);
			AddString(Ast.Token.StrValue);
		end;
	else
		raise TInterpreterException.CreateFmt(
			lngCantGenConstS,
			[Ast.Token.Text]
		);
	end;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TBinOperException }

constructor TBinOperException.Create(const Value1, Value2 : TValue; const Oper : string);
begin
	Inherited CreateFmt(lngCantCalcSSS, [Value1.ToString, Oper, Value2.ToString]);
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TUnaryOperException }

constructor TUnaryOperException.Create(const Value : TValue; const Oper : string);
begin
	Inherited CreateFmt(lngCantCalcSS, [Oper, Value.ToString]);
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TWrongTypeException }
constructor TWrongTypeException.Create(const Value : TValue; const ExpectedType : string);
begin
	Inherited CreateFmt(lngWrongTypeExpSForS, [ExpectedType, Value.ToString]);
end;

constructor TWrongTypeException.Create(const Value : TValueWrapper; const ExpectedType : string);
begin
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TStringValue }

function TStringValue.GetPStr() : PString;
begin
	Result := @FString;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TArrayValue }

constructor TArrayValue.Create();
begin
	FMultiItems := TDictionary<TIntegerDynArray, TValue>.Create;
	FItems := TDictionary<Integer, TValue>.Create;
end;

destructor TArrayValue.Destroy;
begin
	FreeAndNil(FItems);
	FreeAndNil(FMultiItems);
end;

procedure TArrayValue.Clear();
begin
	FItems.Clear();
	FMultiItems.Clear();
end;

function TArrayValue.Count : Integer;
begin
	Result := FItems.Count + FMultiItems.Count;
end;

procedure TArrayValue.ForEach(Proc : TVisitAllValuesProc);
var
	Value : TValue;
begin
	for Value in FItems.Values do Proc(Value);
	for Value in FMultiItems.Values do Proc(Value);
end;

function TArrayValue.GetValue_MultiDim(const Args :  TIntegerDynArray; out Value : TValue) : Boolean;
begin
	Result := FMultiItems.TryGetValue(Args, Value);
end;

procedure TArrayValue.PutValue_MultiDim(const Args :  TIntegerDynArray; const Value : TValue);
begin
	if FMultiItems.ContainsKey(Args) then
		FMultiItems[Args] := Value
	else
		FMultiItems.Add(Args, Value);
end;

function TArrayValue.GetValue_OneDim(Index : Integer; out Value : TValue) : Boolean;
begin
	Result := FItems.TryGetValue(Index, Value);
end;

procedure TArrayValue.PutValue_OneDim(Index : Integer; const Value : TValue);
begin
	if FItems.ContainsKey(Index) then
		FItems[Index] := Value
	else
		FItems.Add(Index, Value);
end;


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TMemoryManager }

constructor TMemoryManager.Create();
begin
	FAllocCounter := 0;
	FObjects := TObjectList<TMemoryManagerObject>.Create();
end;

destructor TMemoryManager.Destroy();
begin
	FreeAndNil(FObjects);
end;

function TMemoryManager.GetString(Stack : PValueDynArray; StkEnd : PValue; GlobalVars : PValueDynArray) : TStringValue;
begin
	TryToCollectGarbage(Stack, StkEnd, GlobalVars);
	result := TStringValue.Create();
	FObjects.Add(result);
end;

function TMemoryManager.GetArray() : TArrayValue;
begin
	result := TArrayValue.Create();
	FObjects.Add(result);
end;

class procedure TMemoryManager.MarkObjectAsUsed(const Value : TValue);
begin
	case Value.Typ of
		vtString:
			Value.Str.Used := true;

		vtArray: begin
			Value.Arr.Used := true;
			Value.Arr.ForEach( procedure (const ArrItem : TValue) begin
				MarkObjectAsUsed(ArrItem);
			end);
		end;
	end;
end;

procedure TMemoryManager.TryToCollectGarbage(Stack : PValueDynArray; StkEnd : PValue; GlobalVars : PValueDynArray);
var
	i : Integer;
	Value : TValue;
	Obj : TMemoryManagerObject;
begin
	Inc(FAllocCounter);
	if (Stack = nil) or (StkEnd = nil) or (GlobalVars = nil) then Exit;
	if FAllocCounter < 1000 then Exit;
	FAllocCounter := 0;

	// Used = false для всех объектов
	for Obj in FObjects do Obj.Used := false;

	// Used = true для используемых объектов
	for i := 0 to Length(GlobalVars^)-1 do
		MarkObjectAsUsed((GlobalVars^)[i]);
	for i := 0 to Length(Stack^)-1 do begin
		Value := (Stack^)[i];
		if @Value = StkEnd then break;
		MarkObjectAsUsed(Value);
	end;

	// Удаляем неиспользуемые объекты
	for i := FObjects.Count-1 downto 0 do begin
		Obj := TMemoryManagerObject(FObjects[i]);
		if Obj.Used then Continue;
		FObjects.Delete(i);
	end;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TCallStackVariable }

constructor TCallStackVariable.Create(const Name : String; Values : PValueDynArray; ValueIndex : Integer; MM : TMemoryManager);
begin
	FName := Name;
	FValueWrapper := TValueWrapper.Create(Values, ValueIndex, MM);
end;

destructor TCallStackVariable.Destroy();
begin
	FreeAndNil(FValueWrapper);
	Inherited;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TCallStackVariables }

constructor TCallStackVariables.Create(Names : TStrings; Values : TValueDynArray; MM : TMemoryManager);
var
	i : Integer;
begin
	FValues := Values;
	FWrappers := TObjectList<TCallStackVariable>.Create();
	for i := 0 to Length(Values)-1 do FWrappers.Add(TCallStackVariable.Create(Names[i], @FValues, i, MM));
end;

destructor TCallStackVariables.Destroy();
begin
	FreeAndNil(FWrappers);
end;

function TCallStackVariables.GetCount : Integer;
begin
	Result := FWrappers.Count;
end;

function TCallStackVariables.GetItem(Index : Integer) : TCallStackVariable;
begin
	Result := FWrappers[Index];
end;

function TCallStackVariables.GetEnumerator() : TList<TCallStackVariable>.TEnumerator;
begin
	Result := FWrappers.GetEnumerator;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TCallStackItem }

constructor TCallStackItem.Create(Fun : TFunDebugInfo; Args : TValueDynArray; Vars : TValueDynArray; MM : TMemoryManager);
begin
	FFun := Fun;
	FArgs := TCallStackVariables.Create(Fun.Args, Args, MM);
	FVars := TCallStackVariables.Create(Fun.Vars, Vars, MM);
end;

destructor TCallStackItem.Destroy();
begin
	FreeAndNil(FVars);
	FreeAndNil(FArgs);
end;

function TCallStackItem.GetFunAndArgsString() : String;
var
	Str : TStringBuilder;
	Value : TCallStackVariable;
	IsFirstArg : Boolean;
begin
	try
		Str := TStringBuilder.Create();
		Str.Append(FFun.FunName);

		if FArgs.Count <> 0 then begin
			Str.Append(' (');
			IsFirstArg := true;
			for Value in FArgs do begin
				if not IsFirstArg then Str.Append(', ');
				Str.Append(Value.Name);
				Str.Append('=');
				Str.Append(Value.ValueWrapper.ToStr);
				IsFirstArg := false;
			end;
			Str.Append(')');
		end;

		Result := Str.ToString;
	finally
		FreeAndNil(Str);
	end;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TCallStackItems }

constructor TCallStackItems.Create();
begin
	FItems := TObjectList<TCallStackItem>.Create();
end;

destructor TCallStackItems.Destroy();
begin
	FreeAndNil(FItems);
end;

procedure TCallStackItems.Add(Item : TCallStackItem);
begin
	FItems.Add(Item);
end;

procedure TCallStackItems.ToStrings(Strings : TStrings);
var
	Item : TCallStackItem;
begin
	for Item in FItems do Strings.Add(Item.GetFunAndArgsString);
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TVirtualMachine }

constructor TVirtualMachine.Create();
begin
	FStoredCodePtr := nil;
	FStoredStackPtr := nil;

	SetLength(FStack, 1);
	FArgs := TUserFunctionArgs.Create();

	SetLength(FAcc, 1);
end;

destructor TVirtualMachine.Destroy();
begin
	FreeAndNil(FArgs);
	FreeAndNil(FAccWrapper);
	Inherited;
end;

function TVirtualMachine.GetStk() : PValueDynArray;
begin
	Result := @FStack;
end;

procedure TVirtualMachine.IncreaseStack(var Stk, StackEnd : PValue; var Loc : PValueArray);
var
	CurPos : Cardinal;
	LocPos : Cardinal;
	NewLen : Cardinal;
begin
	CurPos := (Cardinal(Stk) - Cardinal(@FStack[0])) div sizeof(TValue);
	LocPos := (Cardinal(Loc) - Cardinal(@FStack[0])) div sizeof(TValue);
	NewLen := 2*length(FStack);
	if NewLen <= CurPos then NewLen := 2*CurPos;
	SetLength(FStack, NewLen);
	Stk := @FStack[CurPos];
	StackEnd := PValue(Cardinal(@FStack[0]) + Cardinal(length(FStack)*sizeof(TValue)));
	Loc := PValueArray(Cardinal(@FStack[0]) + LocPos * sizeof(TValue));
end;

class procedure TVirtualMachine.UnaryOperException(const Value : TValue; Oper : PChar);
begin
	raise TUnaryOperException.Create(Value, Oper);
end;

class procedure TVirtualMachine.BinOperException(const Value1, Value2 : TValue; Oper : PChar);
begin
	raise TBinOperException.Create(Value1, Value2, Oper);
end;

class procedure TVirtualMachine.ZeroDivException();
begin
	raise TZeroDivException.Create(lngZeroDiv);
end;

class procedure TVirtualMachine.WrongTypeException(const Value : TValue; ExpectedType : PChar);
begin
	raise TWrongTypeException.Create(Value, ExpectedType);
end;

class procedure TVirtualMachine.UnknownInstrError(InstrCode : Integer);
begin
	Raise TRuntimeException.CreateFmt(lngUnknownInstr, [InstrCode]);
end;

procedure TVirtualMachine.CallUserFun(
	var Stk     : PValue;
	UserFuncs   : TUserFunctions;
	var CodePtr : PBinaryCodeItem;
	MM          : TMemoryManager
);
var
	FuncArgCount : Integer;
	UserFunc : TUserFunction;
	StackPos : Integer;
	i : Integer;
begin
	FuncArgCount := CodePtr.Int;
	Inc(CodePtr);
	UserFunc := UserFuncs.Items[CodePtr.Int];
	Inc(CodePtr);

	StackPos := (Cardinal(Stk) - Cardinal(@FStack[0])) div sizeof(TValue);
	FArgs.Clear();
	for i := 0 to FuncArgCount-1 do begin
		FArgs.Add(@FStack, StackPos, MM);
		Dec(StackPos);
		Dec(Stk);
	end;

	UserFunc.Callback(UserFunc.Context, FArgs, FAccWrapper);
end;

procedure TVirtualMachine.AddStrings(Stk, Stk2, StkEnd : PValue; UserVarValues : PValueDynArray; MM : TMemoryManager);
var
	PrevStr : String;
begin
	PrevStr := Stk.ToString;
	Stk.Str := MM.GetString(@FStack, StkEnd, UserVarValues);
	Stk.Typ := vtString;
	Stk.Str.Value := PrevStr + Stk2.ToString;
end;

class procedure TVirtualMachine.PushArrItemIntoStack(
	var Stack      : PValue;
	const Variable : TValue;
	ArgCount       : Integer;
	var Indexes    : TIntegerDynArray
);
var
	i : Integer;
	Found : Boolean;
begin
	if ArgCount = 1 then begin
		case Variable.Typ of
			vtArray:
				Found := Variable.Arr.GetValue_OneDim(Stack.Int, Stack^);

			vtString,
			vtConstStr: begin
				Found := true;
				Stack^ := Variable.GetChar(Stack.Int);
			end;
		else
			raise TRuntimeException.Create(lngVarInNotArr);
		end;
	end
	else begin
		if Variable.Typ <> vtArray then raise TRuntimeException.Create(lngVarInNotArr);
		SetLength(Indexes, ArgCount);
		for i := 0 to ArgCount-1 do begin
			if Stack.Typ <> vtInt then raise TRuntimeException.Create(lngNoIntArrInd);
			Indexes[i] := Stack.Int;
			Dec(Stack);
		end;
		Inc(Stack);
		Found := Variable.Arr.GetValue_MultiDim(Indexes, Stack^);
	end;

	if not Found then Stack.Typ := vtNil;
end;

class procedure TVirtualMachine.PopArrItemFromStack(
	var Stack    : PValue;
	var Variable : TValue;
	ArgCount     : Integer;
	MM           : TMemoryManager;
	var Indexes  : TIntegerDynArray
);
var
	i : Integer;
	Index : Integer;
begin
	if Variable.Typ <> vtArray then begin
		Variable.Arr := MM.GetArray();
		Variable.Typ := vtArray;
	end;

	if ArgCount = 1 then begin
		Index := Stack.Int;
		Dec(Stack);
		Variable.Arr.PutValue_OneDim(Index, Stack^);
	end

	else begin
		SetLength(Indexes, ArgCount);
		for i := 0 to ArgCount-1 do begin
			if Stack.Typ <> vtInt then raise TRuntimeException.Create(lngNoIntArrInd);
			Indexes[i] := Stack.Int;
			Dec(Stack);
		end;
		Variable.Arr.PutValue_MultiDim(Indexes, Stack^);
	end;

	Dec(Stack);
end;

procedure TVirtualMachine.ClearStoredState();
begin
	FStoredCodePtr := nil;
	FStoredStackPtr := nil;
	FStoredLocPtr := nil;
end;

procedure TVirtualMachine.Execute(
	BinCode           : TBinaryCode;
	var UserVariables : TValueDynArray;
	UserFuncs         : TUserFunctions;
	MM                : TMemoryManager;
	ContinueExec      : Boolean
);
const
	FloatFloat     = (Cardinal(vtFloat) shl 16) or Cardinal(vtFloat);
	IntegerFloat   = (Cardinal(vtInt)   shl 16) or Cardinal(vtFloat);
	FloatInteger   = (Cardinal(vtFloat) shl 16) or Cardinal(vtInt);
	IntegerInteger = (Cardinal(vtInt)   shl 16) or Cardinal(vtInt);
	BoolBool       = (Cardinal(vtBool)  shl 16) or Cardinal(vtBool);
var
	Stk        : PValue;          // Текущий указатель стека
	Stk2       : PValue;          // Вспом. указатель стека (исползуется для бинарных опеарций)
	Loc        : PValueArray;     // Указатель на локальные переменные
	StackEnd   : PValue;          // Указатель на конец стека (чтобы знать, когда надо стек расширить)
	CodePtr    : PBinaryCodeItem; // Указатель на исполняемую инструкцию
	Instr      : TCodeInstr;      // Текущая инструкция
	I          : Integer;
	VarIndex   : Integer;
	Code       : PBinaryCodeItem;
	ArrIndexes : TIntegerDynArray; // Используется для индексов массивов, чтобы не перевыделать каждый раз память
begin
	FErrorPos := -1;

	Code := BinCode.GetInstrBegin;

	if FAccWrapper=nil then
		FAccWrapper := TValueWrapper.Create(@FAcc, 0, MM);

	if not ContinueExec then begin
		Stk := @FStack[0];
		Dec(Stk);
		Loc := @FStack[0];
		CodePtr := Code;
	end
	else begin
		if not Assigned(FStoredStackPtr)
		or not Assigned(FStoredLocPtr)
		or not Assigned(FStoredCodePtr) then raise TCantContinueException.Create(lngCantContNoConext);
		Stk := FStoredStackPtr;
		Loc := FStoredLocPtr;
		CodePtr := FStoredCodePtr;
		ClearStoredState();
	end;

	StackEnd := PValue(Cardinal(@FStack[0]) + Cardinal(length(FStack)*sizeof(TValue)));

	try
		while true do begin
			Instr := CodePtr.Instr;
			Inc(CodePtr);
			case Instr of
// Операции со стеком
				ciPushInt: begin // int const -> stack
					Inc(Stk); if Stk=StackEnd then IncreaseStack(Stk, StackEnd, Loc);
					Stk.Typ := vtInt;
					Stk.Int := CodePtr.Int;
					Inc(CodePtr);
				end;

				ciPushFloat: begin // float const -> stack
					Inc(Stk); if Stk=StackEnd then IncreaseStack(Stk, StackEnd, Loc);
					Stk.Typ := vtFloat;
					Stk.Float := CodePtr.Float;
					Inc(CodePtr);
				end;

				ciPushBool: begin // bool const -> stack
					Inc(Stk); if Stk=StackEnd then IncreaseStack(Stk, StackEnd, Loc);
					Stk.Typ := vtBool;
					Stk.Bool := CodePtr.Bool;
					Inc(CodePtr);
				end;

				ciPushStr: begin // string const -> stack
					Inc(Stk); if Stk=StackEnd then IncreaseStack(Stk, StackEnd, Loc);
					Stk.Typ := vtConstStr;
					Stk.CStr := BinCode.GetString(CodePtr.Int);
					Inc(CodePtr);
				end;

				ciPushNil: begin // nil -> stack
					Inc(Stk); if Stk=StackEnd then IncreaseStack(Stk, StackEnd, Loc);
					Stk^ := NilVal;
				end;

				ciPushLocalValue: begin // local var -> stack
					Inc(Stk); if Stk=StackEnd then IncreaseStack(Stk, StackEnd, Loc);
					Stk^ := PValue(Integer(Loc)+CodePtr.Int*sizeof(TValue))^;
					Inc(CodePtr);
				end;

				ciPopLocalValue: begin // stack -> local var
					PValue(Integer(Loc)+CodePtr.Int*sizeof(TValue))^ := Stk^;
					Dec(Stk);
					Inc(CodePtr);
				end;

				ciPushGlobalValue: begin  // user var -> stack
					Inc(Stk); if Stk=StackEnd then IncreaseStack(Stk, StackEnd, Loc);
					Stk^ := UserVariables[CodePtr.Int];
					Inc(CodePtr);
				end;

				ciPopGlobalValue: begin // stack -> user var
					UserVariables[CodePtr.Int] := Stk^;
					Dec(Stk);
					Inc(CodePtr);
				end;

				ciPushLocalArray: begin // local array item -> stack
					VarIndex := CodePtr.Int;
					Inc(CodePtr);
					PushArrItemIntoStack(Stk, PValue(Integer(Loc)+VarIndex*sizeof(TValue))^, CodePtr.Int, ArrIndexes);
					Inc(CodePtr);
				end;

				ciPopLocalArray: begin // stack -> local array item
					VarIndex := CodePtr.Int;
					Inc(CodePtr);
					PopArrItemFromStack(Stk, PValue(Integer(Loc)+VarIndex*sizeof(TValue))^, CodePtr.Int, MM, ArrIndexes);
					Inc(CodePtr);
				end;

				ciPushGlobalArray: begin // global array item -> stack
					VarIndex := CodePtr.Int;
					Inc(CodePtr);
					PushArrItemIntoStack(Stk, UserVariables[VarIndex], CodePtr.Int, ArrIndexes);
					Inc(CodePtr);
				end;

				ciPopGlobalArray: begin // stack -> global array item
					VarIndex := CodePtr.Int;
					Inc(CodePtr);
					PopArrItemFromStack(Stk, UserVariables[VarIndex], CodePtr.Int, MM, ArrIndexes);
					Inc(CodePtr);
				end;

				ciFunFrameBegin: begin // func start
					Inc(Stk); if Stk=StackEnd then IncreaseStack(Stk, StackEnd, Loc);
					Stk.Int := CodePtr.Int;
					Stk.Typ := vtFunStackFrame;
					Inc(CodePtr);

					Loc := PValueArray(Cardinal(Stk)+sizeof(TValue));
					for i := 1 to CodePtr.Int do begin
						Inc(Stk); if Stk=StackEnd then IncreaseStack(Stk, StackEnd, Loc);
						Stk^ := NilVal;
					end;
					Inc(CodePtr);
				end;

				ciFunFrameEnd: begin // func end
					Dec(Stk, CodePtr.Int);
					if (Stk.Typ <> vtFunStackFrame) then
						raise TStackCorruptedException.Create(lngStackCorrupted);
					Dec(Stk);
					Inc(CodePtr);
				end;

				ciDecrStack: begin // Stk = Stk - const
					Dec(Stk, CodePtr.Int);
					Inc(CodePtr);
				end;

				ciPushAcc: begin // acc -> stack
					Inc(Stk); if Stk=StackEnd then IncreaseStack(Stk, StackEnd, Loc);
					Stk^ := FAcc[0];
				end;

				ciPopAcc: begin // stack -> acc
					FAcc[0] := Stk^;
					Dec(Stk);
				end;

// Бинарные операции

				ciSub: begin { - }
					Stk2 := Stk; Dec(Stk);
					case (Cardinal(Stk.Typ) shl 16) or Cardinal(Stk2.Typ) of
						IntegerInteger:       Stk.Int   := Stk.Int   - Stk2.Int;
						FloatFloat:           Stk.Float := Stk.Float - Stk2.Float;
						FloatInteger:         Stk.Float := Stk.Float - Stk2.Int;
						IntegerFloat:   begin Stk.Float := Stk.Int   - Stk2.Float; Stk.Typ := vtFloat; end;
					else
						 if (Stk.Typ=vtNil) or (Stk2.Typ=vtNil) then Stk.Typ := vtNil
						 else BinOperException(Stk^, Stk2^, strMinus);
					end;
				end;

				ciAdd: begin { + }
					Stk2 := Stk; Dec(Stk);
					case (Cardinal(Stk.Typ) shl 16) or Cardinal(Stk2.Typ) of
						IntegerInteger:       Stk.Int   := Stk.Int   + Stk2.Int;
						FloatFloat:           Stk.Float := Stk.Float + Stk2.Float;
						FloatInteger:         Stk.Float := Stk.Float + Stk2.Int;
						IntegerFloat:   begin Stk.Float := Stk.Int   + Stk2.Float; Stk.Typ := vtFloat; end;
					else
						 if (Stk.Typ=vtNil) or (Stk2.Typ=vtNil) then
							Stk.Typ := vtNil
						 else if (Stk.Typ in [vtString, vtConstStr]) or (Stk2.Typ in [vtString, vtConstStr]) then
							AddStrings(Stk, Stk2, StackEnd, @UserVariables, MM)
						 else
							BinOperException(Stk^, Stk2^, strPlus);
					end;
				end;

				ciMult: begin { * }
					Stk2 := Stk; Dec(Stk);
					case (Cardinal(Stk.Typ) shl 16) or Cardinal(Stk2.Typ) of
						IntegerInteger:       Stk.Int   := Stk.Int   * Stk2.Int;
						FloatFloat:           Stk.Float := Stk.Float * Stk2.Float;
						FloatInteger:         Stk.Float := Stk.Float * Stk2.Int;
						IntegerFloat:   begin Stk.Float := Stk.Int   * Stk2.Float; Stk.Typ := vtFloat; end;
					else
						 if (Stk.Typ=vtNil) or (Stk2.Typ=vtNil) then Stk.Typ := vtNil
						 else BinOperException(Stk^, Stk2^, strMult);
					end;
				end;

				ciDiv: begin { / }
					Stk2 := Stk; Dec(Stk);
					case (Cardinal(Stk.Typ) shl 16) or Cardinal(Stk2.Typ) of
						IntegerInteger: begin
							if Stk2.Int = 0 then ZeroDivException();
							Stk.Float := Stk.Int / Stk2.Int;
							Stk.Typ := vtFloat;
						end;
						FloatFloat: begin
							if Stk2.Float = 0 then ZeroDivException();
							Stk.Float := Stk.Float / Stk2.Float;
						end;
						FloatInteger:
							Stk.Float := Stk.Float / Stk2.Int;
						IntegerFloat: begin
							Stk.Float := Stk.Int / Stk2.Float;
							Stk.Typ := vtFloat;
						end;
					else
						 if (Stk.Typ=vtNil) or (Stk2.Typ=vtNil) then Stk.Typ := vtNil
						 else BinOperException(Stk^, Stk2^, strDiv);
					end;
				end;

				ciIDiv: begin { div }
					Stk2 := Stk; Dec(Stk);
					if      (Stk.Typ = vtInt) and (Stk2.Typ = vtInt) then Stk.Int := Stk.Int div Stk2.Int
					else if (Stk.Typ = vtNil) or  (Stk2.Typ = vtNil) then Stk.Typ := vtNil
					else BinOperException(Stk^, Stk2^, strIDiv);
				end;

				ciMod: begin { mod }
					Stk2 := Stk; Dec(Stk);
					if      (Stk.Typ = vtInt) and (Stk2.Typ = vtInt) then Stk.Int := Stk.Int mod Stk2.Int
					else if (Stk.Typ = vtNil) or  (Stk2.Typ = vtNil) then Stk.Typ := vtNil
					else BinOperException(Stk^, Stk2^, strMod);
				end;

				ciShl: begin { shl }
					Stk2 := Stk; Dec(Stk);
					if      (Stk.Typ = vtInt) and (Stk2.Typ = vtInt) then Stk.Int := Stk.Int shl Stk2.Int
					else if (Stk.Typ = vtNil) or  (Stk2.Typ = vtNil) then Stk.Typ := vtNil
					else BinOperException(Stk^, Stk2^, strShl);
				end;

				ciShr: begin { shr }
					Stk2 := Stk; Dec(Stk);
					if      (Stk.Typ = vtInt) and (Stk2.Typ = vtInt) then Stk.Int := Stk.Int shr Stk2.Int
					else if (Stk.Typ = vtNil) or  (Stk2.Typ = vtNil) then Stk.Typ := vtNil
					else BinOperException(Stk^, Stk2^, strShr);
				end;


				ciPower: begin { ^ }
					Stk2 := Stk; Dec(Stk);
					case (Cardinal(Stk.Typ) shl 16) or Cardinal(Stk2.Typ) of
						IntegerInteger: begin Stk.Float := Power(Stk.Int,   Stk2.Int  ); Stk.Typ := vtFloat; end;
						FloatFloat:           Stk.Float := Power(Stk.Float, Stk2.Float);
						FloatInteger:         Stk.Float := Power(Stk.Float, Stk2.Int  );
						IntegerFloat:   begin Stk.Float := Power(Stk.Int,   Stk2.Float); Stk.Typ := vtFloat; end;
					else
						 if (Stk.Typ=vtNil) or (Stk2.Typ=vtNil) then Stk.Typ := vtNil
						 else BinOperException(Stk^, Stk2^, strPower);
					end;
				end;

				ciOr: begin { or }
					Stk2 := Stk; Dec(Stk);
					case (Cardinal(Stk.Typ) shl 16) or Cardinal(Stk2.Typ) of
						IntegerInteger: Stk.Int  := Stk.Int  or Stk2.Int;
						BoolBool:       Stk.Bool := Stk.Bool or Stk2.Bool;
					else
						if (Stk.Typ=vtNil) or (Stk2.Typ=vtNil) then Stk.Typ := vtNil
						else BinOperException(Stk^, Stk2^, strOr);
					end;
				end;

				ciAnd: begin { and }
					Stk2 := Stk; Dec(Stk);
					case (Cardinal(Stk.Typ) shl 16) or Cardinal(Stk2.Typ) of
						IntegerInteger: Stk.Int  := Stk.Int and Stk2.Int;
						BoolBool:       Stk.Int  := Stk.Int and Stk2.Int;
					else
						if (Stk.Typ=vtNil) or (Stk2.Typ=vtNil) then Stk.Typ := vtNil
						else BinOperException(Stk^, Stk2^, strAnd);
					end;
				end;

				ciXor: begin { xor }
					Stk2 := Stk; Dec(Stk);
					case (Cardinal(Stk.Typ) shl 16) or Cardinal(Stk2.Typ) of
						IntegerInteger: Stk.Int  := Stk.Int xor Stk2.Int;
						BoolBool:       Stk.Int  := Stk.Int xor Stk2.Int;
					else
						if (Stk.Typ=vtNil) or (Stk2.Typ=vtNil) then Stk.Typ := vtNil
						else BinOperException(Stk^, Stk2^, strXor);
					end;
				end;

				ciEqual: begin { = }
					Stk2 := Stk; Dec(Stk);
					case (Cardinal(Stk.Typ) shl 16) or Cardinal(Stk2.Typ) of
						IntegerInteger: Stk.Bool := Stk.Int   = Stk2.Int;
						FloatFloat:     Stk.Bool := Stk.Float = Stk2.Float;
						FloatInteger:   Stk.Bool := Stk.Float = Stk2.Int;
						IntegerFloat:   Stk.Bool := Stk.Int   = Stk2.Float;
						BoolBool:       Stk.Bool := Stk.Bool  = Stk2.Bool;
					else
						if (Stk.Typ=vtNil) or (Stk2.Typ=vtNil) then begin Stk.Typ := vtNil; continue; end
						else if (Stk.Typ in [vtString, vtConstStr]) or (Stk2.Typ in [vtString, vtConstStr]) then
							Stk.Bool := Stk.ToString = Stk2.ToString
						else
							BinOperException(Stk^, Stk2^, strEquals);
					end;
					Stk.Typ := vtBool;
				end;

				ciLess: begin { < }
					Stk2 := Stk; Dec(Stk);
					case (Cardinal(Stk.Typ) shl 16) or Cardinal(Stk2.Typ) of
						IntegerInteger: Stk.Bool := Stk.Int   < Stk2.Int;
						FloatFloat:     Stk.Bool := Stk.Float < Stk2.Float;
						FloatInteger:   Stk.Bool := Stk.Float < Stk2.Int;
						IntegerFloat:   Stk.Bool := Stk.Int   < Stk2.Float;
						BoolBool:       Stk.Bool := Stk.Bool  < Stk2.Bool;
					else
						if (Stk.Typ=vtNil) or (Stk2.Typ=vtNil) then begin Stk.Typ := vtNil; continue; end
						else if (Stk.Typ in [vtString, vtConstStr]) or (Stk2.Typ in [vtString, vtConstStr]) then
							Stk.Bool := Stk.ToString < Stk2.ToString
						else
							BinOperException(Stk^, Stk2^, strLess);
					end;
					Stk.Typ := vtBool;
				end;

				ciLessEq: begin { <= }
					Stk2 := Stk; Dec(Stk);
					case (Cardinal(Stk.Typ) shl 16) or Cardinal(Stk2.Typ) of
						IntegerInteger: Stk.Bool := Stk.Int   <= Stk2.Int;
						FloatFloat:     Stk.Bool := Stk.Float <= Stk2.Float;
						FloatInteger:   Stk.Bool := Stk.Float <= Stk2.Int;
						IntegerFloat:   Stk.Bool := Stk.Int   <= Stk2.Float;
						BoolBool:       Stk.Bool := Stk.Bool  <= Stk2.Bool;
					else
						if (Stk.Typ=vtNil) or (Stk2.Typ=vtNil) then begin Stk.Typ := vtNil; continue; end
						else if (Stk.Typ in [vtString, vtConstStr]) or (Stk2.Typ in [vtString, vtConstStr]) then
							Stk.Bool := Stk.ToString <= Stk2.ToString
						else
							BinOperException(Stk^, Stk2^, strLessEq);
					end;
					Stk.Typ := vtBool;
				end;

				ciGreater: begin { > }
					Stk2 := Stk; Dec(Stk);
					case (Cardinal(Stk.Typ) shl 16) or Cardinal(Stk2.Typ) of
						IntegerInteger: Stk.Bool := Stk.Int   > Stk2.Int;
						FloatFloat:     Stk.Bool := Stk.Float > Stk2.Float;
						FloatInteger:   Stk.Bool := Stk.Float > Stk2.Int;
						IntegerFloat:   Stk.Bool := Stk.Int   > Stk2.Float;
						BoolBool:       Stk.Bool := Stk.Bool  > Stk2.Bool;
					else
						if (Stk.Typ=vtNil) or (Stk2.Typ=vtNil) then begin Stk.Typ := vtNil; continue; end
						else if (Stk.Typ in [vtString, vtConstStr]) or (Stk2.Typ in [vtString, vtConstStr]) then
							Stk.Bool := Stk.ToString > Stk2.ToString
						else
							BinOperException(Stk^, Stk2^, strGreater);
					end;
					Stk.Typ := vtBool;
				end;

				ciGreaterEq: begin { >= }
					Stk2 := Stk; Dec(Stk);
					case (Cardinal(Stk.Typ) shl 16) or Cardinal(Stk2.Typ) of
						IntegerInteger: Stk.Bool := Stk.Int   >= Stk2.Int;
						FloatFloat:     Stk.Bool := Stk.Float >= Stk2.Float;
						FloatInteger:   Stk.Bool := Stk.Float >= Stk2.Int;
						IntegerFloat:   Stk.Bool := Stk.Int   >= Stk2.Float;
						BoolBool:       Stk.Bool := Stk.Bool  >= Stk2.Bool;
					else
						if (Stk.Typ=vtNil) or (Stk2.Typ=vtNil) then begin Stk.Typ := vtNil; continue; end
						else if (Stk.Typ in [vtString, vtConstStr]) or (Stk2.Typ in [vtString, vtConstStr]) then
							Stk.Bool := Stk.ToString >= Stk2.ToString
						else
							BinOperException(Stk^, Stk2^, strGreaterEq);
					end;
					Stk.Typ := vtBool;
				end;

// Унарные операции

				ciNot: begin { not }
					if Stk.Typ = vtBool then Stk.Bool := not Stk.Bool
					else UnaryOperException(Stk^, strNot);
				end;

				ciNeg: begin { - }
					case Stk.Typ of
						vtInt:   Stk.Int   := -Stk.Int;
						vtFloat: Stk.Float := -Stk.Float;
					else
						UnaryOperException(Stk^, strNot);
					end;
				end;

// Управление потоком выполнения

				ciJmp:
					CodePtr := PBinaryCodeItem(Integer(Code) + CodePtr.Int * sizeof(TBinaryCodeItem));

				ciJmpIf: begin
					if Stk.Typ <> vtBool then WrongTypeException(Stk^, strBoolean);
					if Stk.Bool then
						CodePtr := PBinaryCodeItem(Integer(Code) + CodePtr.Int * sizeof(TBinaryCodeItem))
					else
						Inc(CodePtr);
					Dec(Stk);
				end;

				ciJmpIfNot: begin
					if Stk.Typ <> vtBool then WrongTypeException(Stk^, strBoolean);
					if not Stk.Bool then
						CodePtr := PBinaryCodeItem(Integer(Code) + CodePtr.Int * sizeof(TBinaryCodeItem))
					else
						Inc(CodePtr);
					Dec(Stk);
				end;

				ciReturn: begin
					Loc := @FStack[Stk.Int];
					Dec(Stk);
					CodePtr := PBinaryCodeItem(Integer(Code) + Stk.Int * sizeof(TBinaryCodeItem));
					Dec(Stk);
				end;

				ciCall: begin
					Inc(Stk); if Stk = StackEnd then IncreaseStack(Stk, StackEnd, Loc);
					Stk.Int := (Cardinal(CodePtr)-Cardinal(Code)) div sizeof(TBinaryCodeItem) + 1;
					Stk.Typ := vtInt;
					Inc(Stk); if Stk = StackEnd then IncreaseStack(Stk, StackEnd, Loc);
					Stk.Int := (Cardinal(@Loc[0])-Cardinal(@FStack[0])) div sizeof(TValue);
					Stk.Typ := vtInt;
					CodePtr := PBinaryCodeItem(Integer(Code) + CodePtr.Int * sizeof(TBinaryCodeItem));
				end;

				ciUserFunc:
					CallUserFun(Stk, UserFuncs, CodePtr, MM);

				ciExit: break;

				ciBreakpoint: begin
					Dec(CodePtr);
					BinCode.RevertInstructionForBreakpoint((Cardinal(CodePtr)-Cardinal(Code)) div sizeof(TBinaryCodeItem));
					FStoredCodePtr  := CodePtr;
					FStoredStackPtr := Stk;
					FStoredLocPtr   := Loc;
					break;
				end;
			else
				UnknownInstrError(Integer(Instr));
			end;

			if ContinueExec then begin
				ContinueExec := false;
				BinCode.SetInstructionsForAllBreakpoints();
			end;
		end;
	except
		FErrorPos := (Cardinal(CodePtr)-Cardinal(Code)) div sizeof(TBinaryCodeItem) - 1;
		Raise;
	end;
end;

procedure TVirtualMachine.GetCallStack(DebugInfo : TDebugInfo; MM : TMemoryManager; Result : TCallStackItems);
var
	i : Integer;
	Stk : PValue;
	Loc : PValue;
	Item : TCallStackItem;
	Args : TValueDynArray;
	Vars : TValueDynArray;
	Fun : TFunDebugInfo;
begin
	if FStoredStackPtr = nil then Exit;
	Stk := FStoredStackPtr;
	while Cardinal(Stk) >= Cardinal(@FStack[0]) do begin
		if Stk.Typ = vtFunStackFrame then begin

			Loc := PValue(Cardinal(Stk) + sizeof(TValue));

			Fun := DebugInfo.GetFunDebugItem(Stk.Int);

			SetLength(Vars, Fun.Vars.Count);
			for i := 0 to Fun.Vars.Count-1 do Vars[i] := PValue(Integer(Loc) + i*sizeof(TValue))^;

			SetLength(Args, Fun.Args.Count);
			for i := 0 to Fun.Args.Count-1 do Args[i] := PValue(Integer(Loc) - (i+cArgsOffset)*sizeof(TValue))^;

			Item := TCallStackItem.Create(Fun, Args, Vars, MM);

			Result.Add(Item);
		end;
		Dec(Stk);
	end;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

{ Класс TInterpreter }

constructor TInterpreter.Create();
begin
	FUserFunctions  := TUserFunctions.Create();
	FVirtualMachine := TVirtualMachine.Create();
	FBinaryCode     := TBinaryCode.Create();
	FMemoryManager  := TMemoryManager.Create();
	FResult         := TValueWrapper.Create(FVirtualMachine.GetStk(), 0, FMemoryManager);
	FUserVariables  := TUserVariables.Create(@FUserVarValues, FMemoryManager);
	FUserConsts     := TUserConsts.Create();
	FDebugInfo      := TDebugInfo.Create();
end;

destructor TInterpreter.Destroy();
begin
	FreeAndNil(FDebugInfo);
	FreeAndNil(FUserConsts);
	FreeAndNil(FUserVariables);
	FreeAndNil(FResult);
	FreeAndNil(FMemoryManager);
	FreeAndNil(FBinaryCode);
	FreeAndNil(FVirtualMachine);
	FreeAndNil(FUserFunctions);
	Inherited;
end;

procedure TInterpreter.CompileScript(const Source : String);
var
	Tockenizer : TTokenizer;
	Parser : TParser;
	Generator : TCodeGenerator;
	Functions : TScriptFunctions;
begin
	Tockenizer := TTokenizer.Create;
	Parser := TParser.Create;
	Generator := TCodeGenerator.Create;
	Functions := TScriptFunctions.Create;

	try
		FVirtualMachine.ClearStoredState();

		// Text -> Tokens
		Tockenizer.Tockenize(Source, FUserConsts);

		// Tokens -> AST
		Parser.ParseScript(Tockenizer, Functions);

		// AST -> Bytecode
		Generator.GenerateFunctions(Functions, FUserFunctions, FUserVariables, FUserConsts, FDebugInfo, FBinaryCode);
	finally
		FreeAndNil(Functions);
		FreeAndNil(Generator);
		FreeAndNil(Parser);
		FreeAndNil(Tockenizer);
	end;
end;

procedure TInterpreter.CompileExpression(const Source : String);
var
	Tockenizer : TTokenizer;
	Parser : TParser;
	Generator : TCodeGenerator;
	Expression : TASTNode;
begin
	Tockenizer := TTokenizer.Create;
	Parser := TParser.Create;
	Generator := TCodeGenerator.Create;
	Expression := TASTNode.Create(Parser, nil, nil);

	try
		FVirtualMachine.ClearStoredState();

		// Text -> Tokens
		Tockenizer.Tockenize(Source, FUserConsts);

		// Tokens -> AST
		Parser.ParseExpression(Tockenizer, Expression);

		// AST -> Bytecode
		Generator.GenerateExpression(Expression, FUserFunctions, FUserVariables, FUserConsts, FDebugInfo, FBinaryCode);
	finally
		FreeAndNil(Generator);
		FreeAndNil(Parser);
		FreeAndNil(Tockenizer);
	end;
end;

procedure TInterpreter.CompileScriptFile(const FileName : String);
var
	FileContent : TStringList;
begin
	FileContent := TStringList.Create();
	try
		FileContent.LoadFromFile(FileName);
		CompileScript(FileContent.Text);
	finally
		FreeAndNil(FileContent);
	end;
end;

procedure TInterpreter.Execute(Continue : Boolean);
var
	ErrCodePos : TSrcPos;
begin
	FErrorCol := -1;
	FErrorLine := -1;
	try
		FVirtualMachine.Execute(FBinaryCode, FUserVarValues, FUserFunctions, FMemoryManager, Continue);
	except
		ErrCodePos := FDebugInfo.GetSrcPosByAddr(FVirtualMachine.ErrorPos);
		if ErrCodePos.Col <> -1 then begin
			FErrorCol := ErrCodePos.Col;
			FErrorLine := ErrCodePos.Line;
		end;
		Raise;
    end;
end;

procedure TInterpreter.AddBreakpoint(LineNum : Integer);
begin
	FBinaryCode.AddBreakPoint(LineNum, FDebugInfo);
end;

procedure TInterpreter.RemoveBreakpoint(LineNum : Integer);
begin
	FBinaryCode.RemoveBreakpoint(LineNum, FDebugInfo);
end;

procedure TInterpreter.Disassemble(Text : TStrings);
begin
	FBinaryCode.Disassemble(Text, FDebugInfo);
end;

procedure TInterpreter.GetCallStack(Result : TCallStackItems);
begin
	FVirtualMachine.GetCallStack(FDebugInfo, FMemoryManager, Result);
end;

procedure TInterpreter.SaveToFile(const FileName : String);
var
	FileStream : TFileStream;
	LZip: TZCompressionStream;
begin
	FileStream := TFileStream.Create(FileName, fmCreate);
	LZip := TZCompressionStream.Create(clMax, FileStream);
	try
		FBinaryCode.SaveToStream(LZip);
	finally
		FreeAndNil(LZip);
		FreeAndNil(FileStream);
	end;
end;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

end.
