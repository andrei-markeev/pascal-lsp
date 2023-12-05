unit ReservedWord;

{$mode objfpc}
{$longstrings on}

interface

uses
    strings, CompilationMode, ParserContext, Token;

const
    NUM_OF_RESERVED_WORDS = 93;

type
    TReservedWordKind = (
        rwUnknown, rwInvalid,
        { standard pascal reserved words }
        rwAnd, rwArray,
        rwBegin,
        rwCase, rwConst,
        rwDiv, rwDo, rwDownto,
        rwElse, rwEnd,
        rwFile, rwFor, rwFunction,
        rwGoto,
        rwIf, rwIn,
        rwLabel,
        rwMod,
        rwNil, rwNot,
        rwOf, rwOr,
        rwPacked, rwProcedure, rwProgram,
        rwRecord, rwRepeat,
        rwSet,
        rwThen, rwTo, rwType,
        rwUntil,
        rwVar,
        rwWhile, rwWith,
        { turbo pascal reserved words}
        rwAbsolute, rwAsm,
        rwConstructor,
        rwDestructor,
        rwImplementation, rwInherited, rwInline, rwInterface,
        rwObject, rwOperator,
        rwReintroduce,
        rwSelf, rwShl, rwShr, rwString,
        rwUnit, rwUses,
        rwXor,
        { object pascal reserved words }
        rwAs, rwClass, rwDispinterface, rwExcept, rwExports, rwFinalization, rwFinally, rwInitialization,
        rwIs, rwLibrary, rwOn, rwOut, rwProperty, rwRaise, rwResourcestring, rwThreadvar, rwTry,
        { special symbols }
        rwAssign, rwPlus, rwMinus, rwMultiply, rwDivide, rwHat,
        rwEquals, rwNotEqual, rwLess, rwMore, rwLessOrEqual, rwMoreOrEqual,
        rwOpenParenthesis, rwCloseParenthesis, rwOpenSquareBracket, rwCloseSquareBracket,
        rwDot, rwComma, rwColon, rwSemiColon, rwRange
    );
    TReservedWord = class(TToken)
    public
        kind: TReservedWordKind;
        constructor Create(ctx: TParserContext; expectedKind: TReservedWordKind; peeked: boolean);
        destructor Destroy; override;
    end;

    function DetermineReservedWord(ctx: TParserContext): TReservedWordKind;
    function PeekReservedWord(ctx: TParserContext; kind: TReservedWordKind): boolean;
    function PeekReservedWord(ctx: TParserContext; expected: string): boolean; inline;

implementation

const
    ReservedWords: array [0..NUM_OF_RESERVED_WORDS - 1] of shortstring = (
        '', '',
        { standard pascal reserved words }
        'and', 'array',
        'begin',
        'case', 'const',
        'div', 'do', 'downto',
        'else', 'end',
        'file', 'for', 'function',
        'goto',
        'if', 'in',
        'label', 
        'mod',
        'nil', 'not',
        'of', 'or', 
        'packed', 'procedure', 'program',
        'record', 'repeat',
        'set',
        'then', 'to', 'type',
        'until',
        'var',
        'while', 'with',
        { turbo pascal reserved words }
        'absolute', 'asm',
        'constructor',
        'destructor',
        'implementation', 'inherited', 'inline', 'interface',
        'object', 'operator',
        'reintroduce',
        'self', 'shl', 'shr', 'string',
        'unit', 'uses',
        'xor',
        { object pascal reserved words }
        'as', 'class', 'dispinterface', 'except', 'exports', 'finalization', 'finally', 'initialization',
        'is', 'library', 'on', 'out', 'property', 'raise', 'resourcestring', 'threadvar', 'try',
        { special symbols }
        ':=', '+', '-', '*', '/', '^',
        '=', '<>', '<', '>', '<=', '>=',
        '(', ')', '[', ']',
        '.', ',', ':', ';', '..'
    );

function IsTurboPascalReservedWord(rwKind: TReservedWordKind): boolean; inline;
begin
    IsTurboPascalReservedWord := rwKind in [
        rwAbsolute, rwAsm,
        rwConstructor,
        rwDestructor,
        rwImplementation, rwInherited, rwInline, rwInterface,
        rwObject, rwOperator,
        rwReintroduce,
        rwSelf, rwShl, rwShr, rwString,
        rwUnit, rwUses,
        rwXor
    ];
end;

function IsObjectPascalReservedWord(rwKind: TReservedWordKind): boolean; inline;
begin
    IsObjectPascalReservedWord := rwKind in [
        rwAs, rwClass, rwDispinterface, rwExcept, rwExports, rwFinalization, rwFinally, rwInitialization,
        rwIs, rwLibrary, rwOn, rwOut, rwProperty, rwRaise, rwResourcestring, rwThreadvar, rwTry
    ];
end;

function PeekReservedWord(ctx: TParserContext; expected: string): boolean; inline;
var
    separated: boolean;
begin
    ctx.SkipTrivia;
    separated := not (expected[1] in ['a'..'z']) or ctx.IsSeparator(ctx.Cursor[length(expected)]);
    PeekReservedWord := (strlicomp(ctx.Cursor, PChar(expected), length(expected)) = 0) and separated;
end;

function PeekReservedWord(ctx: TParserContext; kind: TReservedWordKind): boolean;
begin
    PeekReservedWord := PeekReservedWord(ctx, ReservedWords[ord(kind)]);
end;

function DetermineReservedWord(ctx: TParserContext): TReservedWordKind;
var
    maybe, found: TReservedWordKind;
begin
    ctx.SkipTrivia;
    if not (ctx.Cursor[0] in ['A'..'Z','a'..'z']) then
    begin
        case ctx.Cursor[0] of
            ':':
                if ctx.Cursor[1] = '=' then found := rwAssign
                else found := rwColon;
            '+': found := rwPlus;
            '-': found := rwMinus;
            '*': found := rwMultiply;
            '/': found := rwDivide;
            '^': found := rwHat;
            '=': found := rwEquals;
            '<':
                if ctx.Cursor[1] = '=' then found := rwLessOrEqual
                else found := rwLess;
            '>':
                if ctx.Cursor[1] = '=' then found := rwMoreOrEqual
                else found := rwMore;
            '(': found := rwOpenParenthesis;
            ')': found := rwCloseParenthesis;
            '[': found := rwOpenSquareBracket;
            ']': found := rwCloseSquareBracket;
            '.':
                if ctx.Cursor[1] = '.' then found := rwRange
                else found := rwDot;
            ',': found := rwComma;
            ';': found := rwSemiColon;
        else
            found := rwInvalid;
        end;
        DetermineReservedWord := found;
        exit;
    end;

    found := rwUnknown;
    maybe := rwUnknown;
    case ctx.Cursor[0] of
        'a','A':
            case ctx.Cursor[1] of
                'b','B': maybe := rwAbsolute;
                'n','N': maybe := rwAnd;
                'r','R': maybe := rwArray;
                's','S':
                    if (ctx.Cursor[2] = 'm') and ctx.IsSeparator(ctx.Cursor[2]) then found := rwAsm
                    else if ctx.IsSeparator(ctx.Cursor[2]) then found := rwAs;
            end;
        'b','B': maybe := rwBegin;
        'c','C':
            case ctx.Cursor[1] of
                'a','A': maybe := rwCase;
                'l','L': maybe := rwClass;
                'o','O':
                    if (ctx.Cursor[2] in ['n','N']) and (ctx.Cursor[3] in ['s','S']) and (ctx.Cursor[4] in ['t','T']) then
                        if ctx.Cursor[5] in ['r','R'] then maybe := rwConstructor
                        else if ctx.IsSeparator(ctx.Cursor[5]) then found := rwConst;
            end;
        'd','D':
            case ctx.Cursor[1] of
                'e','E': maybe := rwDestructor;
                'i','I': if ctx.Cursor[2] in ['s','S'] then maybe := rwDispinterface else maybe := rwDiv;
                'o','O': if ctx.Cursor[2] in ['w','W'] then maybe := rwDownto else maybe := rwDo;
            end;
        'e','E':
            case ctx.Cursor[1] of
                'l','L': maybe := rwElse;
                'n','N': maybe := rwEnd;
                'x','X': if ctx.Cursor[2] in ['c','C'] then maybe := rwExcept else maybe := rwExports;
            end;
        'f','F':
            case ctx.Cursor[1] of
                'i','I':
                    if ctx.Cursor[2] in ['l','L'] then maybe := rwFile
                    else if (ctx.Cursor[2] in ['n','N']) and (ctx.Cursor[3] in ['a','A']) and (ctx.Cursor[4] in ['l','L']) then
                        if ctx.Cursor[5] in ['i','I'] then maybe := rwFinalization
                        else if ctx.Cursor[6] in ['l','L'] then maybe := rwFinally;
                'o','O': maybe := rwFor;
                'u','U': maybe := rwFunction;
            end;
        'g','G': maybe := rwGoto;
        'i','I':
            case ctx.Cursor[1] of
                'f','F': if ctx.IsSeparator(ctx.Cursor[2]) then found := rwIf;
                'm','M': maybe := rwImplementation;
                'n','N':
                    if ctx.IsSeparator(ctx.Cursor[2]) then found := rwIn
                    else
                        case ctx.Cursor[2] of
                            'h','H': maybe := rwInherited;
                            'i','I': maybe := rwInitialization;
                            'l','L': maybe := rwInline;
                            't','T': maybe := rwInterface;
                        end;
                's','S': if ctx.IsSeparator(ctx.Cursor[2]) then found := rwIs;
            end;
        'l','L':
            case ctx.Cursor[1] of
                'a','A': maybe := rwLabel;
                'i','I': maybe := rwLibrary;
            end;
        'm','M': maybe := rwMod;
        'n','N':
            case ctx.Cursor[1] of
                'i','I': maybe := rwNil;
                'o','O': maybe := rwNot;
            end;
        'o','O':
            case ctx.Cursor[1] of
                'f','F': if ctx.IsSeparator(ctx.Cursor[2]) then found := rwOf;
                'n','N': if ctx.IsSeparator(ctx.Cursor[2]) then found := rwOn;
                'p','P': maybe := rwOperator;
                'r','R': if ctx.IsSeparator(ctx.Cursor[2]) then found := rwOr;
                'u','U': maybe := rwOut;
            end;
        'p','P':
            case ctx.Cursor[1] of
                'a','A': maybe := rwPacked;
                'r','R': if ctx.Cursor[2] in ['o','O'] then
                    case ctx.Cursor[3] of
                        'c','C': maybe := rwProcedure;
                        'g','G': maybe := rwProgram;
                        'p','P': maybe := rwProperty;
                    end;
            end;
        'r','R':
            case ctx.Cursor[1] of
                'a','A': maybe := rwRaise;
                'e','E':
                    case ctx.Cursor[2] of
                        'c','C': maybe := rwRecord;
                        'i','I': maybe := rwReintroduce;
                        's','S': maybe := rwResourcestring;
                        'p','P': maybe := rwRepeat;
                    end;
            end;
        's','S':
            case ctx.Cursor[1] of
                'e','E':
                    case ctx.Cursor[2] of
                        'l','L': maybe := rwSelf;
                        't','T': if ctx.IsSeparator(ctx.Cursor[3]) then found := rwSet;
                    end;
                'h','H':
                    case ctx.Cursor[2] of
                        'l','L': if ctx.IsSeparator(ctx.Cursor[3]) then found := rwShl;
                        'r','R': if ctx.IsSeparator(ctx.Cursor[3]) then found := rwShr;
                    end;
                't','T': maybe := rwString;
            end;
        't','T':
            case ctx.Cursor[1] of
                'h','H': if ctx.Cursor[2] in ['r','R'] then maybe := rwThreadvar else maybe := rwThen;
                'o','O': if ctx.IsSeparator(ctx.Cursor[2]) then found := rwTo;
                'r','R': maybe := rwTry;
                'y','Y': maybe := rwType;
            end;
        'u','U':
            case ctx.Cursor[1] of
                'n','N': if ctx.Cursor[2] in ['i','I'] then maybe := rwUnit else maybe := rwUntil;
                's','S': maybe := rwUses;
            end;
        'v','V': maybe := rwVar;
        'w','W':
            case ctx.Cursor[1] of
                'h','H': maybe := rwWhile;
                'i','I': maybe := rwWith;
            end;
        'x','X': maybe := rwXor;
    end;

    if (ctx.mode < cmTurboPascal) and IsTurboPascalReservedWord(maybe) then
        maybe := rwUnknown;

    if (ctx.mode < cmObjectFreePascal) and IsObjectPascalReservedWord(maybe) then
        maybe := rwUnknown;

    if (maybe <> rwUnknown) and PeekReservedWord(ctx, ReservedWords[ord(maybe)]) then
        found := maybe;

    DetermineReservedWord := found;

end;

constructor TReservedWord.Create(ctx: TParserContext; expectedKind: TReservedWordKind; peeked: boolean);
var
    expected: shortstring;
begin
    kind := expectedKind;
    expected := ReservedWords[ord(kind)];
    tokenName := 'RW';
    isPrimitive := true;

    if not peeked then
    begin
        if not PeekReservedWord(ctx, expected) then
        begin
            state := tsMissing;
            start := ctx.cursorBeforeTrivia;
            len := 0;
            tokenName := 'RW ''' + expected + '''';
            ctx.Add(Self);
            exit;
        end;
    end;

    start := ctx.Cursor;
    len := length(expected);
    state := tsCorrect;
    inc(ctx.Cursor, len);
    ctx.Add(Self);

end;

destructor TReservedWord.Destroy;
begin
end;

end.
