unit RoutineEquivalence;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token, TypedToken, TypeDef, TypeDefs, Identifier, Modifiers, Symbols, RoutineTypeDef;

function GetCallingConvStr(const mods: TFunctionModifiers): string;

function MatchCandidateEquivalence(
    declRoutine: TRoutineTypeDef;
    declKind: TSymbolKind;
    symbolKind: TSymbolKind;
    routineTypeDef: TRoutineTypeDef;
    hasOpenParenthesis: boolean;
    funcModifiers: TFunctionModifiers;
    methodModifiers: TMethodModifiers;
    out errorMsg: string
): boolean;

procedure VerifyImplementationEquivalence(
    declSymbol: TSymbol;
    symbolKind: TSymbolKind;
    routineTypeDef: TRoutineTypeDef;
    hasOpenParenthesis: boolean;
    funcModifiers: TFunctionModifiers;
    methodModifiers: TMethodModifiers;
    nameIdent: TIdentifier
);

implementation

uses
    sysutils, classes, Parameters, FunctionDecl;

function GetCallingConvStr(const mods: TFunctionModifiers): string;
begin
    if mods.cdecl then exit('cdecl');
    if mods.cppdecl then exit('cppdecl');
    if mods.pascal then exit('pascal');
    if mods.register then exit('register');
    if mods.safecall then exit('safecall');
    if mods.stdcall then exit('stdcall');
    if mods.vectorcall then exit('vectorcall');
    if mods.winapi then exit('winapi');
    exit('');
end;

function MatchCandidateEquivalence(
    declRoutine: TRoutineTypeDef;
    declKind: TSymbolKind;
    symbolKind: TSymbolKind;
    routineTypeDef: TRoutineTypeDef;
    hasOpenParenthesis: boolean;
    funcModifiers: TFunctionModifiers;
    methodModifiers: TMethodModifiers;
    out errorMsg: string
): boolean;
var
    declParams, implParams: TParameterList;
    declCount, implCount, i: integer;
    declRet, implRet: TTypeDef;
    declMods: TFunctionModifiers;
    implCallConv, declCallConv: string;
begin
    errorMsg := '';

    if symbolKind <> declKind then
    begin
        errorMsg := 'Header of subroutine differs from previous declaration!';
        exit(false);
    end;

    if declRoutine <> nil then
        declParams := TParameterList(declRoutine.parameters)
    else
        declParams := nil;

    implParams := TParameterList(routineTypeDef.parameters);

    if hasOpenParenthesis then
    begin
        if declParams <> nil then declCount := declParams.count else declCount := 0;
        if implParams <> nil then implCount := implParams.count else implCount := 0;

        if declCount <> implCount then
        begin
            errorMsg := 'Header of subroutine differs from previous declaration!';
            exit(false);
        end;

        for i := 0 to declCount - 1 do
        begin
            if declParams.items[i].kind <> implParams.items[i].kind then
            begin
                errorMsg := 'Header of subroutine differs from previous declaration!';
                exit(false);
            end;

            if not SameText(declParams.items[i].name, implParams.items[i].name) then
            begin
                errorMsg := 'Header of subroutine differs from previous declaration!';
                exit(false);
            end;

            if not AreTypesEquivalent(declParams.items[i].typeDef, implParams.items[i].typeDef) then
            begin
                errorMsg := 'Header of subroutine differs from previous declaration!';
                exit(false);
            end;
        end;
    end;

    if symbolKind = skFunction then
    begin
        if declRoutine <> nil then declRet := declRoutine.returnType else declRet := nil;
        implRet := routineTypeDef.returnType;

        if not AreTypesEquivalent(declRet, implRet) then
        begin
            errorMsg := 'Header of subroutine differs from previous declaration!';
            exit(false);
        end;
    end;

    if (declRoutine <> nil) and (declRoutine.rangeToken is TFunctionDecl) then
    begin
        declMods := TFunctionDecl(declRoutine.rangeToken).funcModifiers;

        implCallConv := GetCallingConvStr(funcModifiers);
        declCallConv := GetCallingConvStr(declMods);

        if (implCallConv <> '') and (declCallConv <> '') and (implCallConv <> declCallConv) then
        begin
            errorMsg := 'Header of subroutine differs from previous declaration!';
            exit(false);
        end;
    end;

    Result := true;
end;

procedure VerifyImplementationEquivalence(
    declSymbol: TSymbol;
    symbolKind: TSymbolKind;
    routineTypeDef: TRoutineTypeDef;
    hasOpenParenthesis: boolean;
    funcModifiers: TFunctionModifiers;
    methodModifiers: TMethodModifiers;
    nameIdent: TIdentifier
);
var
    declRoutine: TRoutineTypeDef;
    overloads: TFPList;
    i: integer;
    errorMsg, candError: string;
    matched: boolean;
begin
    if (declSymbol = nil) or (nameIdent = nil) or (nameIdent.state = tsError) then exit;

    if not (declSymbol.kind in [skFunction, skProcedure, skConstructor, skDestructor]) or
       (declSymbol.typeDef = nil) or not (declSymbol.typeDef is TRoutineTypeDef) then
    begin
        nameIdent.state := tsError;
        nameIdent.errorMessage := declSymbol.displayName + ' is not a subroutine!';
        exit;
    end;

    declRoutine := TRoutineTypeDef(declSymbol.typeDef);
    overloads := declRoutine.overloads;

    matched := false;
    if MatchCandidateEquivalence(declRoutine, declSymbol.kind, symbolKind, routineTypeDef, hasOpenParenthesis, funcModifiers, methodModifiers, errorMsg) then
        matched := true
    else if overloads <> nil then
    begin
        for i := 0 to overloads.Count - 1 do
        begin
            if (TTypeDef(overloads.Items[i]) is TRoutineTypeDef) and
               MatchCandidateEquivalence(TRoutineTypeDef(overloads.Items[i]), declSymbol.kind, symbolKind, routineTypeDef, hasOpenParenthesis, funcModifiers, methodModifiers, candError) then
            begin
                matched := true;
                break;
            end;
        end;
    end;

    if not matched then
    begin
        nameIdent.state := tsError;
        nameIdent.errorMessage := errorMsg;
    end;
end;

end.
