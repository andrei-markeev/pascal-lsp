unit CompilationMode;

{$mode objfpc}

interface

type
    TCompilationMode = (cmStandardPascal, cmExtendedPascal, cmTurboPascal, cmMacPascal, cmFreePascal, cmObjectFreePascal, cmDelphi, cmUniversalPascal);

    TModeFeature = (
        mfHexNumbers,             // $123 hex numbers ($)
        mfOctalNumbers,           // &123 octal numbers (&) - FPC and ObjFPC only
        mfArrayLiterals,          // [...] array literals
        mfFunctionResultVariable, // Implicit 'Result' variable in function block
        mfParenthesizedConstExpr, // Parenthesized const expressions / structured constants
        mfStaticMethods,          // static method modifier
        mfClassMethods,           // class method modifier
        mfDefaultParamValues,     // parameter default values
        mfUntypedParams,          // untyped parameters (var / const without type)
        mfNamespacedUnits,        // Unit.Subunit syntax
        mfDefaultVarValues,       // var x: integer = 5
        mfExitProcName,           // Allow procedure name as argument in Exit() statement (MacPascal)
        mfAnsiStringDefault,      // 'string' keyword maps to ansiString (Delphi mode only)
        mfArrayConstructors,      // [...] array constructors / dynamic array constructors
        mfBasicTypecasting,       // Basic ordinal/pointer typecasting rules
        mfExtendedTypecasting,    // Extended pointer <-> ordinal typecasting rules
        mfCallAsVarRef,           // Call syntax as variable reference (e.g. Func()[1])
        mfUSCDPascalKeywords,     // USCD Pascal reserved words: string, unit, uses, interface, implementation
        mfTurboPascalKeywords,    // Turbo Pascal reserved words: absolute, asm, constructor, destructor, inherited, 
                                  //   object, operator, shl, shr, xor
        mfObjectPascalKeywords,   // Object Pascal reserved words: as, class, dispinterface, except, exports,
                                  //   finalization, finally, initialization, is, library, on, out, property, raise,
                                  //   resourcestring, threadvar, try
        mfExtendedPascalKeywords, // Extended Pascal reserved words: otherwise
        mfStringCaseLabels,       // String labels in case statements
        mfCaseRanges,             // Range labels 1..5 in case branches
        mfCaseElseClause,         // 'else' clause in case statement (TP, FPC, ObjFPC, Delphi)
        mfCaseOtherwiseClause,    // 'otherwise' clause in case statement (ExtPascal, MacPascal, FPC, ObjFPC, Delphi)
        mfAtOperator,             // @ operator for addresses
        mfBitwiseOperators,       // shl, shr, xor etc. (TP, FPC, ObjFPC, Delphi)
        mfExponentiationOperator, // ** exponentiation
        mfSymmetricDifference,    // >< symmetric difference
        mfShlShrOperators,        // << and >> operators (FPC, ObjFPC, Delphi)
        mfClassModifiers,         // sealed / abstract class modifiers
        mfProtectedVisibility,    // protected visibility specifier
        mfUntypedFiles,           // untyped 'file' type specification
        mfOptionalTypes,          // 'optional' types
        mfPointerTo,              // 'pointer to' instead of '^' in type definitions
        mfImplicitDereference,    // explicit dereference with '^' is banned
        mfPartialRecords,         // 'partial' records (hiding private fields in implementation)
        mfOberonMethodSyntax      // Oberon/Go-like method syntax e.g. `procedure (var self: TMyClass) Add(item: integer);`
    );

    TModeFeatures = set of TModeFeature;

const
    Features: array[TCompilationMode] of TModeFeatures = (
        // cmStandardPascal
        [],
        
        // cmExtendedPascal
        [mfBasicTypecasting, mfCallAsVarRef, mfCaseRanges, mfCaseOtherwiseClause, mfExtendedPascalKeywords, mfUntypedFiles],

        // cmTurboPascal
        [mfHexNumbers, mfArrayLiterals, mfParenthesizedConstExpr, mfUntypedParams, mfBasicTypecasting, mfUSCDPascalKeywords,
         mfTurboPascalKeywords, mfCaseRanges, mfCaseElseClause, mfAtOperator, mfBitwiseOperators, mfUntypedFiles],

        // cmMacPascal
        [mfHexNumbers, mfArrayLiterals, mfParenthesizedConstExpr, mfUntypedParams, mfBasicTypecasting, mfUSCDPascalKeywords,
         mfTurboPascalKeywords, mfCaseRanges, mfCaseOtherwiseClause, mfAtOperator, mfUntypedFiles, mfExitProcName],

        // cmFreePascal
        [mfHexNumbers, mfOctalNumbers, mfArrayLiterals, mfParenthesizedConstExpr, mfDefaultParamValues, mfUntypedParams,
         mfNamespacedUnits, mfDefaultVarValues, mfBasicTypecasting, mfExtendedTypecasting,
         mfCallAsVarRef, mfUSCDPascalKeywords, mfTurboPascalKeywords, mfExtendedPascalKeywords,
         mfStringCaseLabels, mfCaseRanges, mfCaseElseClause, mfCaseOtherwiseClause, mfAtOperator, mfBitwiseOperators,
         mfExponentiationOperator, mfSymmetricDifference, mfShlShrOperators, mfUntypedFiles],

        // cmObjectFreePascal
        [mfHexNumbers, mfOctalNumbers, mfArrayLiterals, mfFunctionResultVariable, mfParenthesizedConstExpr, mfStaticMethods,
         mfClassMethods, mfDefaultParamValues, mfUntypedParams, mfNamespacedUnits, mfDefaultVarValues,
         mfArrayConstructors, mfExtendedTypecasting, mfBasicTypecasting, mfCallAsVarRef,
         mfUSCDPascalKeywords, mfTurboPascalKeywords, mfObjectPascalKeywords, mfExtendedPascalKeywords, mfStringCaseLabels,
         mfCaseRanges, mfCaseElseClause, mfCaseOtherwiseClause, mfAtOperator, mfBitwiseOperators, mfExponentiationOperator,
         mfSymmetricDifference, mfShlShrOperators, mfClassModifiers, mfProtectedVisibility, mfUntypedFiles],

        // cmDelphi
        [mfHexNumbers, mfArrayLiterals, mfFunctionResultVariable, mfParenthesizedConstExpr, mfStaticMethods,
         mfClassMethods, mfDefaultParamValues, mfUntypedParams, mfNamespacedUnits, mfDefaultVarValues,
         mfAnsiStringDefault, mfArrayConstructors, mfExtendedTypecasting, mfBasicTypecasting, mfCallAsVarRef,
         mfUSCDPascalKeywords, mfTurboPascalKeywords, mfObjectPascalKeywords, mfExtendedPascalKeywords, mfStringCaseLabels,
         mfCaseRanges, mfCaseElseClause, mfCaseOtherwiseClause, mfAtOperator, mfBitwiseOperators, mfExponentiationOperator,
         mfSymmetricDifference, mfShlShrOperators, mfClassModifiers, mfProtectedVisibility, mfUntypedFiles],

        // cmUniversalPascal
        [mfBasicTypecasting, mfUSCDPascalKeywords, mfCaseElseClause, mfCallAsVarRef, mfOptionalTypes, mfPointerTo,
         mfImplicitDereference, mfPartialRecords, mfOberonMethodSyntax]
    );

implementation

end.