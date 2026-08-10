unit CompilationMode;

{$mode objfpc}

interface

type
    TCompilationMode = (cmStandardPascal, cmExtendedPascal, cmTurboPascal, cmMacPascal, cmFreePascal, cmObjectFreePascal, cmDelphi);

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
        mfProcInExprDisallowed,   // Procedure calls in expressions throw error (all except MacPascal)
        mfAnsiStringDefault,      // 'string' keyword maps to ansiString (Delphi mode only)
        mfArrayConstructors,      // [...] array constructors / dynamic array constructors
        mfExtendedTypecasting,    // Extended pointer <-> ordinal typecasting rules
        mfBasicTypecasting,       // Basic ordinal/pointer typecasting rules
        mfCallAsVarRef,           // Call syntax as variable reference (e.g. Func()[1])
        mfTypecastingSyntax,      // TypeName(expr) syntax
        mfTurboPascalKeywords,    // Turbo Pascal reserved words
        mfObjectPascalKeywords,   // Object Pascal reserved words
        mfExtendedPascalKeywords, // Extended Pascal reserved words
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
        mfUntypedFiles            // untyped 'file' type specification
    );

    TModeFeatures = set of TModeFeature;

const
    Features: array[TCompilationMode] of TModeFeatures = (
        // cmStandardPascal
        [mfBasicTypecasting],
        
        // cmExtendedPascal
        [mfBasicTypecasting, mfCallAsVarRef, mfCaseRanges, mfCaseOtherwiseClause, mfExtendedPascalKeywords, mfUntypedFiles],

        // cmTurboPascal
        [mfHexNumbers, mfArrayLiterals, mfParenthesizedConstExpr, mfUntypedParams, mfBasicTypecasting, mfTypecastingSyntax,
         mfTurboPascalKeywords, mfCaseRanges, mfCaseElseClause, mfAtOperator, mfBitwiseOperators, mfUntypedFiles, mfProcInExprDisallowed],

        // cmMacPascal
        [mfHexNumbers, mfArrayLiterals, mfParenthesizedConstExpr, mfUntypedParams, mfBasicTypecasting, mfTypecastingSyntax,
         mfTurboPascalKeywords, mfCaseRanges, mfCaseOtherwiseClause, mfAtOperator, mfUntypedFiles],

        // cmFreePascal
        [mfHexNumbers, mfOctalNumbers, mfArrayLiterals, mfParenthesizedConstExpr, mfDefaultParamValues, mfUntypedParams,
         mfNamespacedUnits, mfDefaultVarValues, mfProcInExprDisallowed, mfExtendedTypecasting,
         mfBasicTypecasting, mfCallAsVarRef, mfTypecastingSyntax, mfTurboPascalKeywords, mfExtendedPascalKeywords,
         mfStringCaseLabels, mfCaseRanges, mfCaseElseClause, mfCaseOtherwiseClause, mfAtOperator, mfBitwiseOperators,
         mfExponentiationOperator, mfSymmetricDifference, mfShlShrOperators, mfUntypedFiles],

        // cmObjectFreePascal
        [mfHexNumbers, mfOctalNumbers, mfArrayLiterals, mfFunctionResultVariable, mfParenthesizedConstExpr, mfStaticMethods,
         mfClassMethods, mfDefaultParamValues, mfUntypedParams, mfNamespacedUnits, mfDefaultVarValues, mfProcInExprDisallowed,
         mfArrayConstructors, mfExtendedTypecasting, mfBasicTypecasting, mfCallAsVarRef,
         mfTypecastingSyntax, mfTurboPascalKeywords, mfObjectPascalKeywords, mfExtendedPascalKeywords, mfStringCaseLabels,
         mfCaseRanges, mfCaseElseClause, mfCaseOtherwiseClause, mfAtOperator, mfBitwiseOperators, mfExponentiationOperator,
         mfSymmetricDifference, mfShlShrOperators, mfClassModifiers, mfProtectedVisibility, mfUntypedFiles],

        // cmDelphi
        [mfHexNumbers, mfArrayLiterals, mfFunctionResultVariable, mfParenthesizedConstExpr, mfStaticMethods,
         mfClassMethods, mfDefaultParamValues, mfUntypedParams, mfNamespacedUnits, mfDefaultVarValues, mfProcInExprDisallowed,
         mfAnsiStringDefault, mfArrayConstructors, mfExtendedTypecasting, mfBasicTypecasting, mfCallAsVarRef,
         mfTypecastingSyntax, mfTurboPascalKeywords, mfObjectPascalKeywords, mfExtendedPascalKeywords, mfStringCaseLabels,
         mfCaseRanges, mfCaseElseClause, mfCaseOtherwiseClause, mfAtOperator, mfBitwiseOperators, mfExponentiationOperator,
         mfSymmetricDifference, mfShlShrOperators, mfClassModifiers, mfProtectedVisibility, mfUntypedFiles]
    );

implementation

end.