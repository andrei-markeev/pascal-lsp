unit Modifiers;

interface

type
    TMethodModifiers = record
        abstract: boolean;
        dynamic: boolean;
        reintroduce: boolean;
        message: boolean; // TODO: link constant expression
        override: boolean;
        virtual: boolean;
    end;
    TFunctionModifiers = record
        alias: string;
        cdecl: boolean;
        cppdecl: boolean;
        export: boolean;
        external: record
            libraryName: string;
            name: string;
            index: string;
        end;
        far: boolean;
        forward: boolean;
        hardfloat: boolean;
        inline: boolean;
        interrupt: boolean;
        iocheck: boolean;
        local: boolean;
        MS_ABI_Default: boolean;
        MS_ABI_CDecl: boolean;
        MWPascal: boolean;
        near: boolean;
        noreturn: boolean;
        nostackframe: boolean;
        overload: boolean;
        pascal: boolean;
        public: string;
        register: boolean;
        safecall: boolean;
        saveregisters: boolean;
        softload: boolean;
        stdcall: boolean;
        SYSV_ABI_Default: boolean;
        SYSV_ABI_CDecl: boolean;
        varargs: boolean;
        vectorcall: boolean;
        winapi: boolean;
    end;

implementation

end.