// fine

export let MAXINT = 1073741823;
export let MININT = -1073741824;

export type PrintOptions = {
    stopId?: number; // id of the oldest state to print
    fullSymbol?: string; // Symbol used for the first line of output per declaration (default: "")
    emptySymbol?: string; // Symbol used for every other line of output per declaration (def: "")
    indent?: number; // indent amount (spaces, default: 2)
    boldText?: (text: string) => string; // Function used to make text bold
    italicText?: (text: string) => string; // Function used to make text italic
    escapeText?: (text: string) => string; // Function on user-defined names
    showTypeVariablesAsUnicode?: boolean; // Display type variables as unicode
};

export type InterpreterOptions = {
    // General
    strictMode?: boolean; // Enforce more strict adherence to the SML 97 Standard
    realEquality?: boolean; // Turn real into a type with equality
    allowSuccessorML?: boolean; // Enable experimental features
    noModules?: boolean; // Don't load any modules

    // Lexer
    allowUnicode?: boolean; // enable unicode support
    allowUnicodeTypeVariables?: boolean; // allow unicode replacements for type variables
    allowCommentToken?: boolean; // don't skip output tokens for comments

    // Parser
    allowVector?: boolean; // Allow vector patterns
    allowStructuresAnywhere?: boolean;
    allowSignaturesAnywhere?: boolean;
    allowFunctorsAnywhere?: boolean;

    // Elaboration
    disableElaboration?: boolean;

    // Evaluation
    disableEvaluation?: boolean;
};