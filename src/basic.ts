// fine
import { Warning } from './errors';
import { Token, LongIdentifierToken } from './tokens';

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

export interface IState {
    // Public fields
    id: number;
    parent: any;
    staticBasis: any;
    dynamicBasis: any;
    memory: any;
    exceptionEvalId: number;
    freeTypeVariables: any;
    valueIdentifierId: { [name: string]: number };
    warns: Warning[];
    insideLocalDeclBody: boolean;
    localDeclStart: boolean;
    loadedModules: string[];
	infixEnvironment: any;

    // Public methods
    printBinding(
        name: string,
        value: any,
        type: any,
        options?: PrintOptions,
        acon?: boolean
    ): string;

    printBasis(
        dynamicBasis: any,
        staticBasis: any,
        options?: PrintOptions,
        indent?: number
    ): string;

    toString(options?: PrintOptions): string;

    getNestedState(newId?: number): IState;

    hasModule(name: string): boolean;
    registerModule(name: string): void;

    getIdChanges(stopId: number): { [name: string]: number };
    getMemoryChanges(stopId: number): any;
    getDynamicChanges(stopId: number): any;
    getDynamicLocalDeclChanges(stopId: number): any;
    getStaticChanges(stopId: number): any;

    getCell(address: number): any;
    getTypeVariableBinds(idLimit?: number): any;

    getStaticValue(name: string, idLimit?: number): any;
    getStaticType(name: string, idLimit?: number): any;
    getStaticStructure(name: string, idLimit?: number): any;
    getAndResolveStaticStructure(name: LongIdentifierToken, idLimit?: number): any;
    getStaticSignature(name: string, idLimit?: number): any;
    getStaticFunctor(name: string, idLimit?: number): any;

    getDynamicValue(name: string, idLimit?: number): any;
    getDynamicType(name: string, idLimit?: number): any;
    getDynamicStructure(name: string, idLimit?: number): any;
    getAndResolveDynamicStructure(name: LongIdentifierToken, idLimit?: number): any;
    getDynamicSignature(name: string, idLimit?: number): any;
    getDynamicFunctor(name: string, idLimit?: number): any;

    getInfixStatus(id: Token, idLimit?: number): any;

    getValueIdentifierId(name: string, idLimit?: number): number;
    getWarnings(): Warning[];

    incrementValueIdentifierId(name: string, atId?: number): void;
    setCell(address: number, value: any): void;
    setNewCell(value: any): any;
    getNextExceptionEvalId(): number;

    deleteStaticValue(name: string): void;
    setStaticValue(name: string, type: any, is: any, atId?: number): void;
    setStaticType(name: string, type: any, constructors: string[], arity: number, allowsEquality: boolean, atId?: number): void;
    setStaticStructure(name: string, structure: any, atId?: number): void;
    setStaticSignature(name: string, signature: any, atId?: number): void;
    setStaticFunctor(name: string, functor: any, openParameter?: boolean, atId?: number): void;

    setDynamicValue(name: string, value: any, is: any, atId?: number): void;
    setDynamicType(name: string, constructors: string[], atId?: number): void;
    setDynamicStructure(name: string, structure: any, atId?: number): void;
    setDynamicSignature(name: string, signature: any, atId?: number): void;
    setDynamicFunctor(name: string, functor: any, atId?: number): void;

    setInfixStatus(id: Token, precedence: number, rightAssociative: boolean, infix: boolean, atId?: number): void;

    setValueIdentifierId(name: string, setTo: number, atId?: number): void;
    addWarning(warn: Warning, atId?: number): void;
    setWarnings(warns: Warning[], atId?: number): void;
}
