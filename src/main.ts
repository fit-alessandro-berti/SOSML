// main.ts (merged with stdlib.ts)

// --------------------------------------
// Original imports from main.ts
// --------------------------------------
import * as Errors from './errors';
import { InternalInterpreterError, Warning } from './errors';
import * as Tokens from './tokens';
import { InterpreterOptions } from './basic';
import { IState as State } from './basic';
import { Value } from './basic';
import * as Lexer from './lexer';
import { Type } from './types';
import * as Types from './types';
import * as Values from './values';
import { getInitialState } from './initialState';
import * as Parser from './parser';
import * as Evaluator from './evaluator';
import * as Declarations from './declarations';
import * as Expressions from './expressions';

import { intType, realType, wordType, boolType, stringType, charType, exnType, overflowException, domainException, sizeException, chrException, subscriptException, failException, addGeneralLib, Module } from './main0';

export { intType, realType, wordType, boolType, stringType, charType, exnType, overflowException, domainException, sizeException, chrException, subscriptException, failException, addGeneralLib, Module };

// Import stdlib submodules
import { ARRAY_LIB } from './stdlib/array';
import { ASSERT_LIB } from './stdlib/assert';
import { CHAR_LIB } from './stdlib/char';
import { EVAL_LIB } from './stdlib/eval';
import { INT_LIB } from './stdlib/int';
import { LIST_LIB } from './stdlib/list';
import { LISTSORT_LIB } from './stdlib/listsort';
import { MATH_LIB } from './stdlib/math';
import { RANDOM_LIB } from './stdlib/random';
import { REAL_LIB } from './stdlib/real';
import { STRING_LIB } from './stdlib/string';
import { VECTOR_LIB } from './stdlib/vector';

// --------------------------------------
// Re-exports from original main.ts
// --------------------------------------
export {
    Lexer,
    Parser,
    Evaluator,
    Declarations,
    Errors,
    Expressions,
    Tokens,
    Types,
    Values
};
export * from './state';

export type InterpretationResult = {
    state: State; // Computed state
    evaluationErrored: boolean; // Evaluation returned an SML error
    error?: Value; // Thrown SML error; present only if evaluationErrored is true
    warnings: Warning[]; // Array of emitted warnings / messages
};

// --------------------------------------
// interpret function from main.ts
// --------------------------------------
export function interpret(nextInstruction: string,
                          oldState: State = getInitialState(),
                          options: InterpreterOptions = {
                              'allowSuccessorML': false,
                              'disableElaboration': false,
                              'allowVector': true,
                              'strictMode': true,
                              'realEquality': false
                          }): InterpretationResult {
    let state = oldState.getNestedState();
    let tkn = Lexer.lex(nextInstruction, options);
    let ast = Parser.parse(tkn, state, options);
    ast = ast.simplify();
    state = oldState.getNestedState();

    if (options.disableElaboration === true) {
        let tmp = Evaluator.evaluate(oldState.getNestedState(), ast /* , options */);
        if (tmp === undefined || tmp.newState === undefined) {
            throw new InternalInterpreterError('How is this undefined?');
        }
        return {
            'state':                tmp.newState,
            'evaluationErrored':    tmp.hasThrown,
            'error':                tmp.value,
            'warnings':             (<State> tmp.newState).getWarnings()
        };
    }

    let elab = ast.elaborate(state, new Map<string, [Type, boolean]>(), '\'*t0',
                             new Map<string, Type>(), true, options);
    state = elab[0];

    if (options.disableEvaluation === true) {
        return {
            'state':                state,
            'evaluationErrored':    false,
            'error':                undefined,
            'warnings':             elab[1]
        };
    }
    // Use a fresh state to be able to piece types and values together
    let res = Evaluator.evaluate(oldState.getNestedState(), ast /* , options */);
    if (res === undefined || res.newState === undefined) {
        throw new InternalInterpreterError('How is this undefined?');
    }

    for (let i = 0; i < elab[1].length; ++i) {
        (<State> res.newState).addWarning(elab[1][i]);
    }

    if (res.hasThrown) {
        return {
            'state':                res.newState,
            'evaluationErrored':    true,
            'error':                res.value,
            'warnings':             (<State> res.newState).getWarnings()
        };
    }

    let curState = <State> res.newState;

    while (curState.id > oldState.id) {
        if (curState.dynamicBasis !== undefined) {
            curState.freeTypeVariables = state.getTypeVariableBinds(curState.id);

            // For every new bound value, try to find its type
            for (let i in curState.dynamicBasis.valueEnvironment) {
                if (Object.prototype.hasOwnProperty.call(
                    curState.dynamicBasis.valueEnvironment, i)) {

                    let tp = state.getStaticValue(i, curState.id);
                    if (tp !== undefined) {
                        curState.setStaticValue(i, tp[0], tp[1]);
                    }
                }
            }

            // For every new bound type, try to find its type
            for (let i in curState.dynamicBasis.typeEnvironment) {
                if (Object.prototype.hasOwnProperty.call(
                    curState.dynamicBasis.typeEnvironment, i)) {

                    let tp = state.getStaticType(i, curState.id);
                    if (tp !== undefined) {
                        curState.setStaticType(i, tp.type, tp.constructors, tp.arity, tp.allowsEquality);
                    }
                }
            }

            // For every new bound structure, try to find its type
            for (let i in curState.dynamicBasis.structureEnvironment) {
                if (Object.prototype.hasOwnProperty.call(
                    curState.dynamicBasis.structureEnvironment, i)) {

                    let tp = state.getStaticStructure(i, curState.id);
                    if (tp !== undefined) {
                        curState.setStaticStructure(i, tp);
                    }
                }
            }

            // For every new bound signature, try to find its type
            for (let i in curState.dynamicBasis.signatureEnvironment) {
                if (Object.prototype.hasOwnProperty.call(
                    curState.dynamicBasis.signatureEnvironment, i)) {

                    let tp = state.getStaticSignature(i, curState.id);
                    if (tp !== undefined) {
                        curState.setStaticSignature(i, tp);
                    }
                }
            }

            // For every new bound functor, try to find its type
            for (let i in curState.dynamicBasis.functorEnvironment) {
                if (Object.prototype.hasOwnProperty.call(
                    curState.dynamicBasis.functorEnvironment, i)) {

                    let tp = state.getStaticFunctor(i, curState.id);
                    if (tp !== undefined) {
                        curState.setStaticFunctor(i, [tp[0], tp[1], tp[2]], tp[3]);
                    }
                }
            }
        }
        if (state.parent === undefined) {
            break;
        }
        curState = <State> curState.parent;
        while (state.id > curState.id && state.parent !== undefined) {
            state = <State> state.parent;
        }
    }

    return {
        'state':                res.newState,
        'evaluationErrored':    false,
        'error':                res.value,
        'warnings':             (<State> res.newState).getWarnings()
    };
}


// --------------------------------------
// Code from stdlib.ts integrated below
// --------------------------------------

export let STDLIB: {
    [name: string]: {
        'native': ((state: State, options?: InterpreterOptions) => State) | undefined,
        'code': string | undefined,
        'requires': string[] | undefined
    }
} = {
    '__Base': {
        'native': addGeneralLib,
        'code': `fun o (f,g) x = f (g x);
            infix 3 o;
            datatype order = LESS | EQUAL | GREATER;

            exception Domain;
            exception Size;
            exception Chr;
            exception Subscript;
            exception Fail of string;

            fun not true = false | not false = true;

            fun ignore a = ();
            infix 0 before;
            fun a before (b: unit) = a;`,
        'requires': undefined
    },
    'Array': ARRAY_LIB,
    'Assert' : ASSERT_LIB,
    'Char': CHAR_LIB,
    'Eval': EVAL_LIB,
    'Int': INT_LIB,
    'List': LIST_LIB,
    'Listsort': LISTSORT_LIB,
    'Math': MATH_LIB,
    'Option': {
        'native': undefined,
        'code': `structure Option = struct
                exception Option;

                datatype 'a option = NONE | SOME of 'a;

                fun getOpt (NONE, a) = a
                  | getOpt (SOME x, a) = x;

                fun isSome NONE = false
                  | isSome (SOME _) = true;

                fun valOf (SOME x) = x
                  | valOf NONE = raise Option;
            end;
            open Option;

            structure Option = struct
                open Option;

                fun app f (SOME v) = f v
                  | app f NONE = ();

                fun map f NONE = NONE
                  | map f (SOME v) = SOME(f v);

                fun mapPartial f NONE = NONE
                  | mapPartial f (SOME v) = f v;

                fun filter f x = if f x then SOME x else NONE;

                fun join NONE = NONE
                  | join (SOME (SOME x)) = SOME x;

                fun compose (f, g) a = case g a of
                      NONE => NONE
                    | SOME v => SOME (f v);

                fun composePartial (f, g) a = case g a of
                      NONE => NONE
                    | SOME v => (f v);
            end;
            `,
        'requires': undefined
    },
    'Random': RANDOM_LIB,
    'Real': REAL_LIB,
    'String': STRING_LIB,
    'Vector': VECTOR_LIB
};

export function loadModule(state: State, name: string, options: InterpreterOptions): State {
    if (!STDLIB.hasOwnProperty(name)) {
        throw new InternalInterpreterError('The module "' + name + '" does not exist. Auuuu~');
    }
    if (state.hasModule(name)) {
        return state;
    }

    let mod = STDLIB[name];
    if (mod.requires !== undefined ) {
        for (let i of mod.requires) {
            if (!state.hasModule(i)) {
                state = loadModule(state, i, options);
            }
        }
    }
    if (mod.native !== undefined) {
        state = mod.native(state, options);
    }
    if (mod.code !== undefined) {
        // Previously: state = Interpreter.interpret(mod.code, state, options).state;
        // Now we directly call interpret since it's defined above
        state = interpret(mod.code, state, options).state;
    }
    state.registerModule(name);
    return state;
}


// --------------------------------------
// getAvailableModules and getFirstState from main.ts
// --------------------------------------

export function getAvailableModules(): string[] {
    let res: string[] = [];
    for (let i in STDLIB) {
        if (Object.prototype.hasOwnProperty.call(STDLIB, i)) {
            if ((<string> i)[0] !== '_') {
                res.push(<string> i);
            }
        }
    }
    return res;
}

export function getFirstState(loadModules: string[] = getAvailableModules(),
                              options: InterpreterOptions = {}): State {
    if (options.noModules === true) {
        return getInitialState(options);
    }
    let res = loadModule(getInitialState(options), '__Base', options);
    for (let i of loadModules) {
        res = loadModule(res, i, options);
    }
    return res;
}
