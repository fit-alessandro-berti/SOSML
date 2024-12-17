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

export function getAvailableModules(): string[] {
	return [];
}

export function getFirstState(loadModules: string[] = getAvailableModules(),
                              options: InterpreterOptions = {}): State {
    /*if (options.noModules === true) {
        return getInitialState(options);
    }
    let res = loadModule(getInitialState(options), '__Base', options);
    for (let i of loadModules) {
        res = loadModule(res, i, options);
    }*/
	let res = getInitialState(options);
    return res;
}