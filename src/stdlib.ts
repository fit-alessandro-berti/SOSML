import { IState as State } from './basic';
import { IdentifierStatus, EvaluationParameters, Value, PrintCounter } from './basic';
import { InternalInterpreterError } from './errors';
import { CustomType, FunctionType } from './types';
import { PredefinedFunction, StringValue, ExceptionValue,
         ExceptionConstructor } from './values';
import { InterpreterOptions } from './basic';
import * as Interpreter from './main';

import { ARRAY_LIB } from './stdlib/array';
import { ASSERT_LIB } from './stdlib/assert';
import { CHAR_LIB } from './stdlib/char';
import { EVAL_LIB } from './stdlib/eval';
import { INT_LIB } from './stdlib/int';
import { LIST_LIB } from './stdlib/list';
import { LISTSORT_LIB } from './stdlib/listsort';
import { MATH_LIB } from './stdlib/math';
/* import { OS_LIB } from './stdlib/os'; */
import { RANDOM_LIB } from './stdlib/random';
import { REAL_LIB } from './stdlib/real';
import { STRING_LIB } from './stdlib/string';
import { VECTOR_LIB } from './stdlib/vector';

export let intType = new CustomType('int');
export let realType = new CustomType('real');
export let wordType = new CustomType('word');
export let boolType = new CustomType('bool');
export let stringType = new CustomType('string');
export let charType = new CustomType('char');
export let exnType = new CustomType('exn');

export let overflowException = new ExceptionConstructor('Overflow', 0, 0, 3);
export let domainException = new ExceptionConstructor('Domain', 0, 0, 4);
export let sizeException = new ExceptionConstructor('Size', 0, 0, 5);
export let chrException = new ExceptionConstructor('Chr', 0, 0, 6);
export let subscriptException = new ExceptionConstructor('Subscript', 0, 0, 7);
export let failException = new ExceptionConstructor('Fail', 1, 0, 8);

export type Module = {
    'native': ((state: State, options?: InterpreterOptions) => State) | undefined, /* callback for native parts */
    'code': string | undefined,
    'requires': string[] | undefined /* names of modules required for this module (excluding __Base) */
};

function addGeneralLib(state: State): State {
    state.setStaticValue('exnName', new FunctionType(exnType, stringType),
                         IdentifierStatus.VALUE_VARIABLE);
    state.setDynamicValue('exnName',
                          new PredefinedFunction('exnName',
                                                 (val: Value, params: EvaluationParameters) => {
        if (val instanceof ExceptionValue) {
            return [new StringValue((val as ExceptionValue).constructorName), false, []];
        }
        throw new InternalInterpreterError('Expected an exception.');
    }), IdentifierStatus.VALUE_VARIABLE);


    state.setStaticValue('exnMessage', new FunctionType(exnType, stringType),
                         IdentifierStatus.VALUE_VARIABLE);
    state.setDynamicValue('exnMessage',
                          new PredefinedFunction('exnMessage',
                                                 (val: Value, params: EvaluationParameters) => {
        if (val instanceof ExceptionValue) {
            return [new StringValue((val as ExceptionValue).pcToString(params.modifiable,
                    new PrintCounter(50))), false, []];
        }
        throw new InternalInterpreterError('Expected an exception.');
    }), IdentifierStatus.VALUE_VARIABLE);

    return state;
}

export let STDLIB: {
    [name: string]: Module
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

            (* fun ! (a : \'A ref): \'A = ! a;
            fun op := ((a, b) : (\'A ref * \'A)): unit = a := b;
            fun ref (a : \'A): \'A ref = ref a; *)

            fun ignore a = ();
            infix 0 before;
            fun a before (b: unit) = a;`,
        'requires': undefined },
    'Array': ARRAY_LIB,
    'Assert' : ASSERT_LIB,
    'Char': CHAR_LIB,
    'Eval': EVAL_LIB,
    'Int': INT_LIB,
    'List': LIST_LIB, /* complete */
    'Listsort': LISTSORT_LIB, /* complete */
    'Math': MATH_LIB, /* Complete */
    /* 'OS': OS_LIB, */
    'Option': { /* Complete */
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
        'requires': undefined },
    'Random': RANDOM_LIB,
    'Real': REAL_LIB,
    'String': STRING_LIB, /* Complete % useless stuff */
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
        state = Interpreter.interpret(mod.code, state, options).state;
    }
    state.registerModule(name);
    return state;
}

