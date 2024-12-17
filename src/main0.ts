import { InternalInterpreterError } from './errors';
import { IdentifierStatus, InterpreterOptions, PrintCounter, EvaluationParameters, Value } from './basic';
import { State } from './state';
import { CustomType, FunctionType } from './types';
import { ExceptionValue, StringValue, PredefinedFunction, ExceptionConstructor } from './values';


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

export function addGeneralLib(state: State): State {
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

// Define and export the Module type
export type Module = {
    'native': ((state: State, options?: InterpreterOptions) => State) | undefined,
    'code': string | undefined,
    'requires': string[] | undefined
};