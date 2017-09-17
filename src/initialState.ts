import { State, StaticBasis, DynamicBasis, InfixStatus, TypeInformation,
         IdentifierStatus } from './state';
import { FunctionType, CustomType, TupleType, Type, TypeVariable, TypeVariableBind } from './types';
import { CharValue, Real, Integer, StringValue, PredefinedFunction, Word, ConstructedValue,
         ValueConstructor, ExceptionConstructor, BoolValue, Value, RecordValue } from './values';
import { InternalInterpreterError, Warning } from './errors';

// Initial static basis (see SML Definition, appendix C through E)

// let intType = new CustomType('int');
let realType = new CustomType('real');
// let wordType = new CustomType('word');
let boolType = new CustomType('bool');
let stringType = new CustomType('string');
let charType = new CustomType('char');

function functionType(type: Type): Type {
    return new FunctionType(new TupleType([type, type]), type).simplify();
}
function bfunctionType(type: Type): Type {
    return new FunctionType(new TupleType([type, type]), boolType).simplify();
}

let typeVar = new TypeVariable('\'a');
let eqTypeVar = new TypeVariable('\'\'b');

let intWordType = new TypeVariable('\'iw');
let intRealType = new TypeVariable('\'ir');
let intWordRealType = new TypeVariable('\'iwr');
let anyType = new TypeVariable('\'any');

function intWordBind(type: Type): Type {
    return new TypeVariableBind('\'iw', type, [new CustomType('int'), new CustomType('word')]).propagate();
}
function intRealBind(type: Type): Type {
    return new TypeVariableBind('\'ir', type, [new CustomType('int'), new CustomType('real')]).propagate();
}
function intWordRealBind(type: Type): Type {
    return new TypeVariableBind('\'iwr', type, [new CustomType('int'), new CustomType('word'),
        new CustomType('real')]).propagate();
}
function anyBind(type: Type): Type {
    return new TypeVariableBind('\'any', type, [new CustomType('int'), new CustomType('word'),
        new CustomType('real'), new CustomType('string'), new CustomType('char')]).propagate();
}

let initialState: State = new State(
    0,
    undefined,
    new StaticBasis(
        {
            'unit':     new TypeInformation(
                new FunctionType(new CustomType('unit'), new TupleType([])).simplify(), [], 0, true),
            'bool':     new TypeInformation(new CustomType('bool'),  ['true', 'false'], 0, true),
            'int':      new TypeInformation(new CustomType('int'),   [], 0, true),
            'word':     new TypeInformation(new CustomType('word'),  [], 0, true),
            'real':     new TypeInformation(new CustomType('real'),  [], 0, false),
            'string':   new TypeInformation(new CustomType('string'), [], 0, false),
            'char':     new TypeInformation(new CustomType('char'),  [], 0, false),
            'list':     new TypeInformation(new CustomType('list', [typeVar]), ['nil', '::'], 1, true),
            'array':    new TypeInformation(new CustomType('array', [typeVar]), [], 1, true),
            'ref':      new TypeInformation(new CustomType('ref', [typeVar]), ['ref'], 1, true),
            'exn':      new TypeInformation(new CustomType('exn'), [], 0, false)
        },
        {
            'div':      [intWordBind(functionType(intWordType)), IdentifierStatus.VALUE_VARIABLE],
            'mod':      [intWordBind(functionType(intWordType)), IdentifierStatus.VALUE_VARIABLE],
            '*':        [intWordRealBind(functionType(intWordRealType)),
                IdentifierStatus.VALUE_VARIABLE],
            '/':        [functionType(realType), IdentifierStatus.VALUE_VARIABLE],
            '+':        [intWordRealBind(functionType(intWordRealType)),
                IdentifierStatus.VALUE_VARIABLE],
            '-':        [intWordRealBind(functionType(intWordRealType)),
                IdentifierStatus.VALUE_VARIABLE],
            '<':        [anyBind(bfunctionType(anyType)), IdentifierStatus.VALUE_VARIABLE],
            '<=':       [anyBind(bfunctionType(anyType)), IdentifierStatus.VALUE_VARIABLE],
            '>':        [anyBind(bfunctionType(anyType)), IdentifierStatus.VALUE_VARIABLE],
            '>=':       [anyBind(bfunctionType(anyType)), IdentifierStatus.VALUE_VARIABLE],
            '=':        [new TypeVariableBind('\'\'b',
                new FunctionType(new TupleType([eqTypeVar, eqTypeVar]), boolType)).simplify(),
                IdentifierStatus.VALUE_VARIABLE],
            '<>':       [new TypeVariableBind('\'\'b',
                new FunctionType(new TupleType([eqTypeVar, eqTypeVar]), boolType)).simplify(),
                IdentifierStatus.VALUE_VARIABLE],
            'true':     [new CustomType('bool'), IdentifierStatus.VALUE_CONSTRUCTOR],
            'false':    [new CustomType('bool'), IdentifierStatus.VALUE_CONSTRUCTOR],
            'nil':      [new TypeVariableBind('\'a',
                new CustomType('list', [typeVar])), IdentifierStatus.VALUE_CONSTRUCTOR],
            '::':       [new TypeVariableBind('\'a', new FunctionType(
                            new TupleType([typeVar, new CustomType('list', [typeVar])]),
                            new CustomType('list', [typeVar]))).simplify(),
                                IdentifierStatus.VALUE_CONSTRUCTOR],
            'Match':    [new CustomType('exn'), IdentifierStatus.EXCEPTION_CONSTRUCTOR],
            'Bind':     [new CustomType('exn'), IdentifierStatus.EXCEPTION_CONSTRUCTOR],
            'Div':      [new CustomType('exn'), IdentifierStatus.EXCEPTION_CONSTRUCTOR],
            'Overflow': [new CustomType('exn'), IdentifierStatus.EXCEPTION_CONSTRUCTOR],
            '^':        [functionType(stringType), IdentifierStatus.VALUE_VARIABLE],
            'explode':  [new FunctionType(stringType, new CustomType('list', [charType])).simplify(),
                IdentifierStatus.VALUE_VARIABLE],
            'implode':  [new FunctionType(new CustomType('list', [charType]), stringType).simplify(),
                IdentifierStatus.VALUE_VARIABLE],
            '~':        [intRealBind(new FunctionType(intRealType, intRealType)),
                IdentifierStatus.VALUE_VARIABLE],
            'abs':      [intRealBind(new FunctionType(intRealType, intRealType)),
                IdentifierStatus.VALUE_VARIABLE],
            'print':    [new TypeVariableBind('\'a', new FunctionType(typeVar, new TupleType([]))).simplify(),
                IdentifierStatus.VALUE_VARIABLE],
            'printLn':  [new TypeVariableBind('\'a', new FunctionType(typeVar, new TupleType([]))).simplify(),
                IdentifierStatus.VALUE_VARIABLE],
            ':=':       [new TypeVariableBind('\'a',
                new FunctionType(new TupleType([new CustomType('ref', [typeVar]), typeVar]),
                new TupleType([]))).simplify(), IdentifierStatus.VALUE_VARIABLE],
            'ref':      [new TypeVariableBind('\'a', new FunctionType(typeVar, new CustomType('ref', [typeVar]))),
                            IdentifierStatus.VALUE_CONSTRUCTOR],
            '!':        [new TypeVariableBind('\'a', new FunctionType(new CustomType('ref', [typeVar]), typeVar)),
                            IdentifierStatus.VALUE_VARIABLE]
        },
        { },
        { },
        { }
    ),
    new DynamicBasis(
        {
            'unit':     [],
            'bool':     ['true', 'false'],
            'int':      [],
            'word':     [],
            'real':     [],
            'string':   [],
            'char':     [],
            'list':     ['nil', '::'],
            'array':    [],
            'ref':      ['ref'],
            'exn':      [],
        },
        {
            'div':      [new PredefinedFunction('div', (val: Value) => {
                if (val instanceof RecordValue) {
                    let val1 = (<RecordValue> val).getValue('1');
                    let val2 = (<RecordValue> val).getValue('2');

                    if (val1 instanceof Integer && val2 instanceof Integer) {
                        if ((<Integer> val2).value === 0) {
                            return [new ExceptionConstructor('Div').construct(), true, []];
                        }
                        return [(<Integer> val1).divide(<Integer> val2), false, []];
                    } else if (val1 instanceof Word && val2 instanceof Word) {
                        if ((<Word> val2).value === 0) {
                            return [new ExceptionConstructor('Div').construct(), true, []];
                        }
                        return [(<Word> val1).divide(<Word> val2), false, []];
                    }
                }
                throw new InternalInterpreterError(-1,
                    'Called "div" on value of the wrong type (' + val.constructor.name + ').');
            }), IdentifierStatus.VALUE_VARIABLE],
            'mod':      [new PredefinedFunction('mod', (val: Value) => {
                if (val instanceof RecordValue) {
                    let val1 = (<RecordValue> val).getValue('1');
                    let val2 = (<RecordValue> val).getValue('2');

                    if (val1 instanceof Integer && val2 instanceof Integer) {
                        if ((<Integer> val2).value === 0) {
                            return [new ExceptionConstructor('Div').construct(), true, []];
                        }
                        return [(<Integer> val1).modulo(<Integer> val2), false, []];
                    } else if (val1 instanceof Word && val2 instanceof Word) {
                        if ((<Word> val2).value === 0) {
                            return [new ExceptionConstructor('Div').construct(), true, []];
                        }
                        return [(<Word> val1).modulo(<Word> val2), false, []];
                    }
                }
                throw new InternalInterpreterError(-1,
                    'Called "mod" on value of the wrong type (' + val.constructor.name + ').');
            }), IdentifierStatus.VALUE_VARIABLE],
            '*':        [new PredefinedFunction('*', (val: Value) => {
                if (val instanceof RecordValue) {
                    let val1 = (<RecordValue> val).getValue('1');
                    let val2 = (<RecordValue> val).getValue('2');

                    if (val1 instanceof Integer && val2 instanceof Integer) {
                        let result = (<Integer> val1).multiply(<Integer> val2);
                        if (result.hasOverflow()) {
                            return [new ExceptionConstructor('Overflow').construct(), true, []];
                        }
                        return [result, false, []];
                    } else if (val1 instanceof Word && val2 instanceof Word) {
                        let result = (<Word> val1).multiply(<Word> val2);
                        if (result.hasOverflow()) {
                            return [new ExceptionConstructor('Overflow').construct(), true, []];
                        }
                        return [result, false, []];
                    } else if (val1 instanceof Real && val2 instanceof Real) {
                        let result = (<Real> val1).multiply(<Real> val2);
                        if (result.hasOverflow()) {
                            return [new ExceptionConstructor('Overflow').construct(), true, []];
                        }
                        return [result, false, []];
                    }
                }
                throw new InternalInterpreterError(-1,
                    'Called "*" on value of the wrong type (' + val.constructor.name + ').');
            }), IdentifierStatus.VALUE_VARIABLE],
            '/':        [new PredefinedFunction('/', (val: Value) => {
                if (val instanceof RecordValue) {
                    let val1 = (<RecordValue> val).getValue('1');
                    let val2 = (<RecordValue> val).getValue('2');

                    if (val1 instanceof Real && val2 instanceof Real) {
                        if ((<Real> val2).value === 0) {
                            return [new ExceptionConstructor('Div').construct(), true, []];
                        }
                        return [(<Real> val1).divide(<Real> val2), false, []];
                    }
                }
                throw new InternalInterpreterError(-1,
                    'Called "/" on value of the wrong type (' + val.constructor.name + ').');
            }), IdentifierStatus.VALUE_VARIABLE],
            '+':        [new PredefinedFunction('+', (val: Value) => {
                if (val instanceof RecordValue) {
                    let val1 = (<RecordValue> val).getValue('1');
                    let val2 = (<RecordValue> val).getValue('2');

                    if (val1 instanceof Integer && val2 instanceof Integer) {
                        let result = (<Integer> val1).add(<Integer> val2);
                        if (result.hasOverflow()) {
                            return [new ExceptionConstructor('Overflow').construct(), true, []];
                        }
                        return [result, false, []];
                    } else if (val1 instanceof Word && val2 instanceof Word) {
                        let result = (<Word> val1).add(<Word> val2);
                        if (result.hasOverflow()) {
                            return [new ExceptionConstructor('Overflow').construct(), true, []];
                        }
                        return [result, false, []];
                    } else if (val1 instanceof Real && val2 instanceof Real) {
                        let result = (<Real> val1).add(<Real> val2);
                        if (result.hasOverflow()) {
                            return [new ExceptionConstructor('Overflow').construct(), true, []];
                        }
                        return [result, false, []];
                    }
                }
                throw new InternalInterpreterError(-1,
                    'Called "+" on value of the wrong type (' + val.constructor.name + ').');
            }), IdentifierStatus.VALUE_VARIABLE],
            '-':        [new PredefinedFunction('-', (val: Value) => {
                if (val instanceof RecordValue) {
                    let val1 = (<RecordValue> val).getValue('1');
                    let val2 = (<RecordValue> val).getValue('2');

                    if (val1 instanceof Integer && val2 instanceof Integer) {
                        let result = (<Integer> val1).add((<Integer> val2).negate());
                        if (result.hasOverflow()) {
                            return [new ExceptionConstructor('Overflow').construct(), true, []];
                        }
                        return [result, false, []];
                    } else if (val1 instanceof Word && val2 instanceof Word) {
                        let result = (<Word> val1).add((<Word> val2).negate());
                        if (result.hasOverflow()) {
                            return [new ExceptionConstructor('Overflow').construct(), true, []];
                        }
                        return [result, false, []];
                    } else if (val1 instanceof Real && val2 instanceof Real) {
                        let result = (<Real> val1).add((<Real> val2).negate());
                        if (result.hasOverflow()) {
                            return [new ExceptionConstructor('Overflow').construct(), true, []];
                        }
                        return [result, false, []];
                    }
                }
                throw new InternalInterpreterError(-1,
                    'Called "-" on value of the wrong type (' + val.constructor.name + ').');
            }), IdentifierStatus.VALUE_VARIABLE],

            '<':        [new PredefinedFunction('<', (val: Value) => {
                if (val instanceof RecordValue) {
                    let val1 = (<RecordValue> val).getValue('1');
                    let val2 = (<RecordValue> val).getValue('2');

                    if (val1 instanceof Integer && val2 instanceof Integer) {
                        return [new BoolValue((<Integer> val1).compareTo(<Integer> val2) < 0), false, []];
                    } else if (val1 instanceof Word && val2 instanceof Word) {
                        return [new BoolValue((<Word> val1).compareTo(<Word> val2) < 0), false, []];
                    } else if (val1 instanceof Real && val2 instanceof Real) {
                        return [new BoolValue((<Real> val1).compareTo(<Real> val2) < 0), false, []];
                    } else if (val1 instanceof StringValue && val2 instanceof StringValue) {
                        return [new BoolValue((<StringValue> val1).compareTo(<StringValue> val2) < 0),
                            false, []];
                    } else if (val1 instanceof CharValue && val2 instanceof CharValue) {
                        return [new BoolValue((<CharValue> val1).compareTo(<CharValue> val2) < 0),
                            false, []];
                    }
                }
                throw new InternalInterpreterError(-1,
                    'Called "<" on value of the wrong type (' + val.constructor.name + ').');
            }), IdentifierStatus.VALUE_VARIABLE],
            '>':        [new PredefinedFunction('<', (val: Value) => {
                if (val instanceof RecordValue) {
                    let val1 = (<RecordValue> val).getValue('1');
                    let val2 = (<RecordValue> val).getValue('2');

                    if (val1 instanceof Integer && val2 instanceof Integer) {
                        return [new BoolValue((<Integer> val1).compareTo(<Integer> val2) > 0), false, []];
                    } else if (val1 instanceof Word && val2 instanceof Word) {
                        return [new BoolValue((<Word> val1).compareTo(<Word> val2) > 0), false, []];
                    } else if (val1 instanceof Real && val2 instanceof Real) {
                        return [new BoolValue((<Real> val1).compareTo(<Real> val2) > 0), false, []];
                    } else if (val1 instanceof StringValue && val2 instanceof StringValue) {
                        return [new BoolValue((<StringValue> val1).compareTo(<StringValue> val2) > 0),
                            false, []];
                    } else if (val1 instanceof CharValue && val2 instanceof CharValue) {
                        return [new BoolValue((<CharValue> val1).compareTo(<CharValue> val2) > 0),
                            false, []];
                    }
                }
                throw new InternalInterpreterError(-1,
                    'Called ">" on value of the wrong type (' + val.constructor.name + ').');
            }), IdentifierStatus.VALUE_VARIABLE],
            '<=':       [new PredefinedFunction('<', (val: Value) => {
                if (val instanceof RecordValue) {
                    let val1 = (<RecordValue> val).getValue('1');
                    let val2 = (<RecordValue> val).getValue('2');

                    if (val1 instanceof Integer && val2 instanceof Integer) {
                        return [new BoolValue((<Integer> val1).compareTo(<Integer> val2) <= 0),
                            false, []];
                    } else if (val1 instanceof Word && val2 instanceof Word) {
                        return [new BoolValue((<Word> val1).compareTo(<Word> val2) <= 0), false, []];
                    } else if (val1 instanceof Real && val2 instanceof Real) {
                        return [new BoolValue((<Real> val1).compareTo(<Real> val2) <= 0), false, []];
                    } else if (val1 instanceof StringValue && val2 instanceof StringValue) {
                        return [new BoolValue((<StringValue> val1).compareTo(<StringValue> val2) <= 0),
                            false, []];
                    } else if (val1 instanceof CharValue && val2 instanceof CharValue) {
                        return [new BoolValue((<CharValue> val1).compareTo(<CharValue> val2) <= 0),
                            false, []];
                    }
                }
                throw new InternalInterpreterError(-1,
                    'Called "<=" on value of the wrong type (' + val.constructor.name + ').');
            }), IdentifierStatus.VALUE_VARIABLE],
            '>=':       [new PredefinedFunction('<', (val: Value) => {
                if (val instanceof RecordValue) {
                    let val1 = (<RecordValue> val).getValue('1');
                    let val2 = (<RecordValue> val).getValue('2');

                    if (val1 instanceof Integer && val2 instanceof Integer) {
                        return [new BoolValue((<Integer> val1).compareTo(<Integer> val2) >= 0), false, []];
                    } else if (val1 instanceof Word && val2 instanceof Word) {
                        return [new BoolValue((<Word> val1).compareTo(<Word> val2) >= 0), false, []];
                    } else if (val1 instanceof Real && val2 instanceof Real) {
                        return [new BoolValue((<Real> val1).compareTo(<Real> val2) >= 0), false, []];
                    } else if (val1 instanceof StringValue && val2 instanceof StringValue) {
                        return [new BoolValue((<StringValue> val1).compareTo(<StringValue> val2) >= 0),
                            false, []];
                    } else if (val1 instanceof CharValue && val2 instanceof CharValue) {
                        return [new BoolValue((<CharValue> val1).compareTo(<CharValue> val2) >= 0), false, []];
                    }
                }
                throw new InternalInterpreterError(-1,
                    'Called ">=" on value of the wrong type (' + val.constructor.name + ').');
            }), IdentifierStatus.VALUE_VARIABLE],
            '=':        [new PredefinedFunction('=', (val: Value) => {
                if (val instanceof RecordValue) {
                    let val1 = (<RecordValue> val).getValue('1');
                    let val2 = (<RecordValue> val).getValue('2');

                    return [new BoolValue(val1.equals(val2)), false, []];
                }
                throw new InternalInterpreterError(-1,
                    'Called "=" on value of the wrong type (' + val.constructor.name + ').');
            }), IdentifierStatus.VALUE_VARIABLE],
            '<>':       [new PredefinedFunction('=', (val: Value) => {
                if (val instanceof RecordValue) {
                    let val1 = (<RecordValue> val).getValue('1');
                    let val2 = (<RecordValue> val).getValue('2');

                    return [new BoolValue(!val1.equals(val2)), false, []];
                }
                throw new InternalInterpreterError(-1,
                    'Called "<>" on value of the wrong type (' + val.constructor.name + ').');
            }), IdentifierStatus.VALUE_VARIABLE],

            // ':='
            // 'ref': new ValueIdentifier(new FunctionType(typeVar, new CustomType('ref', typeVar)),
            'true':     [new BoolValue(true), IdentifierStatus.VALUE_CONSTRUCTOR],
            'false':    [new BoolValue(false), IdentifierStatus.VALUE_CONSTRUCTOR],
            'nil':      [new ValueConstructor('nil').construct(), IdentifierStatus.VALUE_CONSTRUCTOR],
            '::':       [new ValueConstructor('::', 1), IdentifierStatus.VALUE_CONSTRUCTOR],
            'Match':    [new ExceptionConstructor('Match').construct(), IdentifierStatus.EXCEPTION_CONSTRUCTOR],
            'Bind':     [new ExceptionConstructor('Bind').construct(), IdentifierStatus.EXCEPTION_CONSTRUCTOR],
            'Div':      [new ExceptionConstructor('Div').construct(), IdentifierStatus.EXCEPTION_CONSTRUCTOR],
            'Overflow': [new ExceptionConstructor('Overflow').construct(), IdentifierStatus.EXCEPTION_CONSTRUCTOR],
            '^':        [new PredefinedFunction('^', (val: Value) => {
                if (val instanceof RecordValue) {
                    let val1 = (<RecordValue> val).getValue('1');
                    let val2 = (<RecordValue> val).getValue('2');

                    if (val1 instanceof StringValue && val2 instanceof StringValue) {
                        return [(<StringValue> val1).concat(val2), false, []];
                    }
                }
                throw new InternalInterpreterError(-1,
                    'Called "^" on value of the wrong type (' + val.constructor.name + ').');
            }), IdentifierStatus.VALUE_VARIABLE],
            'explode':  [new PredefinedFunction('explode', (val: Value) => {
                if (val instanceof StringValue) {
                    return [(<StringValue> val).explode(), false, []];
                }
                throw new InternalInterpreterError(-1,
                    'Called "explode" on value of the wrong type (' + val.constructor.name + ').');
            }), IdentifierStatus.VALUE_VARIABLE],
            'implode':  [new PredefinedFunction('implode', (val: Value) => {
                if (val instanceof ConstructedValue) {
                    return [StringValue.implode(val), false, []];
                }
                throw new InternalInterpreterError(-1,
                    'Called "explode" on value of the wrong type (' + val.constructor.name + ').');
            }), IdentifierStatus.VALUE_VARIABLE],
            '~':        [new PredefinedFunction('~', (val: Value) => {
                if (val instanceof Integer) {
                    let result = (<Integer> val).negate();
                    if (result.hasOverflow()) {
                        return [new ExceptionConstructor('Overflow').construct(), true, []];
                    }
                    return [result, false, []];
                } else if (val instanceof Real) {
                    let result = (<Real> val).negate();
                    if (result.hasOverflow()) {
                        return [new ExceptionConstructor('Overflow').construct(), true, []];
                    }
                    return [result, false, []];
                }
                throw new InternalInterpreterError(-1,
                    'Called "~" on something weird.');
            }), IdentifierStatus.VALUE_VARIABLE],
            'abs':        [new PredefinedFunction('~', (val: Value) => {
                if (val instanceof Integer) {
                    if ((<Integer> val).value >= 0) {
                        return [val, false, []];
                    }
                    let result = (<Integer> val).negate();
                    if (result.hasOverflow()) {
                        return [new ExceptionConstructor('Overflow').construct(), true, []];
                    }
                    return [result, false, []];
                } else if (val instanceof Real) {
                    if ((<Real> val).value >= 0) {
                        return [val, false, []];
                    }
                    let result = (<Real> val).negate();
                    if (result.hasOverflow()) {
                        return [new ExceptionConstructor('Overflow').construct(), true, []];
                    }
                    return [result, false, []];
                }
                throw new InternalInterpreterError(-1,
                    'Called "~" on something weird.');
            }), IdentifierStatus.VALUE_VARIABLE],
            'print':        [new PredefinedFunction('print', (val: Value) => {
                let warns: Warning[] = [];
                if (val instanceof StringValue) {
                    warns.push(new Warning(-2, (<StringValue> val).value));
                } else {
                    warns.push(new Warning(-2, val.toString(undefined)));
                }
                return [new RecordValue(), false, warns];
            }), IdentifierStatus.VALUE_VARIABLE],
            'printLn':        [new PredefinedFunction('printLn', (val: Value) => {
                let warns: Warning[] = [];
                if (val instanceof StringValue) {
                    warns.push(new Warning(-2, (<StringValue> val).value + '\n'));
                } else {
                    warns.push(new Warning(-2, val.toString(undefined) + '\n'));
                }
                return [new RecordValue(), false, warns];
            }), IdentifierStatus.VALUE_VARIABLE]
            // ref, :=, ! are implemented directly within evaluate.
        },
        {},
        {},
        {}
    ),
    [ 0, {} ],
    [ 0, new Map<string, [Type, boolean]>() ],
    {
        'div': new InfixStatus(true, 7, false),
        'mod': new InfixStatus(true, 7, false),
        '*': new InfixStatus(true, 7, false),
        '/': new InfixStatus(true, 7, false),

        '+': new InfixStatus(true, 6, false),
        '-': new InfixStatus(true, 6, false),

        '<': new InfixStatus(true, 4, false),
        '>': new InfixStatus(true, 4, false),
        '<=': new InfixStatus(true, 4, false),
        '>=': new InfixStatus(true, 4, false),

        '::': new InfixStatus(true, 5, true),
        '=': new InfixStatus(true, 4, false),
        '<>': new InfixStatus(true, 4, false),
        ':=': new InfixStatus(true, 3, false),

        '^': new InfixStatus(true, 6, false),
    },
    {
        'nil':      1,
        '::':       1,
        'Match':    1,
        'Bind':     1,
        'Div':      1,
        'Overflow': 1
    }
);

export function getInitialState(): State {
    return initialState.getNestedState();
}
