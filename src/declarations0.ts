import { InternalInterpreterError, ElaborationError,
         EvaluationError, Warning } from './errors';
import { InterpreterOptions, IdentifierStatus, EvaluationResult, EvaluationStack, EvaluationParameters, Declaration, Value, IdCnt } from './basic';
import { ExceptionValue, FunctionValue } from './values';
import { Expression, ValueIdentifier, TypedExpression, PatternExpression, Pattern, Match } from './expressions';
import { Type, TypeVariable, TypeVariableBind, FunctionType } from './types';
import { State } from './state';


export class ValueDeclaration extends Declaration {
// val typeVariableSequence valueBinding
    constructor(public typeVariableSequence: TypeVariable[],
                public valueBinding: ValueBinding[], public id: number = 0) {
        super();
    }

    assertUniqueBinding(state: any, conn: Set<string>): Set<string> {
        for (let i = 0; i < this.valueBinding.length; ++i) {
            this.valueBinding[i].pattern.assertUniqueBinding(state, conn);
            this.valueBinding[i].expression.assertUniqueBinding(state, conn);
        }
        return new Set<string>();
    }

    simplify(): ValueDeclaration {
        let valBnd: ValueBinding[] = [];
        for (let i = 0; i < this.valueBinding.length; ++i) {
            valBnd.push(new ValueBinding(this.valueBinding[i].isRecursive,
                                         this.valueBinding[i].pattern.simplify(),
                                         this.valueBinding[i].expression.simplify()));
        }
        return new ValueDeclaration(this.typeVariableSequence, valBnd, this.id);
    }

    elaborate(state: any, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>, isTopLevel: boolean,
              options: InterpreterOptions):
        [State, Warning[], Map<string, [Type, boolean]>, string] {
        let result: [string, Type][] = [];
        let result2: [string, Type][] = [];
        let r2outdeps: boolean[] = [];

        let warns: Warning[] = [];
        let bnds = tyVarBnd;
        let i = 0;
        for (; i < this.valueBinding.length; ++i) {
            if (this.valueBinding[i].isRecursive) {
                for (let j = i; j < this.valueBinding.length; ++j) {
                    let pat = this.valueBinding[j].pattern;

                    while (pat instanceof TypedExpression) {
                        pat = <Pattern & Expression> pat.expression;
                    }

                    let r = (<ValueIdentifier> pat).name.getText();
                    result.push([r, new TypeVariableBind('\'a', new TypeVariableBind('\'b',
                        new FunctionType(new TypeVariable('\'a'), new TypeVariable('\'b'))))]);
                }

                break;
            }

            let hasOuterDeps = false;
            try {
                let nnstate = state.getNestedState(state.id);
                let nbnds = new Map<string, [Type, boolean]>();
                bnds.forEach((val2: [Type, boolean], key: string) => {
                    nbnds = nbnds.set(key, val2);
                });

                let npars = new Map<string, Type>();
                paramBindings.forEach((val2: Type, key: string) => {
                    // Please forgive me for the following hack.
                    npars = npars.set(key, <Type> (<any> null));
                });

                this.valueBinding[i].getType(this.typeVariableSequence, nnstate, nbnds,
                                             nextName, npars, isTopLevel);
            } catch (e) {
                hasOuterDeps = true;
            }

            let val = this.valueBinding[i].getType(this.typeVariableSequence, state, bnds,
                                                   nextName, paramBindings, isTopLevel);

            warns = warns.concat(val[1]);
            bnds = val[2];
            nextName = val[3];
            state.valueIdentifierId = val[4];

            for (let j = 0; j < (<[string, Type][]> val[0]).length; ++j) {
                r2outdeps.push(hasOuterDeps);
                result2.push((<[string, Type][]> val[0])[j]);
            }
        }


        let nstate = state.getNestedState(state.id);
        for (let j = 0; j < result.length; ++j) {
            if (!options || options.allowSuccessorML !== true) {
                if (!result[j][1].isResolved()) {
                    throw new ElaborationError(
                        'Unresolved record type: Multiple non-agreeing types possible.');
                }
            }
            nstate.setStaticValue(result[j][0], result[j][1], IdentifierStatus.VALUE_VARIABLE);
        }

        let wcp = warns;
        let bcp = new Map<string, [Type, boolean]>();
        bnds.forEach((val: [Type, boolean], key: string) => {
            bcp = bcp.set(key, val);
        });
        let ncp = nextName;
        let ids = nstate.valueIdentifierId;
        let numit = this.valueBinding.length - i + 1;


        for (let l = 0; l < numit * numit + 1; ++l) {
            warns = wcp;
            bnds = new Map<string, [Type, boolean]>();
            bcp.forEach((val: [Type, boolean], key: string) => {
                bnds = bnds.set(key, val);
            });
            nextName = ncp;
            nstate.valueIdentifierId = ids;

            let haschange = false;

            for (let j = i; j < this.valueBinding.length; ++j) {
                // console.log(this + '', j, '/', this.valueBinding.length, ' (1)');
                let hasOuterDeps = false;
                if (!isTopLevel) {
                    // Check whether function uses things bound on a higher level
                    try {
                        let nnstate = nstate.getNestedState(state.id);
                        let nbnds = new Map<string, [Type, boolean]>();
                        bnds.forEach((val2: [Type, boolean], key: string) => {
                            nbnds = nbnds.set(key, val2);
                        });
                        this.valueBinding[j].getType(this.typeVariableSequence, nnstate, nbnds,
                                                     nextName, new Map<string, Type>(), isTopLevel);
                    } catch (e) {
                        hasOuterDeps = true;
                    }
                }
                // console.log(this + '', j, '/', this.valueBinding.length, ' (2)');

                let val = this.valueBinding[j].getType(this.typeVariableSequence, nstate, bnds,
                                                       nextName, paramBindings, isTopLevel);
                warns = warns.concat(val[1]);
                bnds = val[2];
                nextName = val[3];
                nstate.valueIdentifierId = val[4];

                for (let k = 0; k < val[0].length; ++k) {
                    let oldtp = nstate.getStaticValue(val[0][k][0]);
                    // console.log(this + '', oldtp[0].normalize()[0] + '', ' =?= ',  val[0][k][1].normalize()[0] + '');
                    if (oldtp === undefined || !oldtp[0].normalize()[0].equals(val[0][k][1].normalize()[0])) {
                        haschange = true;
                    }
                    // console.log(this + '', k, '/', val[0].length, 2.5);

                    if (!options || options.allowSuccessorML !== true) {
                        if (!val[0][k][1].isResolved()) {
                            throw new ElaborationError(
                                'Unresolved record type: Multiple non-agreeing types possible.');
                        }
                    }

                    if (!hasOuterDeps) {
                        paramBindings = paramBindings.set(val[0][k][0], val[0][k][1]);
                    }
                    nstate.setStaticValue(val[0][k][0], val[0][k][1], IdentifierStatus.VALUE_VARIABLE);
                    // console.log(this + '', k, '/', val[0].length, 3);
                }
            }

            if (!haschange) {
                break;
            } else if (l === numit * numit) {
                throw new ElaborationError(
                    'My brain trembles; too much circularity.');
            }
        }

        for (let j = 0; j < result2.length; ++j) {
            if (!options || options.allowSuccessorML !== true) {
                if (!result2[j][1].isResolved()) {
                    throw new ElaborationError(
                        'Unresolved record type: Multiple non-agreeing types possible.');
                }
            }
            if (!r2outdeps[j]) {
                paramBindings = paramBindings.set(result2[j][0], result2[j][1]);
            }
            nstate.setStaticValue(result2[j][0], result2[j][1], IdentifierStatus.VALUE_VARIABLE);
        }


        return [nstate, warns, bnds, nextName];
    }

    evaluate(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        let state = params.state;
        if (params.step === undefined) {
            params.result = [];
            params.recursives = [];
            params.isRec = false;
            params.step = -1;
        }

        let result: [string, Value][] = params.result;
        let recursives: [string, Value][] = params.recursives;

        let isRec: boolean = params.isRec;

        let step: number = params.step;

        if (step >= 0) {
            let val = params.recResult;
            if (val === undefined) {
                throw new InternalInterpreterError('How is this undefined? ' + JSON.stringify(val));
            }

            if (this.valueBinding[step].isRecursive) {
                isRec = true;
            }

            if (val.hasThrown) {
                return {
                    'newState': state,
                    'value': val.value,
                    'hasThrown': true,
                };
            }
            let matched = this.valueBinding[step].pattern.matches(state, <Value> val.value);
            if (matched === undefined) {
                return {
                    'newState': state,
                    'value': new ExceptionValue('Bind', undefined, 0, 1),
                    'hasThrown': true,
                };
            }

            for (let j = 0; j < (<[string, Value][]> matched).length; ++j) {
                if (!isRec) {
                    result.push((<[string, Value][]> matched)[j]);
                } else {
                    recursives.push((<[string, Value][]> matched)[j]);
                }
            }
        }

        ++step;
        if (step < this.valueBinding.length) {
            params.step = step;
            params.isRec = isRec;
            callStack.push({'next': this, 'params': params});
            callStack.push({
                'next': this.valueBinding[step].expression,
                'params': {'state': state, 'modifiable': params.modifiable, 'recResult': undefined}
            });
            return;
        }

        let nstate = state.getNestedState(state.id);

        for (let j = 0; j < recursives.length; ++j) {
            if (recursives[j][0] === '=') {
                throw new EvaluationError(
                    'All declarations are equal, but some declarations are more equal than others.');
            }

            if (recursives[j][1] instanceof FunctionValue) {
                nstate.setDynamicValue(recursives[j][0], new FunctionValue(
                    (<FunctionValue> recursives[j][1]).state, recursives,
                    (<FunctionValue> recursives[j][1]).body), IdentifierStatus.VALUE_VARIABLE);
            } else {
                nstate.setDynamicValue(recursives[j][0], recursives[j][1], IdentifierStatus.VALUE_VARIABLE);
            }
        }

        for (let j = 0; j < result.length; ++j) {
            if (result[j][0] === '=') {
                throw new EvaluationError(
                    'All declarations are equal, but some declarations are more equal than others.');
            }

            nstate.setDynamicValue(result[j][0], result[j][1], IdentifierStatus.VALUE_VARIABLE);
        }

        return {
            'newState': nstate,
            'value': undefined,
            'hasThrown': false,
        };
    }

    toString(): string {
        let res = 'val';
        for (let i = 0; i < this.typeVariableSequence.length; ++i) {
            res += ' ' + this.typeVariableSequence.toString();
        }
        for (let i = 0; i < this.valueBinding.length; ++i) {
            if (i > 0) {
                res += ' and';
            }
            res += ' ' + this.valueBinding[i];
        }
        return res += ';';
    }
}

export class ValueBinding {
// <rec> pattern = expression
    constructor(public isRecursive: boolean,
                public pattern: Pattern, public expression: Expression) {
    }

    toString(): string {
        let res = '';
        if (this.isRecursive) {
            res += 'rec ';
        }
        res += this.pattern;
        res += ' = ';
        return res + this.expression;
    }

    getType(tyVarSeq: TypeVariable[], state: any, tyVarBnd: Map<string, [Type, boolean]>,
            nextName: string, paramBindings: Map<string, Type>, isTopLevel: boolean):
            [[string, Type][], Warning[], Map<string, [Type, boolean]>, string, IdCnt] {
        let nstate = state.getNestedState(state.id);
        let newBnds = tyVarBnd;

        let tp = this.expression.getType(nstate, newBnds, nextName, new Set<string>(),
                                         false, paramBindings);
        let warns = tp[1];
        let res = this.pattern.matchType(nstate, tp[4], tp[0]);

        try {
            let exhm = new Match([[<PatternExpression> this.pattern, this.expression]]);
            let exwn = exhm.checkExhaustiveness(state);

            let rw: Warning[] = [];
            let seenmsg = new Set<string>();

            for (let w of exwn) {
                if (!seenmsg.has(w.message)) {
                    seenmsg.add(w.message);
                    rw.push(new Warning(w.type === -1 ? -1 : 0, w.message));
                }
            }
            warns = warns.concat(rw);
        } catch (e) {
            // Ignored
        }


        let noBind = new Set<string>();

        if (res === undefined) {
            throw new ElaborationError(
                'Type clash. An expression of type "' + tp[0]
                + '" cannot be assigned to "' + res[1] + '".');
        }

        let ntys: TypeVariable[] = [];
        let seennames: Set<string> = new Set<string>();
        for (let i = 0; i < tyVarSeq.length; ++i) {
            if (seennames.has(tyVarSeq[i].name)) {
                throw new ElaborationError(
                    'I will not let a duplicate type variable name "' + tyVarSeq[i].name
                    + '" disturb my Happy Sugar Life.');
            }
            seennames = seennames.add(tyVarSeq[i].name);

            let nt = tyVarSeq[i].instantiate(state, res[2]);
            if (!(nt instanceof TypeVariable) || (<TypeVariable> nt).domain.length > 0
                || tyVarSeq[i].admitsEquality(state) !== nt.admitsEquality(state)) {
                throw new ElaborationError(
                    'Type clash. An expression of explicit type "' + tyVarSeq[i]
                    + '" cannot have type "' + nt.normalize()[0] + '".');
            }
            ntys.push(<TypeVariable> nt);
        }
        this.expression.getExplicitTypeVariables().forEach((val: TypeVariable) => {
            let nt = val.instantiate(state, res[2]);
            if (!(nt instanceof TypeVariable) || (<TypeVariable> nt).domain.length > 0
                || val.admitsEquality(state) !== nt.admitsEquality(state)) {
                throw new ElaborationError(
                    'Type clash. An expression of explicit type "' + val
                    + '" cannot have type "' + nt.normalize()[0] + '".');
            }
        });

        let valuePoly = !this.isRecursive && !this.expression.isSafe(state);
        let hasFree = false;

        for (let i = 0; i < res[0].length; ++i) {
            if (res[0][i][0] === '=') {
                throw new ElaborationError('All declarations are equal, but some declarations are more equal than others.');
            }
            res[0][i][1] = res[0][i][1].instantiate(state, res[2]);
            if (!isTopLevel) {
                paramBindings = paramBindings.set(res[0][i][0], res[0][i][1]);
            }
            let tv = res[0][i][1].getTypeVariables();
            let free = res[0][i][1].getTypeVariables(true);
            let done = new Set<string>();
            for (let j = ntys.length - 1; j >= 0; --j) {
                if (tv.has(ntys[j].name)) {
                    let dm = <Type[]> tv.get(ntys[j].name);
                    if (res[2].has('$' + ntys[j].name)) {
                        dm = TypeVariable.mergeDomain(dm,
                            (<[TypeVariable, boolean]> res[2].get('$' + ntys[j].name))[0].domain);
                    }
                    res[0][i][1] = new TypeVariableBind(ntys[j].name, res[0][i][1], dm);
                    (<TypeVariableBind> res[0][i][1]).isFree =
                        (<TypeVariableBind> res[0][i][1]).domain.length === 0 && (valuePoly || free.has(ntys[j].name));
                    hasFree = hasFree || (<TypeVariableBind> res[0][i][1]).isFree;

                    done.add(ntys[j].name);
                }
            }
            ntys = [];
            res[0][i][1].getTypeVariables().forEach((dom: Type[], val: string) => {
                if ((isTopLevel || !noBind.has(val)) && !done.has(val)) {
                    let dm = dom;
                    if (res[2].has('$' + val)) {
                        dm = TypeVariable.mergeDomain(dm,
                            (<[TypeVariable, boolean]> res[2].get('$' + val))[0].domain);
                    }
                    ntys.push(new TypeVariable(val, dm));
                }
            });
            for (let j = ntys.length - 1; j >= 0; --j) {
                res[0][i][1] = new TypeVariableBind(ntys[j].name, res[0][i][1], ntys[j].domain);
                (<TypeVariableBind> res[0][i][1]).isFree =
                    (<TypeVariableBind> res[0][i][1]).domain.length === 0 && (valuePoly || free.has(ntys[j].name));

                hasFree = hasFree || (<TypeVariableBind> res[0][i][1]).isFree;
            }
        }

        if (hasFree && isTopLevel) {
            warns.push(new Warning(0, 'Free type variables at top level.\n'));
        }

        return [res[0], warns, res[2], tp[2], tp[5]];
    }
}
