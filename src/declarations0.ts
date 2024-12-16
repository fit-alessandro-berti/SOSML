import { InternalInterpreterError, ElaborationError,
         EvaluationError, Warning } from './errors';
import { InterpreterOptions, IdentifierStatus, EvaluationResult, EvaluationStack, EvaluationParameters, Declaration, Value, IdCnt } from './basic';
import { Token, LongIdentifierToken } from './tokens';
import { ExceptionValue, FunctionValue, ValueConstructor, ExceptionConstructor } from './values';
import { Type, TypeVariable, TypeVariableBind, FunctionType, CustomType, AnyType } from './types';
import { Wildcard } from './expressions';
import { State } from './state';


export abstract class Expression {

    getType(state: any, // i: any containing stuff bound in previous decls
            tyVarBnd: Map<string, [Type, boolean]> = new Map<string, [Type, boolean]>(), /* i:
            Map containing all typevars in current decl, their type, and whether they are free */
            nextName: string = '\'*t0', // i: Next name for a new type variable
            tyVars: Set<string> = new Set<string>(), // i: Set of used tyvars
            isPattern: boolean = false,
            paramBindings: Map<string, Type> = new Map<string, Type>() /* i/o: contains bindings
            for parameters introduced in current decl */
           ): [Type, Warning[], string, Set<string>, Map<string, [Type, boolean]>, IdCnt] {
        throw new InternalInterpreterError('Called "getType" on a derived form. [' + this + ']');
    }

    // Computes the value of an expression, returns [computed value, is thrown exception]
    compute(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        throw new InternalInterpreterError('Called "getValue" on a derived form. [' + this + ']');
    }

    // Returns whether the expression could do nasty stuff (value polymorphism ...)
    isSafe(state: any): boolean {
        return true;
    }

    getExplicitTypeVariables(): Set<TypeVariable> {
        return new Set<TypeVariable>();
    }

    toString(): string {
        throw new InternalInterpreterError(
            'You humans can\'t seem to write bug-free code. What an inferior species.');
    }

    assertUniqueBinding(state: any, conn: Set<string>): Set<string> {
        return new Set<string>();
    }

    abstract simplify(): Expression;
}

export class ValueIdentifier extends Expression implements Pattern {
// op longvid or longvid
    constructor(public name: Token) { super(); }

    getConstructorName(state: any): string | undefined {
        // Returns the name of the constructor corresponding to this
        // ValueIdentifier or undefined if this ValueIdentifier is a variable or
        // ExceptionConstructor

        let cnm = this.name.getText();
        let res = state.getStaticValue(cnm);
        if (res === undefined || res[1] === IdentifierStatus.VALUE_VARIABLE) {
            return undefined;
        }
        return cnm;
    }

    getConstructorList(state: any): string[] | undefined {
        // If this ValueIdentifier corresponds to a ValueConstructor,
        // return all constructor names of the corresponding type.
        // Otherwise return undefined.

        let cnm = this.name.getText();
        let res = state.getStaticValue(cnm);
        if (res === undefined || res[1] === IdentifierStatus.VALUE_VARIABLE) {
            return undefined;
        } else if (res[1] === IdentifierStatus.VALUE_CONSTRUCTOR) {
            // Obtain list of all constructors
            let curtp = res[0];
            while (curtp instanceof TypeVariableBind) {
                curtp = (<TypeVariableBind> curtp).type;
            }
            if (curtp instanceof FunctionType) {
                curtp = (<FunctionType> curtp).returnType;
            }
            if (!(curtp instanceof CustomType)) {
                return undefined;
            }
            let tpname = (<CustomType> curtp).name;
            let stattp = state.getStaticType(tpname);
            if (stattp === undefined) {
                return undefined;
            }
            return stattp.constructors;
        }
        return ['__exn'];
    }

    getType(state: any,
            tyVarBnd: Map<string, [Type, boolean]> = new Map<string, [Type, boolean]>(),
            nextName: string = '\'*t0', tyVars: Set<string> = new Set<string>(),
            isPattern: boolean = false,
            paramBindings: Map<string, Type> = new Map<string, Type>())
        : [Type, Warning[], string, Set<string>, Map<string, [Type, boolean]>, IdCnt] {

        let res: [Type, IdentifierStatus] | undefined = undefined;
        if (this.name instanceof LongIdentifierToken) {
            let st = state.getAndResolveStaticStructure(<LongIdentifierToken> this.name);
            if (st !== undefined) {
                res = st.getValue((<LongIdentifierToken> this.name).id.getText());
                if (res !== undefined) {
                    let nst = new State(0, undefined, st, state.dynamicBasis, [0, {}], 4);
                    res = [res[0].qualify(nst, this.name), res[1]];
                }
            }
        } else {
            let ctp = paramBindings.get(this.name.getText());
            if (ctp !== undefined) {
                // valId exists as param
                res = [ctp, IdentifierStatus.VALUE_VARIABLE];
            } else {
                res = state.getStaticValue(this.name.getText());
            }
        }

        if (res === undefined) {
            throw new ElaborationError('Unbound value identifier "' + this.name.getText() + '".');
        }

        let vars = new Set<string>();
        let frees = new Set<string>();
        let repl = new Map<string, string>();
        while (res[0] instanceof TypeVariableBind) {
            if ((<TypeVariableBind> res[0]).isFree) {
                frees = frees.add((<TypeVariableBind> res[0]).name);
                repl = repl.set((<TypeVariableBind> res[0]).name, (<TypeVariableBind> res[0]).name);
            } else {
                vars = vars.add((<TypeVariableBind> res[0]).name);
            }
            res[0] = (<TypeVariableBind> res[0]).type;
        }

        // force generating another new name
        vars = vars.add('*');

        let nwvar: string[] = [];

        vars.forEach((val: string) => {
            nextName = TypeVariable.getUnusedTypeVariableName(state, nextName, tyVars,
                                                              tyVarBnd, vars);
            let nm = nextName;
            if (val[1] === '\'') {
                nm = '\'' + nm;
            }
            nwvar.push(nm);
            repl = repl.set(val, nm);
        });
        for (let i = 0; i < nwvar.length; ++i) {
            tyVars = tyVars.add(nwvar[i]);
        }

        let r2 = res[0].replaceTypeVariables(repl, frees).instantiate(state, tyVarBnd);

        return [r2, [], nextName, tyVars, tyVarBnd, state.valueIdentifierId];
    }

    matchType(state: any, tyVarBnd: Map<string, [Type, boolean]>, t: Type):
        [[string, Type][], Type, Map<string, [Type, boolean]>] {
        if (this.name instanceof LongIdentifierToken) {
            throw new ElaborationError('Variable names in patterns cannot be qualified.');
        }

        let res = state.getStaticValue(this.name.getText());

        if (res === undefined || res[1] === IdentifierStatus.VALUE_VARIABLE) {
            return [[[this.name.getText(), t.instantiate(state, tyVarBnd)]],
                t.instantiate(state, tyVarBnd), tyVarBnd];
        }

        let tmp = this.getType(state, tyVarBnd, '\'*g0');
        tmp[3].forEach((val: string) => {
            let nname = '\'*p' + val.substring(3);
            if (val[1] === '\'') {
                nname = '\'\'*p' + val.substring(4);
            }
            tmp[4] = tmp[4].set(val, [new TypeVariable(nname), false]);
        });
        res[0] = tmp[0];
        tyVarBnd = tmp[4];

        try {
            let rt = t.merge(state, tyVarBnd, res[0]);
            return [[], rt[0], rt[1]];
        } catch (e) {
            if (!(e instanceof Array)) {
                throw e;
            }
            throw new ElaborationError(
                'Type clash: "' + t.normalize()[0] + '" vs. "'
                + res[0].normalize()[0] + '": ' + e[0]);
        }
    }

    subsumes(state: any, other: PatternExpression): boolean {
        while (other instanceof TypedExpression) {
            other = <PatternExpression> (<TypedExpression> other).expression;
        }

        let cnm = this.getConstructorName(state);
        if (cnm === undefined) {
            return true;
        }
        if (other instanceof ValueIdentifier) {
            let onm = other.getConstructorName(state);
            return cnm === onm;
        }
        return false;
    }

    cover(state: any, rules: PatternExpression[]): Warning[] {
        // Got checked elsewhere already.
        return [];
    }

    matches(state: any, v: Value): [string, Value][] | undefined {
        let res: [Value, IdentifierStatus] | undefined = undefined;
        if (this.name instanceof LongIdentifierToken) {
            let st = state.getAndResolveDynamicStructure(<LongIdentifierToken> this.name);
            if (st !== undefined) {
                res = st.getValue((<LongIdentifierToken> this.name).id.getText());
            }
        } else {
            res = state.getDynamicValue(this.name.getText());
        }

        if (res === undefined || res[1] === IdentifierStatus.VALUE_VARIABLE) {
            return [[this.name.getText(), v]];
        }
        try {
            if (v.equals(res[0])) {
                return [];
            }
        } catch (e) { // This is dirty. It may have been possible to rebind after all
            return [[this.name.getText(), v]];
        }
        return undefined;
    }

    simplify(): ValueIdentifier { return this; }

    toString(): string {
        return this.name.getText();
    }

    compute(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        let state = params.state;
        let res: [Value, IdentifierStatus] | undefined = undefined;
        if (this.name instanceof LongIdentifierToken) {
            let st = state.getAndResolveDynamicStructure(<LongIdentifierToken> this.name);
            if (st !== undefined) {
                res = st.getValue((<LongIdentifierToken> this.name).id.getText());
            }
        } else {
            res = state.getDynamicValue(this.name.getText());
        }

        if (res === undefined) {
            throw new EvaluationError('Unbound value identifier "'
                + this.name.getText() + '".');
        }

        if (res[1] === IdentifierStatus.VALUE_CONSTRUCTOR
            && (<ValueConstructor> res[0]).numArgs === 0) {
            return {
                'newState': undefined,
                'value': (<ValueConstructor> res[0]).construct(),
                'hasThrown': false,
            };
        }
        if (res[1] === IdentifierStatus.EXCEPTION_CONSTRUCTOR
            && (<ExceptionConstructor> res[0]).numArgs === 0) {
            return {
                'newState': undefined,
                'value': (<ExceptionConstructor> res[0]).construct(),
                'hasThrown': false,
            };
        }

        return {
            'newState': undefined,
            'value': res[0],
            'hasThrown': false,
        };
    }

    assertUniqueBinding(state: any, conn: Set<string>): Set<string> {
        if (conn.has(this.name.getText())) {
            return new Set<string>();
        }

        let stt = state.getStaticValue(this.name.getText());
        if (stt !== undefined && stt[1] !== IdentifierStatus.VALUE_VARIABLE) {
            return new Set<string>();
        }
        let dyt = state.getDynamicValue(this.name.getText());
        if (dyt !== undefined && dyt[1] !== IdentifierStatus.VALUE_VARIABLE) {
            return new Set<string>();
        }

        return new Set<string>().add(this.name.getText());
    }
}

export type PatternExpression = Pattern & Expression;

export class TypedExpression extends Expression implements Pattern {
// expression: type (L)
    constructor(public expression: Expression,
                public typeAnnotation: Type) { super(); }

    getExplicitTypeVariables(): Set<TypeVariable> {
        let res = new Set<TypeVariable>();
        this.typeAnnotation.getTypeVariables().forEach((val: Type[], key: string) => {
            res = res.add(new TypeVariable(key, val));
        });
        return res;
    }

    isSafe(state: any): boolean {
        return this.expression.isSafe(state);
    }

    matchType(state: any, tyVarBnd: Map<string, [Type, boolean]>, t: Type):
        [[string, Type][], Type, Map<string, [Type, boolean]>] {
        let tp = (<PatternExpression> this.expression).matchType(state, tyVarBnd, t);

        try {
            let res = tp[1].merge(state, tp[2], this.typeAnnotation.instantiate(state, tp[2]));
            return (<PatternExpression> this.expression).matchType(state, res[1], res[0]);
        } catch (e) {
            if (!(e instanceof Array)) {
                throw e;
            }
            throw new ElaborationError(
                'The annotated type "' + this.typeAnnotation
                + '" does not match the expression\'s type "'
                + tp[1].normalize()[0] + '": ' + e[0]);
        }
    }

    subsumes(state: any, other: PatternExpression): boolean {
        return (<PatternExpression> this.expression).subsumes(state, other);
    }

    cover(state: any, rules: PatternExpression[]): Warning[] {
        throw new InternalInterpreterError('一昨日来やがれ。');
    }

    matches(state: any, v: Value): [string, Value][] | undefined {
        return (<PatternExpression> this.expression).matches(state, v);
    }

    getType(state: any,
            tyVarBnd: Map<string, [Type, boolean]> = new Map<string, [Type, boolean]>(),
            nextName: string = '\'*t0', tyVars: Set<string> = new Set<string>(),
            isPattern: boolean = false,
            paramBindings: Map<string, Type> = new Map<string, Type>())
        : [Type, Warning[], string, Set<string>, Map<string, [Type, boolean]>, IdCnt] {

        let tp = this.expression.getType(state, tyVarBnd, nextName, tyVars,
                                         isPattern, paramBindings);

        try {
            let ann = this.typeAnnotation.instantiate(state, tp[4]);
            let tmp = tp[0].merge(state, tp[4], ann);
            return [tmp[0], tp[1], tp[2], tp[3], tmp[1], tp[5]];
        } catch (e) {
            if (!(e instanceof Array)) {
                throw e;
            }
            throw new ElaborationError(
                'The specified type "' + this.typeAnnotation
                + '" does not match the annotated expression\'s type "'
                + tp[0].normalize()[0] + '": ' + e[0]);
        }
    }

    simplify(): TypedExpression {
        return new TypedExpression(
            this.expression.simplify(), this.typeAnnotation.simplify());
    }

    toString(): string {
        let res = '( ' + this.expression;
        res += ': ' + this.typeAnnotation;
        return res + ' )';
    }

    compute(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        return this.expression.compute(params, callStack);
    }

    assertUniqueBinding(state: any, conn: Set<string>): Set<string> {
        return this.expression.assertUniqueBinding(state, conn);
    }
}

export interface Pattern {
    // Returns which bindings would be created by matching v to this Pattern,
    // or undefined, if v does not match this Pattern.

    matchType(state: any, tyVarBnd: Map<string, [Type, boolean]>, t: Type):
        [[string, Type][], Type, Map<string, [Type, boolean]>];
    cover(state: any, rules: PatternExpression[]): Warning[];
    matches(state: any, v: Value): [string, Value][] | undefined;
    simplify(): PatternExpression;
    toString(indentation: number, oneLine: boolean): string;
    subsumes(state: any, other: PatternExpression): boolean;

    assertUniqueBinding(state: any, conn: Set<string>): Set<string>;
}

export class Match {
    // pat => exp or pat => exp | match
    constructor(public matches: [PatternExpression, Expression][]) { }

    getExplicitTypeVariables(): Set<TypeVariable> {
        let res = new Set<TypeVariable>();
        for (let i = 0; i < this.matches.length; ++i) {
            this.matches[i][0].getExplicitTypeVariables().forEach((val: TypeVariable) => {
                res = res.add(val);
            });
            this.matches[i][1].getExplicitTypeVariables().forEach((val: TypeVariable) => {
                res = res.add(val);
            });
        }
        return res;
    }

    toString(): string {
        let res = '';
        for (let i = 0; i < this.matches.length; ++i) {
            if (i > 0) {
                res += ' | ';
            }
            res += this.matches[i][0];
            res += ' => ' + this.matches[i][1];
        }
        return res;
    }

    compute(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        let state = params.state;
        let value: Value = params.value;
        if (value === undefined) {
            throw new InternalInterpreterError('How is this undefined?');
        }

        for (let i = 0; i < this.matches.length; ++i) {
            let nstate = state.getNestedState(state.id);

            let res = this.matches[i][0].matches(nstate, value);
            if (res !== undefined) {
                for (let j = 0; j < res.length; ++j) {
                    nstate.setDynamicValue(res[j][0], res[j][1], IdentifierStatus.VALUE_VARIABLE);
                }
                callStack.push({
                    'next': this.matches[i][1],
                    'params': {'state': nstate, 'modifiable': params.modifiable, 'recResult': undefined}
                });
                return;
            }
        }
        return {
            'newState': undefined,
            'value': new ExceptionValue('Match', undefined, 0, 0),
            'hasThrown': true,
        };
    }

    getType(state: any,
            tyVarBnd: Map<string, [Type, boolean]> = new Map<string, [Type, boolean]>(),
            nextName: string = '\'*t0', tyVars: Set<string> = new Set<string>(),
            isPattern: boolean = false, checkEx: boolean = true,
            paramBindings: Map<string, Type> = new Map<string, Type>()):
    [Type, Warning[], string, Set<string>, Map<string, [Type, boolean]>, IdCnt] {

        let restp: Type = new FunctionType(new AnyType(), new AnyType());
        let warns: Warning[] = [];
        let bnds = tyVarBnd;
        let keep = new Map<string, [Type, boolean]>();

        for (let i = 0; i < this.matches.length; ++i) {

            if (this.matches[i][0] instanceof ValueIdentifier) {
                let tm = state.getStaticValue((<ValueIdentifier> this.matches[i][0]).name.getText());
                if (tm !== undefined && tm[1] !== IdentifierStatus.VALUE_VARIABLE
                    && tm[0] instanceof FunctionType) {
                    throw new ElaborationError(
                        'Unary constructor "'
                        + (<ValueIdentifier> this.matches[i][0]).name.getText()
                        + '" in the pattern needs an argument.');
                }
            }

            // extract new parameter bindings and override those from a higher level
            let nparbnds = new Map<string, Type>();
            paramBindings.forEach((val: Type, key: string) => {
                nparbnds = nparbnds.set(key, val);
            });
            let npars = this.matches[i][0].assertUniqueBinding(state, new Set<string>());
            npars.forEach((name: string) => {
                nextName = TypeVariable.getUnusedTypeVariableName(state, nextName, tyVars,
                                                                  tyVarBnd);
                tyVars = tyVars.add(nextName);
                nparbnds = nparbnds.set(name, new TypeVariable(nextName));
            });

            let nmap = new Map<string, [Type, boolean]>();
            bnds.forEach((val: [Type, boolean], key: string) => {
                nmap = nmap.set(key, val);
            });

            let r1 = this.matches[i][0].getType(state, bnds, nextName, tyVars, true, nparbnds);
            state.valueIdentifierId = r1[5];
            warns = warns.concat(r1[1]);

            let r2 = this.matches[i][1].getType(state, r1[4], r1[2], r1[3], isPattern, nparbnds);

            warns = warns.concat(r2[1]);
            state.valueIdentifierId = r2[5];
            nextName = r2[2];
            tyVars = r2[3];

            // copy changes back
            nparbnds.forEach((val: Type, key: string) => {
                if (!npars.has(key)) {
                    paramBindings = paramBindings.set(key, val);
                }
            });

            let rtp = new FunctionType(r1[0], r2[0]);

            try {
                [restp, bnds] = restp.merge(state, r2[4], rtp);
                restp = restp.instantiate(state, bnds);
            } catch (e) {
                if (!(e instanceof Array)) {
                    throw e;
                }
                throw new ElaborationError('Match rules disagree on type: ' + e[0]);
            }
            restp = restp.instantiate(state, bnds);
            bnds.forEach((val: [Type, boolean], key: string) => {
                if (key[1] !== '*' || key[2] !== '*') {
                    nmap = nmap.set(key, val);
                } else {
                    keep = keep.set(key, val);
                }
            });
            bnds = nmap;
        }

        let nbnds = new Map<string, [Type, boolean]>();
        bnds.forEach((val: [Type, boolean], key: string) => {
            nbnds = nbnds.set(key, val);
        });
        keep.forEach((val: [Type, boolean], key: string) => {
            nbnds = nbnds.set(key, val);
        });

        if (checkEx) {
            try {

                let tmp = this.checkExhaustiveness(state);

                // Make warns unique
                let res: Warning[] = [];
                let seenmsg = new Set<string>();

                for (let w of tmp) {
                    if (!seenmsg.has(w.message)) {
                        seenmsg.add(w.message);
                        res.push(new Warning(w.type === -1 ? -1 : 0, w.message));
                    }
                }

                warns = warns.concat(res);
            } catch (e) {
                warns.push(new Warning(0, 'How should I know whether "' + this + '" is exhaustive?'
                                       + ' Do I look like friggin\' WIKIP***A to you?!\n' + e.message + '\n'));
            }
        }

        return [restp, warns, nextName, tyVars, bnds, state.valueIdentifierId];
    }

    checkExhaustiveness(state: any): Warning[] {
        return new Wildcard().cover(state,
                                      this.matches.map((a: [PatternExpression, Expression]) => {
            return a[0];
        }));
    }

    simplify(): Match {
        let newMatches: [PatternExpression, Expression][] = [];
        for (let i = 0; i < this.matches.length; ++i) {
            let m: [PatternExpression, Expression] = this.matches[i];
            newMatches.push([m[0].simplify(), m[1].simplify()]);
        }
        return new Match(newMatches);
    }

    assertUniqueBinding(state: any, conn: Set<string>): Set<string> {
        for (let i = 0; i < this.matches.length; ++i) {
            this.matches[i][0].assertUniqueBinding(state, conn);
            this.matches[i][1].assertUniqueBinding(state, conn);
        }
        return new Set<string>();
    }
}


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
