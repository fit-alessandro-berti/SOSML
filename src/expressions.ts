// 10 -- safe
import { InternalInterpreterError, ElaborationError, EvaluationError, ParserError,
         PatternMatchError, Warning } from './errors';
import { IdentifierToken, ConstantToken, IntegerConstantToken, RealConstantToken,
         NumericToken, WordConstantToken, CharacterConstantToken, StringConstantToken,
         LongIdentifierToken } from './tokens';
import { EvaluationResult, EvaluationParameters, EvaluationStack, IdCnt, Declaration, Value } from './basic';
import { IdentifierStatus } from './basic';
import { TypeVariable, RecordType, Type, FunctionType, CustomType, AnyType } from './types';
import { CharValue, StringValue, Integer, Real, Word, ValueConstructor,
         PredefinedFunction, RecordValue, FunctionValue,
         ExceptionValue, ConstructedValue, ReferenceValue, VectorValue, ExceptionConstructor } from './values';
import { State } from './state';
import { ValueBinding, ValueDeclaration } from './declarations0';
import { Expression, ValueIdentifier, TypedExpression, PatternExpression, Pattern, Match } from './declarations0';

export { Expression, ValueIdentifier, TypedExpression, PatternExpression, Pattern, Match };


export class Constant extends Expression implements Pattern {
    constructor(public token: ConstantToken) { super(); }

    matchType(state: any, tyVarBnd: Map<string, [Type, boolean]>, t: Type):
        [[string, Type][], Type, Map<string, [Type, boolean]>] {
        return [[], this.getType(state, tyVarBnd)[0], tyVarBnd];
    }

    subsumes(state: any, other: PatternExpression): boolean {
        while (other instanceof TypedExpression) {
            other = <PatternExpression> (<TypedExpression> other).expression;
        }

        if (other instanceof Constant) {
            return this.toString() === other.toString();
        }
        return false;
    }

    cover(state: any, rules: PatternExpression[]): Warning[] {
        throw new InternalInterpreterError('一昨日来やがれ。');
    }

    matches(state: any, v: Value): [string, Value][] | undefined {
        // the call to compute just gets two states but will not use them
        let val = this.compute({'state': state, 'modifiable': state, 'recResult': undefined}, []);
        if (val === undefined || val.value === undefined) {
            throw new InternalInterpreterError(
                'Right now, I\'m the happiest interpreter in the world!');
        }
        if ((<Value> val.value).equals(v)) {
            return [];
        } else {
            return undefined;
        }
    }

    getType(state: any,
            tyVarBnd: Map<string, [Type, boolean]> = new Map<string, [Type, boolean]>(),
            nextName: string = '\'*t0', tyVars: Set<string> = new Set<string>(),
            isPattern: boolean = false,
            paramBindings: Map<string, Type> = new Map<string, Type>())
        : [Type, Warning[], string, Set<string>, Map<string, [Type, boolean]>, IdCnt] {

        if (this.token instanceof IntegerConstantToken || this.token instanceof NumericToken) {
            return [new CustomType('int'), [], nextName, tyVars, tyVarBnd,
                state.valueIdentifierId];
        } else if (this.token instanceof RealConstantToken) {
            return [new CustomType('real'), [], nextName, tyVars, tyVarBnd,
                state.valueIdentifierId];
        } else if (this.token instanceof WordConstantToken) {
            return [new CustomType('word'), [], nextName, tyVars, tyVarBnd,
                state.valueIdentifierId];
        } else if (this.token instanceof CharacterConstantToken) {
            return [new CustomType('char'), [], nextName, tyVars, tyVarBnd,
                state.valueIdentifierId];
        } else if (this.token instanceof StringConstantToken) {
            return [new CustomType('string'), [], nextName, tyVars, tyVarBnd,
                state.valueIdentifierId];
        } else {
            throw new InternalInterpreterError(
                '"' + this + '" does not seem to be a valid constant.');
        }
    }

    simplify(): Constant { return this; }

    toString(): string {
        return this.token.getText();
    }

    compute(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        let val: Value | undefined = undefined;
        if (this.token instanceof IntegerConstantToken || this.token instanceof NumericToken) {
            val = new Integer((<IntegerConstantToken | NumericToken> this.token).value);
        } else if (this.token instanceof RealConstantToken) {
            val = new Real((<RealConstantToken> this.token).value);
        } else if (this.token instanceof WordConstantToken) {
            val = new Word((<WordConstantToken> this.token).value);
        } else if (this.token instanceof CharacterConstantToken) {
            val = new CharValue((<CharacterConstantToken> this.token).value);
        } else if (this.token instanceof StringConstantToken) {
            val = new StringValue((<StringConstantToken> this.token).value);
        }

        if (val === undefined) {
            throw new EvaluationError('You sure that this is a constant?');
        }

        return {
            'newState': undefined,
            'value': val,
            'hasThrown': false,
        };
    }
}

export class Record extends Expression implements Pattern {
// { lab = exp, ... } or { }
// a record(pattern) is incomplete if it ends with '...'
    constructor(public complete: boolean,
                public entries: [string, Expression|PatternExpression][]) {
        super();
        this.entries.sort();
        for (let i = 1; i < this.entries.length; ++i) {
            if (this.entries[i][0] === this.entries[i - 1][0]) {
                throw new ElaborationError(
                    'Label "' + this.entries[i][0] + '" occurs more than once in the same record.');
            }
        }
    }

    hasConstant(): boolean {
        for (let exp of this.entries) {
            if (exp[1] instanceof Constant) {
                return true;
            }
            if (exp[1] instanceof Record && (<Record> exp[1]).hasConstant()) {
                return true;
            }
        }
        return false;
    }

    subsumes(state: any, other: PatternExpression): boolean {
        // Checks whether the other record is a special case of this record
        // Assumes that entries are sorted

        while (other instanceof TypedExpression) {
            other = <PatternExpression> (<TypedExpression> other).expression;
        }

        if (!(other instanceof Record)) {
            return false;
        }

        let i = 0;
        let j = 0;
        while (i < this.entries.length || j < (<Record> other).entries.length) {
            let curi = (i < this.entries.length ? this.entries[i] : undefined);
            let curj = (j < (<Record> other).entries.length ?
                        (<Record> other).entries[j] : undefined);

            if (curj === undefined || (curi !== undefined && curi[0] < curj[0])) {
                if (curi === undefined) {
                    break;
                }
                if ((<PatternExpression> curi[1]).subsumes(state, new Wildcard())) {
                    ++i; continue;
                }
                // Entry that is missing in other (and hence assumed to be a wildcard)
                // exists in this and is not a Wildcard or variable
                return false;
            }
            if (curi !== undefined && curj !== undefined && curi[0] === curj[0]) {
                if ((<PatternExpression> curi[1]).subsumes(state, <PatternExpression> curj[1])) {
                    ++i; ++j;
                    continue;
                }
                return false;
            }
            if (curi === undefined || (curj !== undefined && curi[0] > curj[0])) {
                if (curj === undefined) {
                    break;
                }
                if (new Wildcard().subsumes(state, <PatternExpression> curj[1])) {
                    ++j; continue;
                }
                return false;
            }
        }
        return true;
    }

    getExplicitTypeVariables(): Set<TypeVariable> {
        let res = new Set<TypeVariable>();
        for (let i = 0; i < this.entries.length; ++i) {
            this.entries[i][1].getExplicitTypeVariables().forEach((val: TypeVariable) => {
                res = res.add(val);
            });
        }
        return res;
    }

    hasEntry(name: string): boolean {
        for (let i = 0; i < this.entries.length; ++i) {
            if (this.entries[i][0] === name) {
                return true;
            }
        }
        return false;
    }

    getEntry(name: string): Expression | PatternExpression {
        for (let i = 0; i < this.entries.length; ++i) {
            if (this.entries[i][0] === name) {
                return this.entries[i][1];
            }
        }
        throw new InternalInterpreterError(
            'Yeah, "' + name + '" would be nice to have.');
    }

    isSafe(state: any): boolean {
        for (let i = 0; i < this.entries.length; ++i) {
            if (!this.entries[i][1].isSafe(state)) {
                return false;
            }
        }
        return true;
    }

    matchType(state: any, tyVarBnd: Map<string, [Type, boolean]>, t: Type):
        [[string, Type][], Type, Map<string, [Type, boolean]>] {
        if (!(t instanceof RecordType)) {
            t = t.instantiate(state, tyVarBnd);
        }
        if (t instanceof TypeVariable || (t instanceof RecordType && !(<RecordType> t).complete)) {
            let ntype = new Map<string, Type>();
            let oldname = '';
            let oldIsFree = false;
            if (t instanceof TypeVariable) {
                oldname = (<TypeVariable> t).name;
                oldIsFree = (<TypeVariable> t).isFree;
            } else if (t instanceof RecordType && (<RecordType> t).sourceTyVar !== undefined) {
                oldname = <string> (<RecordType> t).sourceTyVar;
                if (tyVarBnd.has(oldname)) {
                    oldIsFree = (<[Type, boolean]> tyVarBnd.get(oldname))[1];
                } else {
                    throw new InternalInterpreterError('An incomplete record type just died.');
                }
            } else {
                throw new InternalInterpreterError('An incomplete record type just died.');
            }
            for (let i = 0; i < this.entries.length; ++i) {
                let ntv = new TypeVariable(oldname + '*' + this.entries[i][0]);
                ntv.isFree = oldIsFree;
                ntype = ntype.set(this.entries[i][0], ntv);
            }
            let tp = new RecordType(ntype, this.complete, this.complete ? undefined : oldname);

            if (tyVarBnd.has('@' + oldname)) {
                let od = <[Type, boolean]> tyVarBnd.get('@' + oldname);
                try {
                    let mg = (<Type> od[0]).merge(state, tyVarBnd, tp);
                    tyVarBnd = tyVarBnd.set('@' + oldname, [mg[0], oldIsFree]);
                    if (this.complete) {
                        tyVarBnd = tyVarBnd.set(oldname, [mg[0], oldIsFree]);
                    }
                } catch (e) {
                    if (!(e instanceof Array)) {
                        throw e;
                    }
                    throw new ElaborationError('Type mismatch "'
                                               + (<Type> od[0]).normalize()[0] + '" vs. "'
                                               + tp.normalize()[0] + '": ' + e[0]);
                }
            } else {
                tyVarBnd = tyVarBnd.set('@' + oldname, [tp, oldIsFree]);
            }
            t = tp;
        }


        if (!(t instanceof RecordType)) {
            throw new ElaborationError(
                'Expected pattern of a record type, got "' + t.typeName() + '".');
        }
        if (this.complete && (<RecordType> t).complete &&
            this.entries.length !== (<RecordType> t).elements.size) {
            throw new ElaborationError(
                'Expected a record type with ' + this.entries.length + ' entries,'
                + ' but the given record has ' + (<RecordType> t).elements.size + ' entries.');
        }

        let res: [string, Type][] = [];
        let rtp = new Map<string, Type>();
        let bnd = tyVarBnd;
        let elmts = new Set<string>();

        for (let i = 0; i < this.entries.length; ++i) {
            elmts = elmts.add(this.entries[i][0]);
            if (!(<RecordType> t).hasType(this.entries[i][0])) {
                if (!(<RecordType> t).complete) {
                    // TODO: get a proper unique name
                    (<RecordType> t).setType(this.entries[i][0],
                                             new TypeVariable('*z' + i + '*'
                                                              + t.toString().length + '*'));
                } else {
                    throw new ElaborationError(
                        'Record is missing entry "' + this.entries[i][0] + '".');
                }
            }
            let cur = (<PatternExpression> this.entries[i][1]).matchType(
                state, bnd, (<RecordType> t).getType(this.entries[i][0]));

            res = res.concat(cur[0]);
            rtp = rtp.set(this.entries[i][0], cur[1]);
            bnd = cur[2];
        }

        let restp = new RecordType(rtp, this.complete || (<RecordType> t).complete);
        try {
            let mg = restp.merge(state, bnd, t);
            return [res, mg[0], mg[1]];
        } catch (e) {
            if (!(e instanceof Array)) {
                throw e;
            }
            throw new ElaborationError('Type mismatch "' + t.normalize()[0] + '" vs. "'
                                       + restp.normalize()[0] + '": ' + e[0]);
        }

    }

    cover(state: any, rules: PatternExpression[]): Warning[] {
        // Check that first entry alone is exhaustive,
        // then split all rules according to first entry and check that they are
        // exhaustive as well

        if (this.entries.length === 0) {
            if (rules.length === 0) {
                return [
                    new Warning(-1, 'Pattern matching is not exhaustive.\n')
                ];
            }
            return [];
        }

        let sname = this.entries[0][0];
        let nrules: PatternExpression[] = [];
        let sprules = new Map<string, PatternExpression[]>();
        let wc = new Wildcard();

        let wcrules: PatternExpression[] = [];
        let hadConstants = false;

        let cons: string[] = [];

        let oldrecs: PatternExpression[] = [];
        let ocnr: [string, Expression|PatternExpression][][] = [];
        let subsmst: [boolean, boolean][] = []; // Rule i [is sub; subs other]

        let warns: Warning[] = [];

        let breakNext: string | undefined = undefined;
        let specialNeeds = new Set<string>();
        for (let rule of rules) {
            if (breakNext !== undefined) {
                warns.push(new Warning(0, 'Rules after "' + breakNext
                    + '" unused in pattern matching.\n'));
                break;
            }

            if (!(rule instanceof Record)) {
                throw new InternalInterpreterError('Pipiru piru piru pipiru pii!');
            }

            if ((<Record> rule).hasConstant()) {
                continue;
            }

            if (rule.subsumes(state, this)) {
                // This rule rules them all
                breakNext = rule.toString();
            }

            let cnrule: [string, Expression|PatternExpression][] = [];
            let key: PatternExpression | undefined = undefined;
            for (let nr of (<Record> rule).entries) {
                if (nr[0] === sname) {
                    key = <PatternExpression> nr[1];
                    if (key instanceof FunctionApplication) {
                        // For constructed values, also the argument should be
                        // exh, hence also test the argument of the key
                        cnrule.push([nr[0], (<FunctionApplication> key).argument]);
                    }
                } else {
                    cnrule.push(nr);
                }
            }
            if (key === undefined) {
                key = wc;
            }

            if (key instanceof ValueIdentifier) {
                // Check if variable, if it is, replace by Wildcard
                let cnm = (<ValueIdentifier> key).getConstructorName(state);
                if (cnm === undefined) {
                    key = wc;
                } else if (cons.length === 0) {
                    // Obtain list of all constructors
                    let stattp = (<ValueIdentifier> key).getConstructorList(state);
                    if (stattp === undefined) {
                        throw new InternalInterpreterError('Pipiru piru piru pipiru pii!');
                    }
                    cons = stattp;
                }
            }

            nrules.push(key);
            if (key instanceof Record) {
                // Check if rule contains any constant value,
                // if it does, the rule is irrelavant
                if ((<Record> key).hasConstant()) {
                    continue;
                }

                let nrecs: PatternExpression[] = [];

                subsmst.push([false, false]);
                // The following could be implemented faster. I know. I don't care.
                for (let i = 0; i < oldrecs.length; ++i) {
                    let kso = key.subsumes(state, oldrecs[i]);
                    let osk = oldrecs[i].subsumes(state, key);
                    if (kso) {
                        // This rule is more general than old rule,
                        // hence, add this rule to the class of old rules
                        subsmst[i][0] = true;
                        subsmst[oldrecs.length][1] = true;

                        let old = <PatternExpression[]> sprules.get(i + '');
                        old.push(new Record((<Record> rule).complete, cnrule));
                        sprules = sprules.set(i + '', old);
                    }
                    if (osk) {
                        // There is a rule that is more general than this rule
                        // hence, add that rule to the class of this rule
                        subsmst[i][1] = true;
                        subsmst[oldrecs.length][0] = true;

                        nrecs.push(new Record((<Record> rule).complete, ocnr[i]));
                    }
                }

                nrecs.push(new Record((<Record> rule).complete, cnrule));
                sprules = sprules.set(oldrecs.length + '', wcrules.concat(nrecs));
                oldrecs.push(key);
                ocnr.push(cnrule);
                continue;
            }

            let specialN = false;
            let kid = key.toString();
            if (key instanceof FunctionApplication) {
                specialN = true;
                let fnname = (<FunctionApplication> key).func;
                if (!(fnname instanceof ValueIdentifier)) {
                    throw new InternalInterpreterError(
                        'There. Just another unfortunate accident.');
                }
                kid = (<ValueIdentifier> fnname).name.getText();

                if (cons.length === 0) {
                    // Obtain list of all constructors
                    let stattp = (<ValueIdentifier> fnname).getConstructorList(state);
                    if (stattp === undefined) {
                        throw new InternalInterpreterError('Pipiru piru piru pipiru pii!');
                    }
                    cons = stattp;
                }
            }
            kid = kid + key.constructor.name;
            if (specialN) {
                specialNeeds = specialNeeds.add(kid);
            }

            if (key instanceof Constant) {
                // Ignore these rules, as they will never contribute to completeness.
                continue;
            }

            if (key instanceof Wildcard) {
                // Cache rule for new non wc-rules
                wcrules.push(new Record((<Record> rule).complete, cnrule));

                // add rule to existing wc-rules
                let nsprules = new Map<string, PatternExpression[]>();
                sprules.forEach((sprule: PatternExpression[], key2: string) => {
                    sprule.push(new Record((<Record> rule).complete, cnrule));
                    nsprules = nsprules.set(key2, sprule);
                });
                sprules = nsprules;
                continue;
            }

            if (!sprules.has(kid)) {
                sprules = sprules.set(kid, wcrules.concat([new Record(
                    (<Record> rule).complete, cnrule)]));
            } else {
                let old = <PatternExpression[]> sprules.get(kid);
                old.push(new Record((<Record> rule).complete, cnrule));
                sprules = sprules.set(kid, old);
            }
        }

        warns = warns.concat(wc.cover(state, nrules).filter((w: Warning) => w.type === -1));

        // Generate a new Record without the key already processed
        let nentries: [string, Expression|PatternExpression][] = [];
        for (let i = 1; i < this.entries.length; ++i) {
            nentries.push(this.entries[i]);
        }
        let newrec = new Record(true, nentries);

        let nentries2: [string, Expression|PatternExpression][] = [];
        nentries2.push([this.entries[0][0], new Wildcard()]);
        for (let i = 1; i < this.entries.length; ++i) {
            nentries2.push(this.entries[i]);
        }
        let newrec2 = new Record(true, nentries2);


        sprules.forEach((sprule: PatternExpression[], key: string) => {
            if (specialNeeds.has(key)) {
                let nsprule: PatternExpression[] = [];
                // Make sure that all rules (wihch are records) still contain all required entries
                for (let pt of sprule) {
                    if (!(pt instanceof Record)) {
                        nsprule.push(pt);
                    } else {
                        if (!(<Record> pt).hasEntry(sname)) {
                            // Something went missing, so re-add it
                            let nentries3: [string, Expression|PatternExpression][] = [];
                            nentries3.push([sname, new Wildcard()]);
                            for (let i = 0; i < (<Record> pt).entries.length; ++i) {
                                nentries3.push((<Record> pt).entries[i]);
                            }
                            nsprule.push(new Record(true, nentries3));

                        } else {
                            nsprule.push(pt);
                        }
                    }
                }
                warns = warns.concat(newrec2.cover(state, nsprule).filter(
                (w: Warning) => w.type === -1)); // We needn't filter these if we wanted to warn
                // for non-disjoint rules.
            } else {
                warns = warns.concat(newrec.cover(state, sprule).filter(
                (w: Warning) => w.type === -1)); // We needn't filter these if we wanted to warn
                // for non-disjoint rules.
            }
        });

        // If all other rules are constants, no other rule exists, or not all
        // possible constructors have a rule, these cases are caught by a wildcard.
        // Hence, check that rules starting with a wildcard are exhaustive as well.
        if (hadConstants || sprules.size === 0 || sprules.size < cons.length) {
            // Wildcards actually need to be checked after all
            warns = warns.concat(newrec.cover(state, wcrules));
        }

        return warns;
    }

    matches(state: any, v: Value): [string, Value][] | undefined {
        if (!(v instanceof RecordValue)) {
            return undefined;
        }
        if (this.complete && this.entries.length !== (<RecordValue> v).entries.size) {
            return undefined;
        }

        let res: [string, Value][] = [];

        for (let i = 0; i < this.entries.length; ++i) {
            if (!(<RecordValue> v).hasValue(this.entries[i][0])) {
                return undefined;
            }
            let cur = (<PatternExpression> this.entries[i][1]).matches(
                state, (<RecordValue> v).getValue(this.entries[i][0]));
            if (cur === undefined) {
                return cur;
            }
            for (let j = 0; j < cur.length; ++j) {
                res.push(cur[j]);
            }
        }
        return res;
    }

    getType(state: any,
            tyVarBnd: Map<string, [Type, boolean]> = new Map<string, [Type, boolean]>(),
            nextName: string = '\'*t0', tyVars: Set<string> = new Set<string>(),
            isPattern: boolean = false,
            paramBindings: Map<string, Type> = new Map<string, Type>())
        : [Type, Warning[], string, Set<string>, Map<string, [Type, boolean]>, IdCnt] {

        let e: Map<string, Type> = new Map<string, Type>();
        let warns: Warning[] = [];
        let bnds = tyVarBnd;
        for (let i: number = 0; i < this.entries.length; ++i) {
            let name: string = this.entries[i][0];
            let exp: Expression = this.entries[i][1];
            if (e.has(name)) {
                throw new ElaborationError(
                    'Label "' + name + '" occurs more than once in the same record.');
            }

            if (exp instanceof ValueIdentifier && isPattern) {
                let tm = state.getStaticValue((<ValueIdentifier> exp).name.getText());
                if (tm !== undefined && tm[1] !== IdentifierStatus.VALUE_VARIABLE
                    && tm[0] instanceof FunctionType) {
                    throw new ElaborationError(
                        'Unary constructor in the pattern needs an argument.');
                }
            }


            let tmp = exp.getType(state, bnds, nextName, tyVars, isPattern, paramBindings);

            warns = warns.concat(tmp[1]);
            nextName = tmp[2];
            tyVars = tmp[3];
            tmp[4].forEach((val: [Type, boolean], key: string) => {
                bnds = bnds.set(key, val);
            });
            state.valueIdentifierId = tmp[5];
            e = e.set(name, tmp[0]);
        }
        return [new RecordType(e, this.complete), warns, nextName, tyVars, bnds, state.valueIdentifierId];
    }

    simplify(): Record {
        let newEntries: [string, Expression][] = [];
        for (let i = 0; i < this.entries.length; ++i) {
            let e: [string, Expression] = this.entries[i];
            newEntries.push([e[0], e[1].simplify()]);
        }
        return new Record(this.complete, newEntries);
    }

    toString(): string {
        let isTuple = this.entries.length !== 1;
        for (let i = 0; i < this.entries.length; ++i) {
            if (this.entries[i][0] !== ('' + (i + 1))) {
                isTuple = false;
            }
        }
        let result: string = '{';
        if (isTuple) {
            result = '(';
            for (let i = 0; i < this.entries.length; ++i) {
                if (i > 0) {
                    result += ', ';
                }
                result += this.entries[i][1].toString();
            }
            return result + ')';
        }

        let first: boolean = true;
        for (let i = 0; i < this.entries.length; ++i) {
            if (!first) {
                result += ', ';
            }
            first = false;
            result += this.entries[i][0] + ' = '
                + this.entries[i][1];
        }
        if (!this.complete) {
            if (!first) {
                result += ', ';
            }
            result += '...';
        }
        return result + '}';
    }

    compute(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        let state = params.state;
        if (params.step === undefined) {
            params.step = -1;
            params.nentr = new Map<string, Value>();
        }

        let nentr: Map<string, Value> = params.nentr;
        let step: number = params.step;

        if (step >= 0) {
            let val = params.recResult;
            if (val === undefined) {
                throw new InternalInterpreterError('Namjala Latak');
            }

            if (val.hasThrown) {
                // Computing some expression failed
                return {
                    'newState': undefined,
                    'value': val.value,
                    'hasThrown': true,
                };
            }
            nentr = nentr.set(this.entries[step][0], <Value> val.value);
        }
        ++step;
        if (step < this.entries.length) {
            params.step = step;
            params.nentr = nentr;
            callStack.push({'next': this, 'params': params});
            callStack.push({
                'next': this.entries[step][1],
                'params': {'state': state, 'modifiable': params.modifiable, 'recResult': undefined}
            });
            return;
        }

        return {
            'newState': undefined,
            'value': new RecordValue(nentr),
            'hasThrown': false,
        };
    }

    assertUniqueBinding(state: any, conn: Set<string>): Set<string> {
        let seen = new Set<string>();

        for (let i = 0; i < this.entries.length; ++i) {
            let cur = this.entries[i][1].assertUniqueBinding(state, conn);

            cur.forEach((v: string) => {
                if (seen.has(v)) {
                    throw new ParserError('Don\'t try to rebind "' + v + '" "' + v + '" in the'
                        + ' same pattern... Sorry, I stuttered.');
                }
                seen = seen.add(v);
            });
        }

        return seen;
    }
}

export class LocalDeclarationExpression extends Expression {
// let dec in exp1; ...; expn end
// A sequential expression exp1; ... ; expn is represented as such,
// despite the potentially missing parentheses
    constructor(public declaration: Declaration, public expression: Expression) { super(); }

    simplify(): LocalDeclarationExpression {
        return new LocalDeclarationExpression(this.declaration.simplify(), this.expression.simplify());
    }

    toString(): string {
        let res = 'let ' + this.declaration;
        res += ' in ' + this.expression + ' end';
        return res;
    }

    isSafe(state: any): boolean {
        return false;
    }

    getType(state: any,
            tyVarBnd: Map<string, [Type, boolean]> = new Map<string, [Type, boolean]>(),
            nextName: string = '\'*t0', tyVars: Set<string> = new Set<string>(),
            isPattern: boolean = false,
            paramBindings: Map<string, Type> = new Map<string, Type>())
        : [Type, Warning[], string, Set<string>, Map<string, [Type, boolean]>, IdCnt] {

        let nparbnd = new Map<string, Type>();
        paramBindings.forEach((val: Type, key: string) => {
            nparbnd = nparbnd.set(key, val);
        });

        let nstate = state.getNestedState(state.id);
        let res = this.declaration.elaborate(nstate, tyVarBnd, nextName, nparbnd, false);
        let r2 = this.expression.getType(res[0], res[2], res[3], tyVars, isPattern, nparbnd);
        return [r2[0], res[1].concat(r2[1]), r2[2], r2[3], r2[4], r2[5]];
    }

    compute(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        let state = params.state;
        if (params.step === undefined) {
            params.step = -1;
        }

        let step: number = params.step;

        if (step === -1) {
            let nstate = state.getNestedState(0).getNestedState(state.id);
            params.step = step + 1;
            params.state = state;
            callStack.push({'next': this, 'params': params});
            callStack.push({
                'next': this.declaration,
                'params': {'state': nstate, 'modifiable': params.modifiable, 'recResult': undefined}
            });
            return;
        }

        if (step === 0) {
            let res = params.recResult;
            if (res === undefined
                || res.newState === undefined) {
                throw new InternalInterpreterError(
                    'You see, even I sleep at night and wake up in the morning');
            }
            let nstate = <State> res.newState;
            if (res.hasThrown) {
                return {
                    'newState': undefined,
                    'value': res.value,
                    'hasThrown': true,
                };
            }

            callStack.push({
                'next': this.expression,
                'params': {'state': nstate, 'modifiable': params.modifiable, 'recResult': undefined}
            });
            return;
        }
        let nres = params.recResult;
        if (nres === undefined) {
            throw new InternalInterpreterError('I just gave this "dying" a try...');
        }
        return {
            'newState': undefined,
            'value': nres.value,
            'hasThrown': nres.hasThrown,
        };
    }

    assertUniqueBinding(state: any, conn: Set<string>): Set<string> {
        this.declaration.assertUniqueBinding(state, conn);
        this.expression.assertUniqueBinding(state, conn);
        return new Set<string>();
    }
}

// May represent either a function application or a constructor with an argument
export class FunctionApplication extends Expression implements Pattern {
// function argument
    constructor(public func: Expression,
                public argument: Expression|PatternExpression) { super(); }

    getExplicitTypeVariables(): Set<TypeVariable> {
        let res = new Set<TypeVariable>();
        this.func.getExplicitTypeVariables().forEach((val: TypeVariable) => {
            res = res.add(val);
        });
        this.argument.getExplicitTypeVariables().forEach((val: TypeVariable) => {
            res = res.add(val);
        });
        return res;
    }

    isSafe(state: any): boolean {
        if (!(this.func instanceof ValueIdentifier)) {
            return false;
        }
        let f = state.getStaticValue((<ValueIdentifier> this.func).name.getText());
        if (f === undefined || (<ValueIdentifier> this.func).name.getText() === 'ref') {
            return false;
        }
        return f[1] !== IdentifierStatus.VALUE_VARIABLE;
    }

    matchType(state: any, tyVarBnd: Map<string, [Type, boolean]>, t: Type):
        [[string, Type][], Type, Map<string, [Type, boolean]>] {
        if (!(this.func instanceof ValueIdentifier)) {
            throw new ElaborationError('Non-identifier applied to a pattern.');
        }

        let ti = state.getStaticValue((<ValueIdentifier> this.func).name.getText());
        if (ti === undefined || ti[1] === IdentifierStatus.VALUE_VARIABLE) {
            throw new ElaborationError('Expected a constructor, "'
                + (<ValueIdentifier> this.func).name.getText() + '" isn\'t.');
        }

        let tmp = this.func.getType(state, tyVarBnd, '\'g0');
        tmp[3].forEach((val: string) => {
            let nname = '\'p' + val.substring(2);
            if (val[1] === '\'') {
                nname = '\'\'p' + val.substring(3);
            }
            tmp[4] = tmp[4].set(val, [new TypeVariable(nname), false]);
        });
        ti[0] = tmp[0];
        tyVarBnd = tmp[4];

        if (!(ti[0] instanceof FunctionType)) {
            throw new ElaborationError('This is truly more...slothful.');
        }

        try {
            let ft = <FunctionType> ti[0];
            let mg = ft.returnType.merge(state, tyVarBnd, t);

            let res = (<PatternExpression> this.argument).matchType(state, mg[1],
                ft.parameterType.instantiate(state, mg[1]));
            return [res[0], mg[0], res[2]];
        } catch (e) {
            if (!(e instanceof Array)) {
                throw e;
            }
            throw new ElaborationError('Merge failed: ' + e[0]);
        }
    }

    subsumes(state: any, other: PatternExpression): boolean {
        while (other instanceof TypedExpression) {
            other = <PatternExpression> (<TypedExpression> other).expression;
        }

        if (other instanceof FunctionApplication) {
            return (<PatternExpression> this.func).subsumes(
                state, <PatternExpression> (<FunctionApplication> other).func);
        }
        return false;
    }

    cover(state: any, rules: PatternExpression[]): Warning[] {
        // Remove outermost function application and check parameters
        return (<PatternExpression> this.argument).cover(
            state, rules.map((rule: PatternExpression) => {
            if (!(rule instanceof FunctionApplication)) {
                throw new InternalInterpreterError(
                    'I received much data that did not compute.');
            }
            return <PatternExpression> (<FunctionApplication> rule).argument;
        }));
    }

    matches(state: any, v: Value): [string, Value][] | undefined {
        if (v instanceof FunctionValue) {
            throw new EvaluationError(
                'You simply cannot match function values.');
        } else if (v instanceof ConstructedValue) {
            if (this.func instanceof ValueIdentifier
                && (<ValueIdentifier> this.func).name.getText()
                === (<ConstructedValue> v).constructorName
                && (<ValueIdentifier> this.func).name.getText() !== 'ref') {
                if ((<ConstructedValue> v).argument !== undefined) {
                    return (<PatternExpression> this.argument).matches(
                        state, <Value> (<ConstructedValue> v).argument);
                }
                return [];
            }
            return undefined;
        } else if (v instanceof ExceptionValue) {
            let resolved: [Value, IdentifierStatus] | undefined = undefined;
            let name: string | undefined = undefined;
            if ((<ValueIdentifier> this.func).name instanceof LongIdentifierToken) {
                let st = state.getAndResolveDynamicStructure(<LongIdentifierToken>
                    (<ValueIdentifier> this.func).name);
                if (st !== undefined) {
                    name = (<LongIdentifierToken> (<ValueIdentifier> this.func).name).id.getText();
                    resolved = st.getValue(name);
                }
            } else {
                name = (<ValueIdentifier> this.func).name.getText();
                resolved = state.getDynamicValue(name);
            }

            if (resolved === undefined) {
                throw new InternalInterpreterError(
                    'How did you match something that does not exist?');
            }
            let exCtor = resolved[0];
            if (this.func instanceof ValueIdentifier
                && exCtor instanceof ExceptionConstructor
                && name === (<ExceptionValue> v).constructorName
                && (<ExceptionConstructor> exCtor).id
                === (<ExceptionValue> v).id
                && (<ExceptionConstructor> exCtor).evalId
                === (<ExceptionValue> v).evalId) {
                if ((<ExceptionValue> v).argument !== undefined) {
                    return (<PatternExpression> this.argument).matches(
                        state, <Value> (<ExceptionValue> v).argument);
                }
                return [];
            }
            return undefined;
        } else if (v instanceof PredefinedFunction) {
            throw new EvaluationError(
                'You simply cannot match predefined functions.');
        } else if (v instanceof ReferenceValue) {
            if (this.func instanceof ValueIdentifier
                && (<ValueIdentifier> this.func).name.getText() === 'ref') {
                return (<PatternExpression> this.argument).matches(
                    state, <Value> state.getCell((<ReferenceValue> v).address));
            }
            return undefined;
        }
        throw new EvaluationError('Help me, I\'m broken. ('
            + v.typeName() + ').' );
    }

    getType(state: any,
            tyVarBnd: Map<string, [Type, boolean]> = new Map<string, [Type, boolean]>(),
            nextName: string = '\'*t0', tyVars: Set<string> = new Set<string>(),
            isPattern: boolean = false,
            paramBindings: Map<string, Type> = new Map<string, Type>())
        : [Type, Warning[], string, Set<string>, Map<string, [Type, boolean]>, IdCnt] {


        let cleanedFunc = this.func;
        while (cleanedFunc instanceof TypedExpression) {
            cleanedFunc = (<TypedExpression> cleanedFunc).expression;
        }

        if (isPattern && cleanedFunc instanceof ValueIdentifier) {
            if ((<ValueIdentifier> cleanedFunc).name instanceof LongIdentifierToken) {
                let st = state.getAndResolveStaticStructure(<LongIdentifierToken>
                    (<ValueIdentifier> cleanedFunc).name);
                if (st === undefined ||
                    st.getValue((<LongIdentifierToken> (<ValueIdentifier> cleanedFunc).name).
                        id.getText()) === undefined) {
                    throw new ElaborationError(
                        '"' + (<ValueIdentifier> cleanedFunc).name.getText()
                        + '" is not a constructor.');
                }
            } else {
                let t = state.getStaticValue((<ValueIdentifier> cleanedFunc).name.getText());
                if (t === undefined || t[1] === IdentifierStatus.VALUE_VARIABLE) {
                    throw new ElaborationError(
                        '"' + (<ValueIdentifier> cleanedFunc).name.getText()
                        + '" is not a constructor.');
                }
            }
        }
        let f = this.func.getType(state, tyVarBnd, nextName, tyVars, isPattern, paramBindings);
        state.valueIdentifierId = f[5];

        // This is a really dirty hack. I am not sorry.
        if (f[0] instanceof FunctionType
            && (<FunctionType> f[0]).returnType + '' === 'exn') {
                // actively add type annotations for exceptions
            this.argument = new TypedExpression(this.argument,
                                                (<FunctionType> f[0]).parameterType);
        }

        let nf4 = new Map<string, [Type, boolean]>();
        f[4].forEach((val: [Type, boolean], key: string) => {
            nf4 = nf4.set(key, val);
        });
        let arg = this.argument.getType(state, nf4, f[2], f[3], isPattern, paramBindings);

        arg[4].forEach((val: [Type, boolean], key: string) => {
            f[4] = f[4].set(key, val);
        });

        f[0] = f[0].instantiate(state, f[4]);

        if (f[0] instanceof TypeVariable) {
            let tva = new TypeVariable((<TypeVariable> f[0]).name + '*a');
            let tvb = new TypeVariable((<TypeVariable> f[0]).name + '*b');
            tva.isFree = (<TypeVariable> f[0]).isFree;
            tvb.isFree = (<TypeVariable> f[0]).isFree;

            let ntype = new FunctionType(tva, tvb);
            f[4] = f[4].set((<TypeVariable> f[0]).name, [ntype, (<TypeVariable> f[0]).isFree]);
            f[0] = ntype;
        }
        if (f[0] instanceof AnyType) {
            f[0] = new FunctionType(new AnyType(), new AnyType());
        }

        if (f[0] instanceof FunctionType) {
            try {
                let tp = (<FunctionType> f[0]).parameterType.merge(state, f[4], arg[0]);

                let restp = (<FunctionType> f[0]).returnType;
                if ((<FunctionType> f[0]).parameterType instanceof RecordType
                    && (!(<RecordType> (<FunctionType> f[0]).parameterType).complete)) {
                    restp = restp.replace((<FunctionType> f[0]).parameterType, tp[0]);
                }

                return [restp.instantiate(state, tp[1]), f[1].concat(arg[1]), arg[2],
                    arg[3], tp[1], arg[5]];
            } catch (e) {
                if (!(e instanceof Array)) {
                    throw e;
                }
                throw new ElaborationError(
                    'Type clash. Functions of type "' + f[0].normalize()[0]
                    + '" cannot take an argument of type "' + arg[0].normalize()[0] + '": ' + e[0]);
            }
        } else {
            throw new ElaborationError('"' + this.func + '" of type "'
                + f[0].normalize()[0] + '" is not a function.');
        }
    }

    simplify(): FunctionApplication {
        return new FunctionApplication(this.func.simplify(), this.argument.simplify());
    }

    toString(): string {
        let res = '( ' +  this.func;
        res += ' ' + this.argument;
        return res + ' )';
    }

    compute(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        let state = params.state;
        let modifiable = params.modifiable;
        if (params.step === undefined) {
            params.step = -1;
        }

        let step: number = params.step;

        if (step === -1) {
            params.step = step + 1;
            params.state = state;
            callStack.push({'next': this, 'params': params});
            callStack.push({
                'next': this.func,
                'params': {'state': state, 'modifiable': modifiable, 'recResult': undefined}
            });
            return;
        }

        if (step === 0) {
            let funcVal = params.recResult;
            if (funcVal === undefined) {
                throw new InternalInterpreterError(
                    'So you see, squids are evil. I will eat them. I will eat them ALL.');
            }
            if (funcVal.hasThrown) {
                // computing the function failed
                return funcVal;
            }

            params.step = step + 1;
            params.state = state;
            params.funcVal = funcVal;
            callStack.push({'next': this, 'params': params});
            callStack.push({
                'next': this.argument,
                'params': {'state': state, 'modifiable': modifiable, 'recResult': undefined}
            });
            return;
        }

        if (step === 1) {
            let funcVal = <EvaluationResult> params.funcVal;
            let argVal = <EvaluationResult> params.recResult;

            if (funcVal === undefined
                || funcVal.value === undefined) {
                throw new InternalInterpreterError(
                    'I\'m fabulous down to my very core, so praise me, human.');
            }
            if (argVal === undefined
                || argVal.value === undefined) {
                throw new InternalInterpreterError('I guess I really am trash, aren\'t I?');
            }

            if (argVal.hasThrown) {
                return {
                    'newState': undefined,
                    'value': argVal.value,
                    'hasThrown': true,
                };
            }
            if (funcVal.value instanceof FunctionValue) {
                (<FunctionValue> funcVal.value).compute(callStack, <Value> argVal.value, modifiable);
                return;
            } else if (funcVal.value instanceof ValueConstructor) {
                return {
                    'newState': undefined,
                    'value': (<ValueConstructor> funcVal.value).construct(argVal.value),
                    'hasThrown': false,
                };
            } else if (funcVal.value instanceof ExceptionConstructor) {
                return {
                    'newState': undefined,
                    'value': (<ExceptionConstructor> funcVal.value).construct(argVal.value),
                    'hasThrown': false,
                };
            } else if (funcVal.value instanceof PredefinedFunction) {
                let res = (<PredefinedFunction> funcVal.value).apply(argVal.value, params);
                for (let warn of res[2]) {
                    modifiable.addWarning(warn);
                }
                return {
                    'newState': undefined,
                    'value': res[0],
                    'hasThrown': res[1],
                };
            }
            throw new EvaluationError('Cannot evaluate the function "'
                + this.func + '" (' + funcVal[0].typeName() + ').');
        }
        // brace so linter does not complain about shadowed names
        {
            let res = <EvaluationResult> params.recResult;
            if (res === undefined) {
                throw new InternalInterpreterError('No! Stop! Hoshimaru, Stop!');
            }

            return {
                'newState': undefined,
                'value': res.value,
                'hasThrown': res.hasThrown,
            };
        }
    }

    assertUniqueBinding(state: any, conn: Set<string>): Set<string> {
        let fuc = this.func.assertUniqueBinding(state, conn);
        let res = this.argument.assertUniqueBinding(state, conn);
        if (this.func instanceof ValueIdentifier) {
            if (fuc.size === 0) { // Constructor
                return res;
            }
        }
        return new Set<string>();
    }
}

export class HandleException extends Expression {
    // expression handle match
    constructor(public expression: Expression, public match: Match) {
        super();
    }

    getExplicitTypeVariables(): Set<TypeVariable> {
        return this.expression.getExplicitTypeVariables();
    }

    isSafe(state: any): boolean {
        return this.expression.isSafe(state);
    }

    simplify(): HandleException {
        return new HandleException(this.expression.simplify(), this.match.simplify());
    }

    toString(): string {
        let res = '( ' + this.expression + ' )';
        res += ' handle ' + this.match;
        return res;
    }

    getType(state: any,
            tyVarBnd: Map<string, [Type, boolean]> = new Map<string, [Type, boolean]>(),
            nextName: string = '\'*t0', tyVars: Set<string> = new Set<string>(),
            isPattern: boolean = false,
            paramBindings: Map<string, Type> = new Map<string, Type>())
        : [Type, Warning[], string, Set<string>, Map<string, [Type, boolean]>, IdCnt] {

        let mtp = this.match.getType(state, tyVarBnd, nextName, tyVars, isPattern,
                                     false, paramBindings);
        // TODO: also check exn handles for exh
        if (!(mtp[0] instanceof FunctionType)) {
            throw new ElaborationError(
                'You can only handle things of type "exn" and not "'
                + (<FunctionType> mtp[0]).parameterType.normalize()[0] + '".');
        }
        try {
            (<FunctionType> mtp[0]).parameterType.merge(state, mtp[4], new CustomType('exn'));
        } catch (e) {
            throw new ElaborationError(
                'You can only handle things of type "exn" and not "'
                + (<FunctionType> mtp[0]).parameterType.normalize()[0] + '".');
        }
        let rt = (<FunctionType> mtp[0]).returnType;
        state.valueIdentifierId = mtp[5];
        let etp = this.expression.getType(state, mtp[4], mtp[2], mtp[3], isPattern,
                                         paramBindings);

        try {
            let res = rt.merge(state, etp[4], etp[0]);
            return [res[0], mtp[1].concat(etp[1]), etp[2], etp[3], res[1], etp[5]];
        } catch (e) {
            if (!(e instanceof Array)) {
                throw e;
            }
            throw new ElaborationError(
                'The "handle" cannot change the type of the expression from "'
                + etp[0].normalize()[0] + '" to "' + rt.normalize()[0] + '": ' + e[0]);
        }
    }

    compute(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        let state = params.state;
        if (params.recResult === undefined) {
            callStack.push({'next': this, 'params': params});
            callStack.push({
                'next': this.expression,
                'params': {'state': state, 'modifiable': params.modifiable, 'recResult': undefined}
            });
            return;
        }
        let res = params.recResult;
        if (res === undefined
            || res.value === undefined) {
            throw new InternalInterpreterError(
                'Try removing this jumper and that one over there.');
        }
        if (params.exprResult === undefined) {
            if (res.hasThrown) {

                params.exprResult = res;
                callStack.push({'next': this, 'params': params});
                callStack.push({
                    'next': this.match,
                    'params': {
                        'state': state,
                        'modifiable': params.modifiable,
                        'recResult': undefined,
                        'value': res.value
                    }
                });
                return;
            }
            return res;
        }
        res = params.exprResult;
        let next = params.recResult;
        if (res === undefined
            || res.value === undefined
            || next === undefined
            || next.value === undefined) {
            throw new InternalInterpreterError('Error. I didn\'t quite catch that.');
        }
        if (!next.hasThrown || !(<Value> next.value).equals(new ExceptionValue('Match', undefined, 0, 0))) {
            // Exception got handled
            return {
                'newState': undefined,
                'value': next.value,
                'hasThrown': next.hasThrown,
            };
        }
        return {
            'newState': undefined,
            'value': res.value,
            'hasThrown': res.hasThrown,
        };
    }

    assertUniqueBinding(state: any, conn: Set<string>): Set<string> {
        this.expression.assertUniqueBinding(state, conn);
        this.match.assertUniqueBinding(state, conn);
        return new Set<string>();
    }
}

export class RaiseException extends Expression {
    // raise expression
    constructor(public expression: Expression) { super(); }

    getExplicitTypeVariables(): Set<TypeVariable> {
        return this.expression.getExplicitTypeVariables();
    }

    simplify(): RaiseException {
        return new RaiseException(this.expression.simplify());
    }

    isSafe(state: any): boolean {
        return false;
    }

    toString(): string {
        return 'raise ' + this.expression;
    }

    getType(state: any,
            tyVarBnd: Map<string, [Type, boolean]> = new Map<string, [Type, boolean]>(),
            nextName: string = '\'*t0', tyVars: Set<string> = new Set<string>(),
            isPattern: boolean = false,
            paramBindings: Map<string, Type> = new Map<string, Type>())
        : [Type, Warning[], string, Set<string>, Map<string, [Type, boolean]>, IdCnt] {

        let res = this.expression.getType(state, tyVarBnd, nextName, tyVars,
                                          isPattern, paramBindings);
        try {
            let mg = res[0].merge(state, res[4], new CustomType('exn'));
            // TODO This is a really sloppy way to set a new "nextName"
            return [new TypeVariable(res[2]), res[1], res[2] + '0', res[3], mg[1], res[5]];
        } catch (e) {
            if (!(e instanceof Array)) {
                throw e;
            }
            throw new ElaborationError(
                'Raising anything but exceptions will only raise exceptions: ' + e[0]);
        }
    }

    compute(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        let state = params.state;
        if (params.recResult === undefined) {
            callStack.push({'next': this, 'params': params});
            callStack.push({
                'next': this.expression,
                'params': {'state': state, 'modifiable': params.modifiable, 'recResult': undefined}
            });
            return;
        }

        let res = params.recResult;
        if (res === undefined
            || res.value === undefined) {
            throw new InternalInterpreterError(
                'I know I only killed four squids this month...but last month I killed at least ten!');
        }
        if (!(res.value instanceof ExceptionValue)) {
            throw new EvaluationError(
                'Cannot "raise" value of type "' + (<Value> res.value).typeName()
                + '" (type must be "exn").');
        }
        return {
            'newState': state,
            'value': res.value,
            'hasThrown': true,
        };
    }

    assertUniqueBinding(state: any, conn: Set<string>): Set<string> {
        this.expression.assertUniqueBinding(state, conn);
        return new Set<string>();
    }
}

export class Lambda extends Expression {
    // fn match
    constructor(public match: Match) { super(); }

    getExplicitTypeVariables(): Set<TypeVariable> {
        return this.match.getExplicitTypeVariables();
    }

    simplify(): Lambda {
        return new Lambda(this.match.simplify());
    }

    toString(): string {
        return '(fn ' + this.match + ')';
    }

    getType(state: any,
            tyVarBnd: Map<string, [Type, boolean]> = new Map<string, [Type, boolean]>(),
            nextName: string = '\'*t0', tyVars: Set<string> = new Set<string>(),
            isPattern: boolean = false,
            paramBindings: Map<string, Type> = new Map<string, Type>())
        : [Type, Warning[], string, Set<string>, Map<string, [Type, boolean]>, IdCnt] {
        return this.match.getType(state, tyVarBnd, nextName, tyVars, isPattern, true,
                                  paramBindings);
    }

    compute(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        let state = params.state;
        // We need to ensure that the function value receives a capture
        // of the current state, and that that capture stays that way
        // (Local declarations may change the past, so we must record that, too.
        let nstate = state.getNestedState(state.id);

        if (nstate.insideLocalDeclBody) {
            nstate.dynamicBasis = state.getDynamicLocalDeclChanges(-1);
            nstate.insideLocalDeclBody = false;
        }

        return {
            'newState': undefined,
            'value': new FunctionValue(nstate, [], this.match),
            'hasThrown': false,
        };
    }

    assertUniqueBinding(state: any, conn: Set<string>): Set<string> {
        this.match.assertUniqueBinding(state, conn);
        return new Set<string>();
    }
}

// Matches

// Pure Patterns

export class Wildcard extends Expression implements Pattern {
    constructor() { super(); }

    getType(state: any,
            tyVarBnd: Map<string, [Type, boolean]> = new Map<string, [Type, boolean]>(),
            nextName: string = '\'*t0', tyVars: Set<string> = new Set<string>(),
            isPattern: boolean = false,
            paramBindings: Map<string, Type> = new Map<string, Type>())
        : [Type, Warning[], string, Set<string>, Map<string, [Type, boolean]>, IdCnt] {
        let cur = (+nextName.substring(3)) + 1;
        let nm = '';
        for (; ; ++cur) {
            nextName = '\'' + nextName[1] + nextName[2] + cur;
            if (!tyVars.has(nextName) && !tyVarBnd.has(nextName)
                && state.getStaticValue(nextName) === undefined) {
                nm = nextName;
                return [new TypeVariable(nm), [], nm, tyVars.add(nm), tyVarBnd, state.valueIdentifierId];
            }
        }

    }

    compute(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        throw new InternalInterpreterError(
            'Wildcards are far too wild to have a value.');
    }

    matchType(state: any, tyVarBnd: Map<string, [Type, boolean]>, t: Type):
        [[string, Type][], Type, Map<string, [Type, boolean]>] {
        return [[], t, tyVarBnd];
    }

    subsumes(state: any, other: PatternExpression): boolean {
        return true;
    }

    cover(state: any, rules: PatternExpression[]): Warning[] {
        let isExh = false;
        let hasWildcard = false;
        let warns: Warning[] = [];
        let seenRules = new Set<string>();

        let cons: string[] = [];
        let splitRules = new Map<string, PatternExpression[]>();
        let idealPts = new Map<string, PatternExpression>();

        let recordVals = new Set<string>();
        let recRules: PatternExpression[] = [];

        for (let i = 0; i < rules.length; ++i) {
            let currentRule = rules[i];

            while (currentRule instanceof TypedExpression) {
                currentRule = <PatternExpression> (<TypedExpression> currentRule).expression;
            }

            if (seenRules.has(currentRule.toString())) {
                warns.push(new Warning(0, 'Duplicate rule for "' + currentRule
                                       + '" in pattern matching.\n'));
            }
            seenRules.add(currentRule.toString());

            if (currentRule instanceof Record) {
                let rec = <Record> currentRule;

                for (let n of rec.entries) {
                    recordVals.add(n[0]);
                }
                recRules.push(currentRule);
                continue;
            }

            if (currentRule instanceof Constant) {
                // We assume that listing all different possible values for primitive types
                // is never exhaustive, so just ignore constant values.
                continue;
            }

            if (currentRule instanceof Wildcard) {
                isExh = true;
                hasWildcard = true;
                if (i + 1 < rules.length) {
                    warns.push(new Warning(i, 'Rules after "' + currentRule
                                           + '" unused in pattern matching.\n'));
                }
                break;
            }

            if (currentRule instanceof ValueIdentifier) {
                let cnm = (<ValueIdentifier> currentRule).getConstructorName(state);
                if (cnm === undefined) {
                    isExh = true;
                    hasWildcard = true;
                    if (i + 1 < rules.length) {
                        warns.push(new Warning(i, 'Rules after "' + currentRule
                                               + '" unused in pattern matching.\n'));
                    }
                    break;
                }

                // Constructor w/o argument, needs splitting
                if (cons.length === 0) {
                    let stattp = (<ValueIdentifier> currentRule).getConstructorList(state);
                    if (stattp === undefined) {
                        throw new InternalInterpreterError(
                            'There. Just another unfortunate accident.');
                    }
                    cons = stattp;
                }
                if (splitRules.has(cnm)) {
                    warns.push(new Warning(0, 'Duplicate rule for "' + currentRule
                                           + '" in pattern matching.\n'));
                } else {
                    splitRules = splitRules.set(cnm, [currentRule]);
                    idealPts = idealPts.set(cnm, currentRule);
                }
                continue;
            }

            if (currentRule instanceof FunctionApplication) {
                // Constructor w/ argument, needs splitting
                let fnname = (<FunctionApplication> currentRule).func;
                if (!(fnname instanceof ValueIdentifier)) {
                    throw new InternalInterpreterError(
                        'There. Just another unfortunate accident.');
                }
                let cnm = (<ValueIdentifier> fnname).getConstructorName(state);
                if (cnm === undefined) {
                    throw new InternalInterpreterError(
                        'There. Just another unfortunate accident.');
                }

                if (cons.length === 0) {
                    let stattp = (<ValueIdentifier> fnname).getConstructorList(state);
                    if (stattp === undefined) {
                        throw new InternalInterpreterError(
                            'There. Just another unfortunate accident.');
                    }
                    cons = stattp;
                }

                if (splitRules.has(cnm)) {
                    let prevrls = <PatternExpression[]> splitRules.get(cnm);
                    prevrls.push(currentRule);
                    splitRules = splitRules.set(cnm, prevrls);
                } else {
                    splitRules = splitRules.set(cnm, [currentRule]);
                    idealPts = idealPts.set(cnm, new FunctionApplication(
                        (<FunctionApplication> currentRule).func,
                        new Wildcard()
                    ));
                }
            }

            if (currentRule instanceof Vector) {
                throw new PatternMatchError('Vector pattern are not checked for exhaustiveness.');
            }
        }

        if (cons.length !== 0) {
            if (!hasWildcard) { // If pattern has a wildcard already, we don't care
                // Actual splitting work is required
                isExh = true;
                for (let i = 0; i < cons.length; ++i) {
                    if (!idealPts.has(cons[i])) {
                        isExh = hasWildcard;
                        continue;
                    }
                    warns = warns.concat((<PatternExpression> idealPts.get(cons[i])).cover(
                        state, <PatternExpression[]> splitRules.get(cons[i])));
                }
            }
        } else if (!isExh && recRules.length !== 0) {
            // Actually do record stuff
            let reclbl: [string, Expression|PatternExpression][] = [];
            recordVals.forEach((val: string) => {
                reclbl.push([val, new Wildcard()]);
            });
            return warns.concat(new Record(true, reclbl).cover(state, recRules));
        }

        if (!isExh) {
            warns.push(new Warning(-1, 'Pattern matching is not exhaustive.\n'));
        }
        return warns;
    }

    matches(state: any, v: Value): [string, Value][] | undefined {
        return [];
    }

    simplify(): Wildcard {
        return this;
    }

    toString(): string {
        return '_';
    }

    assertUniqueBinding(state: any, conn: Set<string>): Set<string> {
        return new Set<string>();
    }
}

export class LayeredPattern extends Expression implements Pattern {
    // <op> identifier <:type> as pattern
    constructor(public identifier: IdentifierToken,
                public typeAnnotation: Type | undefined,
                public pattern: Expression|PatternExpression) { super(); }

    getType(state: any,
            tyVarBnd: Map<string, [Type, boolean]> = new Map<string, [Type, boolean]>(),
            nextName: string = '\'*t0', tyVars: Set<string> = new Set<string>(),
            isPattern: boolean = false,
            paramBindings: Map<string, Type> = new Map<string, Type>())
        : [Type, Warning[], string, Set<string>, Map<string, [Type, boolean]>, IdCnt] {
        if (!isPattern) {
            throw new InternalInterpreterError(
                'Layered patterns are far too layered to have a type.');
        }

        let valid = new ValueIdentifier(this.identifier);
        let gtp = valid.getType(state, tyVarBnd, nextName, tyVars, true, paramBindings);
        let tp = gtp[0];

        if (this.typeAnnotation !== undefined) {
            try {
                let mg = tp.merge(state, gtp[4], <Type> this.typeAnnotation);
                tyVarBnd = mg[1];
                tp = mg[0];
            } catch (e) {
                if (!(e instanceof Array)) {
                    throw e;
                }
                throw new ElaborationError('Wrong type annotation: ' + e[0]);
            }
        }

        let argtp = this.pattern.getType(state, tyVarBnd, gtp[2], gtp[3], true, paramBindings);

        try {
            let mg = tp.merge(state, argtp[4], argtp[0]);
            tyVarBnd = mg[1];
            tp = mg[0];
        } catch (e) {
            if (!(e instanceof Array)) {
                throw e;
            }
            throw new ElaborationError('Wrong type annotation: ' + e[0]);
        }

        return [tp, gtp[1].concat(argtp[1]), argtp[2], argtp[3], tyVarBnd, state.valueIdentifierId];
    }

    compute(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        throw new InternalInterpreterError(
            'Layered patterns are far too layered to have a value.');
    }

    matchType(state: any, tyVarBnd: Map<string, [Type, boolean]>, t: Type):
        [[string, Type][], Type, Map<string, [Type, boolean]>]  {

        let tp = t;
        if (this.typeAnnotation !== undefined) {
            try {
                let mg = t.merge(state, tyVarBnd, <Type> this.typeAnnotation);
                tyVarBnd = mg[1];
                tp = mg[0];
            } catch (e) {
                if (!(e instanceof Array)) {
                    throw e;
                }
                throw new ElaborationError('Wrong type annotation: ' + e[0]);
            }
        }
        let res = (<PatternExpression> this.pattern).matchType(state, tyVarBnd, tp);
        let result: [string, Type][] = [[this.identifier.getText(), tp]];
        return [result.concat(res[0]), t, res[2]];
    }

    subsumes(state: any, other: PatternExpression): boolean {
        throw new PatternMatchError('Layered patterns are not checked for exhaustiveness.');
    }

    cover(state: any, rules: PatternExpression[]): Warning[] {
        throw new PatternMatchError('Layered patterns are not checked for exhaustiveness.');
    }

    matches(state: any, v: Value): [string, Value][] | undefined {
        let res = (<PatternExpression> this.pattern).matches(state, v);
        if (res === undefined) {
            return res;
        }
        let result: [string, Value][] = [[this.identifier.getText(), v]];
        return result.concat(res);
    }

    simplify(): LayeredPattern {
        if (this.typeAnnotation) {
            return new LayeredPattern(this.identifier, this.typeAnnotation.simplify(),
                this.pattern.simplify());
        } else {
            return new LayeredPattern(this.identifier, undefined, this.pattern.simplify());
        }
    }

    toString(): string {
        return this.identifier.getText() + ' as ' + this.pattern;
    }

    assertUniqueBinding(state: any, conn: Set<string>): Set<string> {
        let res = this.pattern.assertUniqueBinding(state, conn);

        let stt = state.getStaticValue(this.identifier.getText());
        if (stt !== undefined && stt[1] !== IdentifierStatus.VALUE_VARIABLE) {
            return res;
        }
        let dyt = state.getDynamicValue(this.identifier.getText());
        if (dyt !== undefined && dyt[1] !== IdentifierStatus.VALUE_VARIABLE) {
            return res;
        }

        if (res.has(this.identifier.getText())) {
            throw new ParserError('No matter from which end you start eating this pattern,'
                + 'rebinding "' + this.identifier.getText() + '" is still a bad idea.');
        }
        return res.add(this.identifier.getText());
    }
}

// Successor ML
export class Vector extends Expression implements Pattern {
    // #[exp1, ..., expn]
    constructor(public expressions: Expression[]) { super(); }

    getType(state: any,
            tyVarBnd: Map<string, [Type, boolean]> = new Map<string, [Type, boolean]>(),
            nextName: string            = '\'*t0',
            tyVars: Set<string>         = new Set<string>(),
            isPattern: boolean       = false,
            paramBindings: Map<string, Type> = new Map<string, Type>())
        : [Type, Warning[], string, Set<string>, Map<string, [Type, boolean]>, IdCnt] {

        if (this.expressions.length === 0) {
            // TODO
            return [new CustomType('vector', [new AnyType()]), [], nextName, tyVars, tyVarBnd, state.valueIdentifierId];
        }

        // TODO Do this properly

        let tmp = this.expressions[0].getType(state, tyVarBnd, nextName, tyVars, isPattern);
        let restp = tmp[0];

        for (let i = 0; i < this.expressions.length; ++i) {
            tmp = this.expressions[i].getType(state, tmp[4], tmp[2], tmp[3], isPattern);
            try {
                let mt = restp.merge(state, tmp[4], tmp[0]);
                tmp[4] = mt[1];
                restp = mt[0];
            } catch (e) {
                if (!(e instanceof Array)) {
                    throw e;
                }
                throw new ElaborationError(
                    'Type clash in vector entries: ' + e[0]);
            }
        }

        return [new CustomType('vector', [restp]), tmp[1], tmp[2], tmp[3],
            tmp[4], tmp[5]];
    }

    // Computes the value of an expression, returns [computed value, is thrown exception]
    compute(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        let res: Value[] = [];

        for (let i = 0; i < this.expressions.length; ++i) {
            let tmp: EvaluationResult = this.expressions[i].compute(params, callStack);
            if (tmp === undefined || tmp.value === undefined) {
                throw new InternalInterpreterError('NG');
            }
            res.push(tmp.value);
        }

        // TODO: do this properly

        return {
            'newState': undefined,
            'value': new VectorValue(res),
            'hasThrown': false,
        };
    }

    getExplicitTypeVariables(): Set<TypeVariable> {
        // TODO
        return new Set<TypeVariable>();
    }


    matchType(state: any, tyVarBnd: Map<string, [Type, boolean]>, t: Type):
        [[string, Type][], Type, Map<string, [Type, boolean]>] {
        if (!(t instanceof CustomType) || (<CustomType> t).name !== 'vector') {
            throw new ElaborationError(
                'Vectors like only vectors, not "' + t + '". Stay cool.');
        }

        let partp = (<CustomType> t).typeArguments[0];
        let res: [string, Type][] = [];

        for (let i = 0; i < this.expressions.length; ++i) {
            let tmp = (<PatternExpression> this.expressions[i]).matchType(state, tyVarBnd, partp);
            partp = tmp[1];
            res = res.concat(tmp[0]);
            tyVarBnd = tmp[2];
        }

        return [res, new CustomType('vector', [partp]), tyVarBnd];
    }

    subsumes(state: any, other: PatternExpression): boolean {
        throw new PatternMatchError('Vector patterns are not checked for exhaustiveness.');
    }

    cover(state: any, rules: PatternExpression[]): Warning[] {
        throw new PatternMatchError('Vector patterns are not checked for exhaustiveness.');
    }

    matches(state: any, v: Value): [string, Value][] | undefined {
        if (!(v instanceof VectorValue)) {
            throw new ElaborationError(
                'Vectors like only vectors, not "' + v + '". Stay cool.');
        }

        let res: [string, Value][] = [];

        // TODO do this properly

        for (let i = 0; i < this.expressions.length; ++i) {
            let tmp = (<PatternExpression> this.expressions[i]).matches(state, (<VectorValue> v).entries[i]);
            if (tmp !== undefined) {
                res = res.concat(tmp);
            }
        }

        return res;
    }

    simplify(): PatternExpression {
        let res: Expression[] = [];
        for (let i = 0; i < this.expressions.length; ++i) {
            res.push(this.expressions[i].simplify());
        }
        return new Vector(res);
    }

    toString(): string {
        let res = '#[ ';
        for (let i = 0; i < this.expressions.length; ++i) {
            if (i > 0) {
                res += ', ';
            }
            res += this.expressions[i];
        }
        return res + ' ]';
    }
}




export class ConjunctivePattern extends Expression implements Pattern {
// pat1 as pat2
    constructor(public left: PatternExpression,
                public right: PatternExpression) {
        super();
    }

    getType(state: any,
            tyVarBnd: Map<string, [Type, boolean]> = new Map<string, [Type, boolean]>(),
            nextName: string = '\'*t0', tyVars: Set<string> = new Set<string>(),
            isPattern: boolean = false,
            paramBindings: Map<string, Type> = new Map<string, Type>())
        : [Type, Warning[], string, Set<string>, Map<string, [Type, boolean]>, IdCnt] {
        let r1 = this.left.getType(state, tyVarBnd, nextName, tyVars, isPattern);
        let r2 = this.right.getType(state, r1[4], r1[2], r1[3], isPattern);

        try {
            let res = r1[0].merge(state, r2[4], r2[0]);
            return [res[0], r1[1].concat(r2[1]), r2[2], r2[3], res[1], r2[5]];
        } catch (e) {
            if (!(e instanceof Array)) {
                throw e;
            }
            throw new ElaborationError(
                'Both patterns of a conjunctive pattern must have the same type.');
        }
    }

    simplify(): ConjunctivePattern {
        return new ConjunctivePattern(this.left.simplify(), this.right.simplify());
    }

    matchType(state: any, tyVarBnd: Map<string, [Type, boolean]>, t: Type):
        [[string, Type][], Type, Map<string, [Type, boolean]>]  {
        // TODO
        throw new InternalInterpreterError('「ニャ－、ニャ－」');
    }

    subsumes(state: any, other: PatternExpression): boolean {
        throw new PatternMatchError('Conjuctive patterns are not checked for exhaustiveness.');
    }

    cover(state: any, rules: PatternExpression[]): Warning[] {
        throw new PatternMatchError(
            'Conjuctive patterns are not checked for exhaustiveness.');
    }

    matches(state: any, v: Value): [string, Value][] | undefined {
        let r1 = this.left.matches(state, v);

        if (r1 === undefined) {
            return undefined;
        }

        let nstate = state.getNestedState(state.id);
        for (let i of r1) {
            nstate.setDynamicValue(i[0], i[1], IdentifierStatus.VALUE_VARIABLE);
        }

        let r2 = this.right.matches(nstate, v);

        if (r2 === undefined) {
            return undefined;
        }

        return (<[string, Value][]> r1).concat(r2);
    }

    assertUniqueBinding(state: any, conn: Set<string>): Set<string> {
        // TODO
        return new Set<string>();
    }
}

export class DisjunctivePattern extends Expression implements Pattern {
// pat1 | pat2
    constructor(public left: PatternExpression,
                public right: PatternExpression) {
        super();
    }

    getType(state: any,
            tyVarBnd: Map<string, [Type, boolean]> = new Map<string, [Type, boolean]>(),
            nextName: string = '\'*t0', tyVars: Set<string> = new Set<string>(),
            isPattern: boolean = false,
            paramBindings: Map<string, Type> = new Map<string, Type>())
        : [Type, Warning[], string, Set<string>, Map<string, [Type, boolean]>, IdCnt] {
        let r1 = this.left.getType(state, tyVarBnd, nextName, tyVars, isPattern);
        let r2 = this.right.getType(state, r1[4], r1[2], r1[3], isPattern);

        try {
            let res = r1[0].merge(state, r2[4], r2[0]);
            return [res[0], r1[1].concat(r2[1]), r2[2], r2[3], res[1], r2[5]];
        } catch (e) {
            if (!(e instanceof Array)) {
                throw e;
            }
            throw new ElaborationError(
                'Both patterns of a disjunctive pattern must have the same type.');
        }
    }

    simplify(): DisjunctivePattern {
        return new DisjunctivePattern(this.left.simplify(), this.right.simplify());
    }

    matchType(state: any, tyVarBnd: Map<string, [Type, boolean]>, t: Type):
        [[string, Type][], Type, Map<string, [Type, boolean]>]  {
        // TODO
        throw new InternalInterpreterError('「ニャ－、ニャ－」');
    }

    subsumes(state: any, other: PatternExpression): boolean {
        throw new PatternMatchError('Disjunctive patterns are not checked for exhaustiveness.');
    }

    cover(state: any, rules: PatternExpression[]): Warning[] {
        throw new PatternMatchError(
            'Disjunctive patterns are not checked for exhaustiveness.');
    }

    matches(state: any, v: Value): [string, Value][] | undefined {
        let r1 = this.left.matches(state, v);
        if (r1 !== undefined) {
            return r1;
        }
        return this.right.matches(state, v);
    }

    assertUniqueBinding(state: any, conn: Set<string>): Set<string> {
        // TODO
        return new Set<string>();
    }
}

export class NestedMatch extends Expression implements Pattern {
// pat1 with pat2 = exp
    constructor(public pattern: PatternExpression,
                public nested: PatternExpression, public expression: Expression) {
        super();
    }

    getType(state: any,
            tyVarBnd: Map<string, [Type, boolean]> = new Map<string, [Type, boolean]>(),
            nextName: string = '\'*t0', tyVars: Set<string> = new Set<string>(),
            isPattern: boolean = false,
            paramBindings: Map<string, Type> = new Map<string, Type>())
        : [Type, Warning[], string, Set<string>, Map<string, [Type, boolean]>, IdCnt] {

        // TODO
        throw new InternalInterpreterError('「ニャ－、ニャ－」');
    }

    simplify(): NestedMatch {
        return new NestedMatch(this.pattern.simplify(),
            this.nested.simplify(), this.expression.simplify());
    }

    matchType(state: any, tyVarBnd: Map<string, [Type, boolean]>, t: Type):
        [[string, Type][], Type, Map<string, [Type, boolean]>]  {
        // TODO
        throw new InternalInterpreterError('「ニャ－、ニャ－」');
    }

    subsumes(state: any, other: PatternExpression): boolean {
        throw new PatternMatchError('Nested matches are not checked for exhaustiveness.');
    }

    cover(state: any, rules: PatternExpression[]): Warning[] {
        throw new PatternMatchError(
            'Nested matches are not checked for exhaustiveness.');
    }

    matches(state: any, v: Value): [string, Value][] | undefined {
        let r1 = this.pattern.matches(state, v);
        if (r1 === undefined) {
            return undefined;
        }

        // TODO
        throw new InternalInterpreterError('「ニャ－、ニャ－」');
    }

    assertUniqueBinding(state: any, conn: Set<string>): Set<string> {
        // TODO
        return new Set<string>();
    }

    toString(): string {
        return '( ' + this.pattern + ' with ' + this.nested + ' = ' + this.expression + ' )';
    }
}

// The following classes are derived forms.
// They will not be present in the simplified AST and do not implement elaborate/getType

export class InfixExpression extends Expression implements Pattern {
    // operators: (op, idx), to simplify simplify
    constructor(public expressions: Expression[], public operators: [IdentifierToken, number][]) {
        super();
    }

    matchType(state: any, tyVarBnd: Map<string, [Type, boolean]>, t: Type):
        [[string, Type][], Type, Map<string, [Type, boolean]>] {
        return this.reParse(state).matchType(state, tyVarBnd, t);
    }

    subsumes(state: any, other: PatternExpression): boolean {
        throw new InternalInterpreterError('一昨日来やがれ。');
    }

    cover(state: any, rules: PatternExpression[]): Warning[] {
        throw new InternalInterpreterError('一昨日来やがれ。');
    }

    matches(state: any, v: Value): [string, Value][] | undefined {
        return this.reParse(state).matches(state, v);
    }

    simplify(): FunctionApplication {
        throw new InternalInterpreterError('Ouch, I\'m not fully parsed.');
    }

    reParse(state: any): FunctionApplication {
        let ops = this.operators;
        let exps = this.expressions;
        let poses: number[][] = [];
        for (let i = 0; i < exps.length; ++i) {
            poses.push([i]);
        }
        ops.sort(([a, p1], [b, p2]) => {
            let sta = state.getInfixStatus(a);
            let stb = state.getInfixStatus(b);
            if (sta.precedence > stb.precedence) {
                return -1;
            }
            if (sta.precedence < stb.precedence) {
                return 1;
            }
            if (sta.rightAssociative) {
                if (p1 > p2) {
                    return -1;
                }
                if (p1 < p2) {
                    return 1;
                }
            } else {
                if (p1 > p2) {
                    return 1;
                }
                if (p1 < p2) {
                    return -1;
                }
            }
            return 0;
        });

        // Using copy by reference to make this work whithout shrinking the array
        for (let i = 0; i < ops.length; ++i) {
            if (i > 0) {
                let info1 = state.getInfixStatus(ops[i][0]);
                let info2 = state.getInfixStatus(ops[i - 1][0]);

                if (info1.precedence === info2.precedence
                    && info1.rightAssociative !== info2.rightAssociative
                    && (poses[ops[i - 1][1] + 1] === poses[ops[i][1]]
                        || poses[ops[i - 1][1]] === poses[ops[i][1] + 1])) {
                    throw new ParserError('Could you ever imagine left associatives '
                        + 'and right associatives living together in peace?');
                }
            }

            let left = exps[ops[i][1]];
            let right = exps[ops[i][1] + 1];
            let com = new FunctionApplication(new ValueIdentifier(ops[i][0]),
                                              new Tuple([left, right]));
            let npos = poses[ops[i][1]];
            for (let j of poses[ops[i][1] + 1]) {
                npos.push(j);
            }
            for (let j of npos) {
                poses[j] = npos;
                exps[j] = com;
            }
        }
        return <FunctionApplication> exps[0];
    }
}

let falseConstant = new ValueIdentifier(new IdentifierToken('false'));
let trueConstant = new ValueIdentifier(new IdentifierToken('true'));
let nilConstant = new ValueIdentifier(new IdentifierToken('nil'));
let consConstant = new ValueIdentifier(new IdentifierToken('::'));

export class Conjunction extends Expression {
    // leftOperand andalso rightOperand
    constructor(public leftOperand: Expression, public rightOperand: Expression) { super(); }

    simplify(): FunctionApplication {
        return new Conditional(this.leftOperand, this.rightOperand,
            falseConstant).simplify();
    }

    toString(): string {
        return '( ' + this.leftOperand + ' andalso '
            + this.rightOperand + ' )';
    }
}

export class Disjunction extends Expression {
    // leftOperand orelse rightOperand
    constructor(public leftOperand: Expression, public rightOperand: Expression) { super(); }

    simplify(): FunctionApplication {
        return new Conditional(this.leftOperand, trueConstant, this.rightOperand).simplify();
    }

    toString(): string {
        return '( ' + this.leftOperand + ' orelse '
            + this.rightOperand + ' )';
    }
}

export class Tuple extends Expression implements Pattern {
    // (exp1, ..., expn), n > 1
    constructor(public expressions: Expression[]) { super(); }

    matchType(state: any, tyVarBnd: Map<string, [Type, boolean]>, t: Type):
        [[string, Type][], Type, Map<string, [Type, boolean]>] {
        return this.simplify().matchType(state, tyVarBnd, t);
    }

    subsumes(state: any, other: PatternExpression): boolean {
        return this.simplify().subsumes(state, other);
    }

    cover(state: any, rules: PatternExpression[]): Warning[] {
        return this.simplify().cover(state, rules);
    }

    matches(state: any, v: Value): [string, Value][] | undefined {
        return this.simplify().matches(state, v);
    }

    simplify(): Record {
        let entries: [string, Expression][] = [];
        for (let i = 0; i < this.expressions.length; ++i) {
            entries.push(['' + (i + 1), this.expressions[i].simplify()]);
        }
        return new Record(true, entries);
    }

    toString(): string {
        let res = '( ';
        for (let i = 0; i < this.expressions.length; ++i) {
            if (i > 0) {
                res += ', ';
            }
            res += this.expressions[i];
        }
        return res + ' )';
    }
}

export class List extends Expression implements Pattern {
    // [exp1, ..., expn]
    constructor(public expressions: Expression[]) { super(); }

    matchType(state: any, tyVarBnd: Map<string, [Type, boolean]>, t: Type):
        [[string, Type][], Type, Map<string, [Type, boolean]>] {
        return this.simplify().matchType(state, tyVarBnd, t);
    }

    subsumes(state: any, other: PatternExpression): boolean {
        return this.simplify().subsumes(state, other);
    }

    cover(state: any, rules: PatternExpression[]): Warning[] {
        return this.simplify().cover(state, rules);
    }

    matches(state: any, v: Value): [string, Value][] | undefined {
        return this.simplify().matches(state, v);
    }

    simplify(): PatternExpression {
        let res: PatternExpression = nilConstant;
        for (let i = this.expressions.length - 1; i >= 0; --i) {
            let pair = new Tuple([this.expressions[i], res]).simplify();
            res = new FunctionApplication(consConstant, pair);
        }
        return res;
    }

    toString(): string {
        let res = '[ ';
        for (let i = 0; i < this.expressions.length; ++i) {
            if (i > 0) {
                res += ', ';
            }
            res += this.expressions[i];
        }
        return res + ' ]';
    }
}

export class Sequence extends Expression {
    // (exp1; ...; expn), n >= 2
    constructor(public expressions: Expression[]) { super(); }

    simplify(): FunctionApplication {
        let pos = this.expressions.length - 1;
        let match = new Match([[new Wildcard(), this.expressions[pos]]]);
        let res = new CaseAnalysis(this.expressions[pos - 1], match);
        for (let i = pos - 2; i >= 0; --i) {
            match = new Match([[new Wildcard(), res]]);
            res = new CaseAnalysis(this.expressions[i], match);
        }
        return res.simplify();
    }

    toString(): string {
        let res = '( ';
        for (let i = 0; i < this.expressions.length; ++i) {
            if (i > 0) {
                res += '; ';
            }
            res += this.expressions[i];
        }
        return res + ' )';
    }
}

export class RecordSelector extends Expression {
    // #label record
    constructor(public label: IdentifierToken | NumericToken) { super(); }

    simplify(): Lambda {
        return new Lambda(new Match([[
            new Record(false, [[this.label.text, new ValueIdentifier(new IdentifierToken('__rs'))]]),
            new ValueIdentifier(new IdentifierToken('__rs'))]]));
    }

    toString(): string {
        return '#' + this.label.getText();
    }
}

export class CaseAnalysis extends Expression {
    // case expression of match
    constructor(public expression: Expression, public match: Match) { super(); }

    simplify(): FunctionApplication {
        return new FunctionApplication(new Lambda(
            this.match.simplify()),
            this.expression.simplify());
    }

    toString(): string {
        return 'case ' + this.expression + ' of ' + this.match;
    }
}

export class Conditional extends Expression {
    // if condition then consequence else alternative
    constructor(public condition: Expression, public consequence: Expression,
                public alternative: Expression) { super(); }

    simplify(): FunctionApplication {
        let match: Match = new Match([[trueConstant, this.consequence],
            [falseConstant, this.alternative]]);
        return new CaseAnalysis(this.condition, match).simplify();
    }

    toString(): string {
        return 'if ' + this.condition + ' then ' + this.consequence + ' else ' + this.alternative;
    }
}

export class While extends Expression {
    // while exp do exp
    constructor(public condition: Expression,
                public body: Expression) {
        super();
    }

    simplify(): Expression {
        let nm = new ValueIdentifier(new IdentifierToken('__whl'));
        let fapp = new FunctionApplication(nm, new Tuple([]));
        let cond = new Conditional(this.condition,
            new Sequence([this.body, fapp]), new Tuple([]));
        let valbnd = new ValueBinding(true, nm,
            new Lambda(new Match(
                [[new Tuple([]), cond]])));
        let dec = new ValueDeclaration([], [valbnd]);

        return new LocalDeclarationExpression(dec, fapp).simplify();
    }

    toString(): string {
        return '( while ' + this.condition + ' do ' + this.body + ' )';
    }
}

// Successor ML
export class PatternGuard extends Expression implements Pattern {
// pat if exp
    constructor(public pattern: PatternExpression,
                public condition: Expression) {
        super();
    }

    simplify(): NestedMatch {
        return new NestedMatch(this.pattern.simplify(),
            trueConstant, this.condition.simplify());
    }

    matchType(state: any, tyVarBnd: Map<string, [Type, boolean]>, t: Type):
        [[string, Type][], Type, Map<string, [Type, boolean]>]  {
        throw new InternalInterpreterError('「ニャ－、ニャ－」');
    }

    subsumes(state: any, other: PatternExpression): boolean {
        throw new PatternMatchError(
            'Patterns with a pattern guard are not checked for exhaustiveness.');
    }

    cover(state: any, rules: PatternExpression[]): Warning[] {
        // Cannot check whether pattern is covered here
        throw new PatternMatchError(
            'Patterns with a pattern guard are not checked for exhaustiveness.');
    }

    matches(state: any, v: Value): [string, Value][] | undefined {
        throw new InternalInterpreterError('「ニャ－、ニャ－」');
    }

    toString(): string {
        return '( ' + this.pattern + ' if ' + this.condition + ' )';
    }
}
