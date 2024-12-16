// 11 -- safe
import { InternalInterpreterError, ElaborationError,
         EvaluationError, FeatureDisabledError, Warning } from './errors';
import { IdentifierToken, Token, LongIdentifierToken } from './tokens';
import { InterpreterOptions, IState, IdentifierStatus, EvaluationResult, EvaluationStack, EvaluationParameters, Declaration, Value } from './basic';
import { Type, TypeVariable, FunctionType, CustomType, TypeVariableBind } from './types';
import { ValueConstructor, ExceptionConstructor } from './values';
import { State } from './state';
import { DynamicBasis, StaticBasis, TypeInformation } from './state';
import { CaseAnalysis, Lambda, Tuple } from './expressions';
import { ValueDeclaration, ValueBinding, PatternExpression, Expression, ValueIdentifier, Match, TypedExpression } from './declarations0';

export { ValueDeclaration, ValueBinding };

export class TypeDeclaration extends Declaration {
// type typeBinding
    constructor(public typeBinding: TypeBinding[], public id: number = 0) {
        super();
    }

    simplify(): TypeDeclaration {
        let bnds: TypeBinding[] = [];
        for (let i = 0; i < this.typeBinding.length; ++i) {
            bnds.push(new TypeBinding(this.typeBinding[i].typeVariableSequence,
                                      this.typeBinding[i].name,
                                      this.typeBinding[i].type.simplify()));
        }
        return new TypeDeclaration(bnds, this.id);
    }

    elaborate(state: any, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>, isTopLevel: boolean,
              options: InterpreterOptions):
        [State, Warning[], Map<string, [Type, boolean]>, string] {
        for (let i = 0; i < this.typeBinding.length; ++i) {

            let knownVars = new Set<string>();
            let badTyVars: string[] = [];
            for (let nm of this.typeBinding[i].typeVariableSequence) {
                knownVars = knownVars.add(nm.toString());
            }
            this.typeBinding[i].type.instantiate(state, tyVarBnd).getTypeVariables().forEach(
                (val: Type[], key: string) => {
                    if (!knownVars.has(key)) {
                        badTyVars.push(key);
                    }
                });
            if (badTyVars.length > 0) {
                throw ElaborationError.getUnguarded(badTyVars);
            }

            let ex = state.getStaticType(this.typeBinding[i].name.getText());
            if (ex !== undefined && ex.type instanceof CustomType) {
                throw new ElaborationError(
                    'Nnaaa~ Redefining types as aliases is not yet implemented.');
            }
            state.setStaticType(this.typeBinding[i].name.getText(),
                new FunctionType(new CustomType(this.typeBinding[i].name.getText(),
                    this.typeBinding[i].typeVariableSequence),
                    this.typeBinding[i].type.instantiate(state, tyVarBnd)), [],
                    this.typeBinding[i].typeVariableSequence.length,
                    true);
        }

        return [state, [], tyVarBnd, nextName];
    }

    evaluate(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        let state = params.state;
        for (let i = 0; i < this.typeBinding.length; ++i) {
            state.setDynamicType(this.typeBinding[i].name.getText(), []);
        }
        return {
            'newState': state,
            'value': undefined,
            'hasThrown': false,
        };
    }

    toString(): string {
        let res = 'type';
        for (let i = 0; i < this.typeBinding.length; ++i) {
            if (i > 0) {
                res += ' and';
            }
            res += ' <stuff> ' + this.typeBinding[i].name.getText();
            res += ' = ' + this.typeBinding[i].type;
        }
        return res + ';';
    }
}

export class DatatypeDeclaration extends Declaration {
// datatype datatypeBinding <withtype typeBinding>
    constructor(public datatypeBinding: DatatypeBinding[],
                public typeBinding: (TypeBinding[]) | undefined, public id: number = 0,
                public givenIds: {[name: string]: number} = {}) {
        super();

        if (this.typeBinding !== undefined) {
            throw new FeatureDisabledError('Who is "withtype"?');
        }
    }

    simplify(): Declaration {
        let datbnd: DatatypeBinding[] = [];

        for (let i = 0; i < this.datatypeBinding.length; ++i) {
            let ntype: [IdentifierToken, Type|undefined][] = [];
            for (let j = 0; j < this.datatypeBinding[i].type.length; ++j) {
                if (this.datatypeBinding[i].type[j][1] !== undefined) {
                    ntype.push([this.datatypeBinding[i].type[j][0],
                               (<Type> this.datatypeBinding[i].type[j][1]).simplify()]);
                } else {
                    ntype.push(this.datatypeBinding[i].type[j]);
                }
            }
            datbnd.push(new DatatypeBinding(this.datatypeBinding[i].typeVariableSequence,
                this.datatypeBinding[i].name,
                ntype));
        }
        return new DatatypeDeclaration(datbnd, undefined, this.id);
    }

    elaborate(state: any, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>, isTopLevel: boolean,
              options: InterpreterOptions):
        [State, Warning[], Map<string, [Type, boolean]>, string] {
        // I'm assuming the withtype is empty

        let tocheck: Type[] = [];
        for (let i = 0; i < this.datatypeBinding.length; ++i) {
            let res = this.datatypeBinding[i].getType(state, isTopLevel, paramBindings, true);

            state.setStaticType(res[2][0], res[1], res[2][1],
                                this.datatypeBinding[i].typeVariableSequence.length, true);

            for (let j = 0; j < res[0].length; ++j) {
                state.setStaticValue(res[0][j][0], res[0][j][1],
                                     IdentifierStatus.VALUE_CONSTRUCTOR);
            }
        }

        for (let i = 0; i < this.datatypeBinding.length; ++i) {
            let res = this.datatypeBinding[i].getType(state, isTopLevel, paramBindings);

            for (let j = 0; j < res[0].length; ++j) {
                if (!State.allowsRebind(res[0][j][0])) {
                    throw new ElaborationError('You simply cannot rebind "'
                        + res[0][j][0] + '".');
                }
                tocheck.push(res[0][j][1]);

                state.setStaticValue(res[0][j][0], res[0][j][1],
                                     IdentifierStatus.VALUE_CONSTRUCTOR);
            }

            state.setStaticType(res[2][0], res[1], res[2][1],
                                this.datatypeBinding[i].typeVariableSequence.length, true);

            state.incrementValueIdentifierId(res[2][0]);
        }

        for (let i = 0; i < tocheck.length; ++i) {
            tocheck[i].instantiate(state, new Map<string, [Type, boolean]>());
        }

        return [state, [], tyVarBnd, nextName];
    }

    evaluate(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        let state = params.state;
        let modifiable = params.modifiable;
        // I'm assuming the withtype is empty
        for (let i = 0; i < this.datatypeBinding.length; ++i) {
            let res = this.datatypeBinding[i].compute(state, modifiable);

            for (let j = 0; j < res[0].length; ++j) {
                if (!State.allowsRebind(res[0][j][0])) {
                    throw new EvaluationError('You simply cannot rebind "'
                        + res[0][j][0] + '".');
                }
                state.setDynamicValue(res[0][j][0], res[0][j][1], IdentifierStatus.VALUE_CONSTRUCTOR);
            }
            state.setDynamicType(res[1][0], res[1][1]);
            if (this.givenIds[res[1][0]] === undefined) {
                modifiable.incrementValueIdentifierId(res[1][0]);
                this.givenIds[res[1][0]] = state.getValueIdentifierId(res[1][0]);
            }
        }
        return {
            'newState': state,
            'value': undefined,
            'hasThrown': false,
        };
    }

    toString(): string {
        let res = 'datatype';
        for (let i = 0; i < this.datatypeBinding.length; ++i) {
            if (i > 0) {
                res += ' and';
            }
            res += ' ' + this.datatypeBinding[i].name.getText() + ' =';
            for (let j = 0; j < this.datatypeBinding[i].type.length; ++j) {
                if (j > 0) {
                    res += ' |';
                }
                res += ' ' + this.datatypeBinding[i].type[j][0].getText();
                if (this.datatypeBinding[i].type[j][1] !== undefined) {
                    res += ' of ' + (<Type> this.datatypeBinding[i].type[j][1]);
                }
            }
        }
        return res + ';';
    }
}

export class DatatypeReplication extends Declaration {
// datatype name = datatype oldname
    constructor(public name: IdentifierToken,
                public oldname: Token, public id: number = 0) {
        super();
    }

    simplify(): DatatypeReplication {
        return this;
    }

    elaborate(state: any, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>, isTopLevel: boolean,
              options: InterpreterOptions):
    [State, Warning[], Map<string, [Type, boolean]>, string] {
        let res: TypeInformation | undefined = undefined;

        if (this.oldname instanceof LongIdentifierToken) {
            let st = state.getAndResolveStaticStructure(<LongIdentifierToken> this.oldname);
            if (st !== undefined) {
                res = (<StaticBasis> st).getType(
                    (<LongIdentifierToken> this.oldname).id.getText());
            }
        } else {
            res = state.getStaticType(this.oldname.getText());
        }
        if (res === undefined) {
            throw new ElaborationError(
                'The datatype "' + this.oldname.getText() + '" doesn\'t exist.');
        }

        let tp = res.type.instantiate(state, tyVarBnd);

        state.setStaticType(this.name.getText(), new FunctionType(new CustomType(this.name.getText(),
            (<CustomType> tp).typeArguments, (this.oldname instanceof LongIdentifierToken)
            ? this.oldname : undefined), tp), [], res.arity, res.allowsEquality);
        return [state, [], tyVarBnd, nextName];
   }

    evaluate(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        let state = params.state;
        let tp: string[] | undefined = [];
        if (this.oldname instanceof LongIdentifierToken) {
            let st = state.getAndResolveDynamicStructure(<LongIdentifierToken> this.oldname);
            if (st !== undefined) {
                tp = <string[]> (<DynamicBasis> st).getType(
                    (<LongIdentifierToken> this.oldname).id.getText());
            }
        } else {
            tp = <string[]> state.getDynamicType(this.oldname.getText());
        }

        if (tp === undefined) {
            throw new EvaluationError('The datatype "'
                + this.oldname.getText() + '" does not exist.');
        }

        state.setDynamicType(this.name.getText(), tp);
        return {
            'newState': state,
            'value': undefined,
            'hasThrown': false,
        };
    }

    toString(): string {
        return 'datatype ' + this.name.getText() + ' = datatype ' + this.oldname.getText() + ';';
    }
}

export class ExceptionDeclaration extends Declaration {
    constructor(public bindings: ExceptionBinding[],
                public id: number = 0) {
        super();
    }

    simplify(): ExceptionDeclaration {
        return this;
    }

    toString(): string {
        return 'exception <stuff>;';
    }

    elaborate(state: any, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>, isTopLevel: boolean,
              options: InterpreterOptions):
    [State, Warning[], Map<string, [Type, boolean]>, string] {
        let knownTypeVars = new Set<string>();

        tyVarBnd.forEach((val: [Type, boolean], key: string) => {
            if (key.includes('!')) {
                knownTypeVars = knownTypeVars.add(key.substring(1));
            }
        });

        for (let i = 0; i < this.bindings.length; ++i) {
            state = this.bindings[i].elaborate(state, isTopLevel, knownTypeVars, options);
        }
        return [state, [], tyVarBnd, nextName];
    }

    evaluate(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        let state = params.state;
        for (let i = 0; i < this.bindings.length; ++i) {
            this.bindings[i].evaluate(state, params.modifiable);
        }
        return {
            'newState': state,
            'value': undefined,
            'hasThrown': false,
        };
    }
}

export class LocalDeclaration extends Declaration {
// local declaration in body end
    constructor(public declaration: Declaration,
                public body: Declaration, public id: number = 0) {
        super();
    }

    assertUniqueBinding(state: any, conn: Set<string>): Set<string> {
        this.declaration.assertUniqueBinding(state, conn);
        this.body.assertUniqueBinding(state, conn);
        return new Set<string>();
    }

    simplify(): LocalDeclaration {
        return new LocalDeclaration(this.declaration.simplify(), this.body.simplify(), this.id);
    }

    elaborate(state: any, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>, isTopLevel: boolean,
              options: InterpreterOptions):
        [State, Warning[], Map<string, [Type, boolean]>, string] {
        let nstate: [IState, Warning[], Map<string, [Type, boolean]>, string]
            = [state.getNestedState(0).getNestedState(state.id), [], tyVarBnd, nextName];

        let nparbnd = new Map<string, Type>();
        paramBindings.forEach((val: Type, key: string) => {
            nparbnd = nparbnd.set(key, val);
        });

        let res = this.declaration.elaborate(nstate[0], tyVarBnd, nextName, nparbnd,
                                             false, options);
        state.valueIdentifierId = res[0].getIdChanges(0);
        let input = res[0].getNestedState(state.id);
        nstate = this.body.elaborate(input, res[2], res[3], nparbnd, isTopLevel, options);
        // Forget all local definitions
        input.parent = state;
        return [nstate[0], res[1].concat(nstate[1]), nstate[2], nstate[3]];
    }

    evaluate(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        let state = params.state;
        let modifiable = params.modifiable;
        if (params.step === undefined) {
            params.step = -1;
        }

        let step: number = params.step;

        if (step === -1) {
            let parent = state.getNestedState(0);
            let nstate = parent.getNestedState(state.id);
            if (!parent.insideLocalDeclBody) {
                parent.localDeclStart = true;
            }
            params.nstate = nstate;
            params.step = step + 1;
            callStack.push({'next': this, 'params': params});
            callStack.push({
                'next': this.declaration,
                'params': {'state': nstate, 'modifiable': modifiable, 'recResult': undefined}
            });
            return;
        }
        if (step === 0) {
            let res = params.recResult;
            if (res === undefined
                || res.newState === undefined) {
                throw new InternalInterpreterError('How is this undefined?');
            }
            let nnstate = <State> res.newState;

            if (res.hasThrown) {
                // Something came flying in our direction. So, hide we were here and let it fly.
                return {
                    'newState': state,
                    'value': res.value,
                    'hasThrown': true,
                };
            }
            let nstate = nnstate.getNestedState(state.id);
            nstate.insideLocalDeclBody = true;

            params.nstate = nstate;
            params.res = res;
            params.step = step + 1;
            callStack.push({'next': this, 'params': params});
            callStack.push({
                'next': this.body,
                'params': {'state': nstate, 'modifiable': modifiable, 'recResult': undefined}
            });
            return;
        }
        // braced, so linter does not complain about nstate being shadowed
        {
            let nres = params.recResult;
            let nstate = <State> params.nstate;
            let res = <EvaluationResult> params.res;
            if (nres === undefined
                || res === undefined) {
                throw new InternalInterpreterError('How is this undefined?');
            }

            // Forget all local definitions
            nstate.parent = state;
            if (nres.newState !== undefined) {
                (nres.newState.insideLocalDeclBody) = state.insideLocalDeclBody;
            }
            return nres;
        }
    }

    toString(): string {
        let res = 'local ' + this.declaration;
        res += ' in ' + this.body;
        res += ' end;';
        return res;
    }
}

export class OpenDeclaration extends Declaration {
// open name_1 ... name_n
    constructor(public names: Token[], public id: number = 0) {
        super();
    }

    simplify(): OpenDeclaration {
        return this;
    }

    elaborate(state: any, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>, isTopLevel: boolean, options: InterpreterOptions):
        [State, Warning[], Map<string, [Type, boolean]>, string] {
        for (let i = 0; i < this.names.length; ++i) {
            let tmp: StaticBasis | undefined = undefined;
            if (this.names[i] instanceof LongIdentifierToken) {
                tmp = state.getAndResolveStaticStructure(<LongIdentifierToken> this.names[i]);
                if (tmp !== undefined) {
                    tmp = tmp.getStructure((<LongIdentifierToken> this.names[i]).id.getText());
                }
            } else {
                tmp = state.getStaticStructure(this.names[i].getText());
            }
            if (tmp === undefined) {
                throw new EvaluationError(
                    'Undefined module "' + this.names[i].getText() + '".');
            }

            state.staticBasis.extend(<StaticBasis> tmp);
        }
        return [state, [], tyVarBnd, nextName];
    }

    evaluate(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        let state = params.state;
        for (let i = 0; i < this.names.length; ++i) {
            let tmp: DynamicBasis | undefined;
            if (this.names[i] instanceof LongIdentifierToken) {
                tmp = state.getAndResolveDynamicStructure(<LongIdentifierToken> this.names[i]);
                if (tmp !== undefined) {
                    tmp = tmp.getStructure((<LongIdentifierToken> this.names[i]).id.getText());
                }
            } else {
                tmp = state.getDynamicStructure(this.names[i].getText());
            }
            if (tmp === undefined) {
                throw new EvaluationError(
                    'Undefined module "' + this.names[i].getText() + '".');
            }
            state.dynamicBasis.extend(<DynamicBasis> tmp);
        }
        return {
            'newState': state,
            'value': undefined,
            'hasThrown': false,
        };
    }

    toString(): string {
        let res = 'open';
        for (let i = 0; i < this.names.length; ++i) {
            res += ' ' + this.names[i].getText();
        }
        return res + ';';
    }
}

export class EmptyDeclaration extends Declaration {
// exactly what it says on the tin.
    constructor(public id: number = 0) {
        super();
    }

    simplify(): EmptyDeclaration {
        return this;
    }

    elaborate(state: any, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>, isTopLevel: boolean, options: InterpreterOptions):
        [State, Warning[], Map<string, [Type, boolean]>, string] {
        return [state, [], tyVarBnd, nextName];
    }

    evaluate(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        return {
            'newState': params.state,
            'value': undefined,
            'hasThrown': false,
        };
    }

    toString(): string {
        return ' ;';
    }
}

export class SequentialDeclaration extends Declaration {
// declaration1 <;> declaration2
    constructor(public declarations: Declaration[], public id: number = 0) {
        super();
    }

    assertUniqueBinding(state: any, conn: Set<string>): Set<string> {
        for (let i = 0; i < this.declarations.length; ++i) {
            this.declarations[i].assertUniqueBinding(state, conn);
        }
        return new Set<string>();
    }

    simplify(): SequentialDeclaration {
        let decls: Declaration[] = [];
        for (let i = 0; i < this.declarations.length; ++i) {
            decls.push(this.declarations[i].simplify());
        }
        return new SequentialDeclaration(decls, this.id);
    }

    elaborate(state: any, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>, isTopLevel: boolean, options: InterpreterOptions):
        [State, Warning[], Map<string, [Type, boolean]>, string] {
        let warns: Warning[] = [];
        let bnds = tyVarBnd;
        let str = nextName;
        for (let i = 0; i < this.declarations.length; ++i) {
            if (isTopLevel) {
                bnds = new Map<string, [Type, boolean]>();
                state.getTypeVariableBinds()[1].forEach((val: [Type, boolean], key: string) => {
                    bnds = bnds.set(key, val);
                });
                paramBindings = new Map<string, Type>();
            }
            let res = this.declarations[i].elaborate(
                state.getNestedState(this.declarations[i].id),
                bnds, str, paramBindings, isTopLevel, options);
            state = res[0];
            warns = warns.concat(res[1]);
            bnds = res[2];

            let nbnds = new Map<string, [Type, boolean]>();
            if (isTopLevel) {
                nbnds = tyVarBnd;
            }

            res[2].forEach((val: [Type, boolean], key: string) => {
                if (key[1] === '~' ) {
                    // Only free type variables are to be kept
                    let ntp = val[0].instantiate(state, res[2]).normalize(
                        state.freeTypeVariables[0], options);
                    if (!(<State> state.parent).getTypeVariableBinds()[1].has(key)) {
                        warns.push(new Warning(0, 'The free type variable "'
                            + key + '" has been instantiated to "' + ntp[0] + '".\n'));
                    }
                    nbnds = nbnds.set(key, [ntp[0], true]);
                } else if (!isTopLevel) {
                    // nbnds = nbnds.set(key, [val[0].instantiate(state, res[2]), false]);
                    nbnds = nbnds.set(key, val); // [val[0], false]);
                }
            });
            bnds = nbnds;
            if (isTopLevel) {
                state.freeTypeVariables[1] = nbnds;
                for (let v in state.staticBasis.valueEnvironment) {
                    if (state.staticBasis.valueEnvironment.hasOwnProperty(v)) {
                        let tp = <[Type, IdentifierStatus]> state.staticBasis.valueEnvironment[v];
                        let norm = tp[0].normalize(state.freeTypeVariables[0], options);
                        state.freeTypeVariables[0] = norm[1];
                        state.setStaticValue(v, norm[0], tp[1]);
                    }
                }
            }
            str = res[3];
        }
        if (!isTopLevel) {
            // Put together record pieces
            let nbnds = new Map<string, [Type, boolean]>();
            let bnds2 = new Map<string, [Type, boolean]>();
            let skip = new Set<string>();
            bnds.forEach((val: [Type, boolean], nm: string) => {
                bnds2 = nbnds.set(nm, val);
            });
            bnds.forEach((val: [Type, boolean], nm: string) => {
                if (!skip.has(nm)) {
                    nbnds = nbnds.set(nm, val);
                    if (nm.startsWith('@')) {
                        let nname = nm.substr(1);
                        skip = skip.add(nname);
                        if (!bnds2.has(nname)) {
                            nbnds = nbnds.set(nname, val);
                        } else {
                            let mg = <[Type, boolean]> bnds2.get(nname);
                            try {
                                let rmg = mg[0].merge(state, bnds2, val[0]);
                                nbnds = nbnds.set(nname, [rmg[0], mg[1] || val[1]]);
                            } catch (e) {
                                if (!(e instanceof Array)) {
                                    throw e;
                                }
                                throw new ElaborationError('Cannot merge "' + mg[0].normalize()[0]
                                                           + '" and "' + val[0].normalize()[0]
                                                           + '" :' + e[0] + '.');
                            }
                        }
                    }
                }
            });
            bnds = nbnds;
        }
        return [state, warns, bnds, str];
    }

    evaluate(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        let state = params.state;
        if (params.step === undefined) {
            params.step = -1;
        }

        let step: number = params.step;

        if (step >= 0) {
            let val = params.recResult;
            if (val === undefined
                || val.newState === undefined) {
                throw new InternalInterpreterError('How is this undefined?');
            }

            if (val.hasThrown) {
                // Something blew up, so let someone else handle the mess
                return {
                    'newState': val.newState,
                    'value': val.value,
                    'hasThrown': true,
                };
            }
            state = <State> val.newState;
        }
        ++step;
        if (step < this.declarations.length) {
            let nstate = state.getNestedState(this.declarations[step].id);
            params.step = step;
            params.state = state;
            callStack.push({'next': this, 'params': params});
            callStack.push({
                'next': this.declarations[step],
                'params': {'state': nstate, 'modifiable': params.modifiable, 'recResult': undefined}
            });
            return;
        }

        return {
            'newState': state,
            'value': undefined,
            'hasThrown': false,
        };
    }

    toString(): string {
        let res = '';
        for (let i = 0; i < this.declarations.length; ++i) {
            if (i > 0) {
                res += ' ';
            }
            res += this.declarations[i];
        }
        return res;
    }
}

// Derived Forms and semantically irrelevant stuff

export class FunctionDeclaration extends Declaration {
// fun typeVariableSequence functionValueBinding
    constructor(public typeVariableSequence: TypeVariable[],
                public functionValueBinding: FunctionValueBinding[], public id: number = 0) {
        super();
    }

    simplify(): ValueDeclaration {
        let valbnd: ValueBinding[] = [];
        for (let i = 0; i < this.functionValueBinding.length; ++i) {
            valbnd.push(this.functionValueBinding[i].simplify());
        }
        return new ValueDeclaration(this.typeVariableSequence, valbnd, this.id);
    }
}

export class Evaluation extends Declaration {
// do exp
    constructor(public expression: Expression) {
        super();
    }

    simplify(): ValueDeclaration {
        return new ValueDeclaration([],
            [new ValueBinding(false, new Tuple([]), this.expression)]).simplify();
    }
}

export class AbstypeDeclaration extends Declaration {
// abstype datatypeBinding <withtype typeBinding> with declaration end
    constructor(public datatypeBinding: DatatypeBinding[],
                public typeBinding: (TypeBinding[]) | undefined, public declaration: Declaration,
                public id: number = 0) {
        super();

        if (this.typeBinding !== undefined) {
            throw new FeatureDisabledError('Who is "withtype"?');
        }
    }

    simplify(): LocalDeclaration {
        let dat = new DatatypeDeclaration(this.datatypeBinding, undefined, this.id);
        let tpbnd: TypeBinding[] = [];
        for (let i = 0; i < this.datatypeBinding.length; ++i) {
            tpbnd.push(new TypeBinding(this.datatypeBinding[i].typeVariableSequence,
                this.datatypeBinding[i].name,
                new CustomType(this.datatypeBinding[i].name.getText(),
                    this.datatypeBinding[i].typeVariableSequence)));
        }
        let tp = new TypeDeclaration(tpbnd, this.id);
        return new LocalDeclaration(
            dat, new SequentialDeclaration([tp, this.declaration],
                this.id), this.id).simplify();
    }
}


export class InfixDeclaration extends Declaration {
// infix <d> vid1 .. vidn
    constructor(public operators: IdentifierToken[],
                public precedence: number = 0, public id: number = 0) {
        super();
    }

    simplify(): InfixDeclaration {
        return this;
    }

    elaborate(state: any, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [State, Warning[], Map<string, [Type, boolean]>, string] {
        return [state, [], tyVarBnd, nextName];
    }

    setInfixStatus(state: any): void {
        for (let i = 0; i < this.operators.length; ++i) {
            state.setInfixStatus(this.operators[i], this.precedence, false, true);
        }
    }

    evaluate(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        let state = params.state;
        this.setInfixStatus(state);
        return {
            'newState': state,
            'value': undefined,
            'hasThrown': false,
        };
    }

    toString(): string {
        let res = 'infix';
        res += ' ' + this.precedence;
        for (let i = 0; i < this.operators.length; ++i) {
            res += ' ' + this.operators[i].getText();
        }
        return res + ';';
    }
}

export class InfixRDeclaration extends Declaration {
// infixr <d> vid1 .. vidn
    constructor(public operators: IdentifierToken[],
                public precedence: number = 0, public id: number = 0) {
        super();
    }

    simplify(): InfixRDeclaration {
        return this;
    }

    elaborate(state: any, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [State, Warning[], Map<string, [Type, boolean]>, string] {
        return [state, [], tyVarBnd, nextName];
    }

    setInfixStatus(state: any): void {
        for (let i = 0; i < this.operators.length; ++i) {
            state.setInfixStatus(this.operators[i], this.precedence, true, true);
        }
    }

    evaluate(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        let state = params.state;
        this.setInfixStatus(state);
        return {
            'newState': state,
            'value': undefined,
            'hasThrown': false,
        };
    }

    toString(): string {
        let res = 'infixr';
        res += ' ' + this.precedence;
        for (let i = 0; i < this.operators.length; ++i) {
            res += ' ' + this.operators[i].getText();
        }
        return res + ';';
    }
}

export class NonfixDeclaration extends Declaration {
// nonfix <d> vid1 .. vidn
    constructor(public operators: IdentifierToken[],
                public id: number = 0) {
        super();
    }

    simplify(): NonfixDeclaration {
        return this;
    }

    elaborate(state: any, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [State, Warning[], Map<string, [Type, boolean]>, string] {
        return [state, [], tyVarBnd, nextName];
    }

    setInfixStatus(state: any): void {
        for (let i = 0; i < this.operators.length; ++i) {
            state.setInfixStatus(this.operators[i], 0, false, false);
        }
    }

    evaluate(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        let state = params.state;
        this.setInfixStatus(state);
        return {
            'newState': state,
            'value': undefined,
            'hasThrown': false,
        };
    }

    toString(): string {
        let res = 'nonfix';
        for (let i = 0; i < this.operators.length; ++i) {
            res += ' ' + this.operators[i].getText();
        }
        return res + ';';
    }
}


export class FunctionValueBinding {
    constructor(public parameters: [PatternExpression[], Type|undefined, Expression][],
                public name: ValueIdentifier) {
    }

    simplify(): ValueBinding {
        if (this.name === undefined) {
            throw new InternalInterpreterError(
                'This function isn\'t ready to be simplified yet.');
        }

        // Build the case analysis, starting with the (vid1,...,vidn)
        let arr: ValueIdentifier[] = [];
        let matches: [PatternExpression, Expression][] = [];
        for (let i = 0; i < this.parameters[0][0].length; ++i) {
            arr.push(new ValueIdentifier(new IdentifierToken('__arg' + i)));
        }
        for (let i = 0; i < this.parameters.length; ++i) {
            let pat2: PatternExpression;
            if (this.parameters[i][0].length === 1) {
                pat2 = this.parameters[i][0][0];
            } else {
                pat2 = new Tuple(this.parameters[i][0]);
            }

            if (this.parameters[i][1] === undefined) {
                matches.push([pat2, this.parameters[i][2]]);
            } else {
                matches.push([pat2,
                    new TypedExpression(this.parameters[i][2], <Type> this.parameters[i][1])]);
            }
        }
        let pat: PatternExpression;
        if (arr.length !== 1) {
            pat = new Tuple(arr).simplify();
        } else {
            pat = arr[0];
        }
        let mat = new Match(matches);
        let exp: Expression;
        //        if (arr.length === 1) {
        //    exp = new Lambda(mat);
        // } else {
        exp = new CaseAnalysis(pat, mat);

        // Now build the lambdas around
        for (let i = this.parameters[0][0].length - 1; i >= 0; --i) {
            exp = new Lambda(new Match([[
                new ValueIdentifier(new IdentifierToken('__arg' + i)),
                exp]]));
        }
        // }

        return new ValueBinding(true, this.name, exp.simplify());
    }

    toString(): string {
        let res = '';
        for (let i = 0; i < this.parameters.length; ++i) {
            if (i > 0) {
                res += ' | ';
            }
            res += this.name.name.getText();
            for (let j = 0; j < this.parameters[i][0].length; ++j) {
                res += ' ' + this.parameters[i][0][j];
            }
            if (this.parameters[i][1] !== undefined) {
                res += ': ' + (<Type> this.parameters[i][1]);
            }
            res += ' = ' + this.parameters[i][2];
        }
        return res;
    }
}

// Type Bindings

export class TypeBinding {
// typeVariableSequence name = type
    constructor(public typeVariableSequence: TypeVariable[],
                public name: IdentifierToken, public type: Type) {
    }
}

// Datatype Bindings

export class DatatypeBinding {
// typeVariableSequence name = <op> constructor <of type>
    // type: [constructorName, <type>]
    constructor(public typeVariableSequence: TypeVariable[],
                public name: IdentifierToken, public type: [IdentifierToken, Type | undefined][],
                public givenIds: {[name: string]: number} = {}) {
    }

    getType(state: any, isTopLevel: boolean, paramBindings: Map<string, Type>,
            simplified: boolean = false):
        [[string, Type][], Type, [string, string[]]] {
        let connames: string[] = [];
        let ve: [string, Type][] = [];
        let nstate = state.getNestedState(state.id);

        let id = state.getValueIdentifierId(this.name.getText());
        nstate.incrementValueIdentifierId(this.name.getText());

        let idlesstp = new CustomType(this.name.getText(), this.typeVariableSequence,
            undefined, false, 0);
        let restp = new CustomType(this.name.getText(), this.typeVariableSequence,
            undefined, false, id);
        let arity = this.typeVariableSequence.length;
        nstate.setStaticType(this.name.getText(), restp, [], arity, true);
        for (let i = 0; i < this.type.length; ++i) {
            let tp: Type = restp;

            if (this.type[i][1] !== undefined) {
                let curtp = (<Type> this.type[i][1]);
                if (!simplified) {
                    curtp.instantiate(nstate, new Map<string, [Type, boolean]>());
                }
                curtp = curtp.replace(idlesstp, restp);
                tp = new FunctionType(curtp, tp);
            }

            let tvs = new Set<string>();
            for (let j = 0; j < this.typeVariableSequence.length; ++j) {
                if (tvs.has(this.typeVariableSequence[j].name)) {
                    throw new ElaborationError(
                        'I\'m not interested in duplicate type variable names such as "'
                        + this.typeVariableSequence[j] + '".');
                }
                tvs = tvs.add(this.typeVariableSequence[j].name);
            }
            let ungar: string[] = [];

            tp.getTypeVariables().forEach((val: Type[], key: string) => {
                if (!tvs.has(key)) {
                    ungar.push(key);
                } else {
                    tp = new TypeVariableBind(key, tp, val);
                }
            });

            if (ungar.length > 0) {
                throw ElaborationError.getUnguarded(ungar);
            }

            ve.push([this.type[i][0].getText(), tp]);
            connames.push(this.type[i][0].getText());
        }
        return [ve, restp, [this.name.getText(), connames]];
    }

    compute(state: any, modifiable: any): [[string, Value][], [string, string[]]] {
        let connames: string[] = [];
        let ve: [string, Value][] = [];
        for (let i = 0; i < this.type.length; ++i) {
            let numArg: number = 0;
            if (this.type[i][1] !== undefined) {
                numArg = 1;
            }
            let id = -1;
            if (this.givenIds[this.type[i][0].getText()] === undefined) {
                id = modifiable.getValueIdentifierId(this.type[i][0].getText());
                modifiable.incrementValueIdentifierId(this.type[i][0].getText());
                this.givenIds[this.type[i][0].getText()] = id;
            } else {
                id = this.givenIds[this.type[i][0].getText()];
            }
            ve.push([this.type[i][0].getText(), new ValueConstructor(this.type[i][0].getText(), numArg, id)]);
            connames.push(this.type[i][0].getText());
        }
        return [ve, [this.name.getText(), connames]];
    }
}

// Exception Bindings

export interface ExceptionBinding {
    evaluate(state: any, modifiable: any): void;
    elaborate(state: any, isTopLevel: boolean, knownTypeVars: Set<string>,
              options: InterpreterOptions): any;
}

export class DirectExceptionBinding implements ExceptionBinding {
// <op> name <of type>
    constructor(public name: IdentifierToken,
                public type: Type | undefined) {
    }

    elaborate(state: any, isTopLevel: boolean, knownTypeVars: Set<string>,
              options: InterpreterOptions): any {
        if (this.type !== undefined) {
            let tp = this.type.simplify().instantiate(state, new Map<string, [Type, boolean]>());
            let tyvars: string[] = [];
            tp.getTypeVariables().forEach((dom: Type[], val: string) => {
                //                if (!knownTypeVars.has(val)) {
                    tyvars.push(val);
                // }
            });
            if (isTopLevel && tyvars.length > 0) {
                throw ElaborationError.getUnguarded(tyvars);
            }

            state.setStaticValue(this.name.getText(),
                new FunctionType(tp.makeFree(), new CustomType('exn')),
                IdentifierStatus.EXCEPTION_CONSTRUCTOR);
        } else {
            state.setStaticValue(this.name.getText(), new CustomType('exn'),
                IdentifierStatus.EXCEPTION_CONSTRUCTOR);
        }
        return state;
    }

    evaluate(state: any, modifiable: any): void {
        let numArg = 0;
        if (this.type !== undefined) {
            numArg = 1;
        }
        let id = state.getValueIdentifierId(this.name.getText());
        state.incrementValueIdentifierId(this.name.getText());
        let evalId = modifiable.getNextExceptionEvalId();

        if (!State.allowsRebind(this.name.getText())) {
            throw new EvaluationError('You simply cannot rebind "'
                + this.name.getText() + '".');
        }

        state.setDynamicValue(this.name.getText(),
            new ExceptionConstructor(this.name.getText(), numArg, id, evalId), IdentifierStatus.EXCEPTION_CONSTRUCTOR);
    }
}

export class ExceptionAlias implements ExceptionBinding {
// <op> name = <op> oldname
    constructor(public name: IdentifierToken, public oldname: Token) {
    }

    elaborate(state: any, isTopLevel: boolean, knownTypeVars: Set<string> = new Set<string>(),
              options: InterpreterOptions): any {
        let res: [Type, IdentifierStatus] | undefined = undefined;
        if (this.oldname instanceof LongIdentifierToken) {
            let st = state.getAndResolveStaticStructure(<LongIdentifierToken> this.oldname);
            if (st !== undefined) {
                res = st.getValue((<LongIdentifierToken> this.oldname).id.getText());
            }
        } else {
            res = state.getStaticValue(this.oldname.getText());
        }
        if (res === undefined) {
            throw new ElaborationError('Unbound value identifier "'
                + this.oldname.getText() + '".');
        } else if (res[1] !== IdentifierStatus.EXCEPTION_CONSTRUCTOR) {
            throw new ElaborationError('You cannot transform "'
                + res[0] + '" into an exception.');
        }
        state.setStaticValue(this.name.getText(), res[0].normalize()[0], IdentifierStatus.EXCEPTION_CONSTRUCTOR);
        return state;
    }

    evaluate(state: any, modifiable: any): void {
        let res: [Value, IdentifierStatus] | undefined = undefined;
        if (this.oldname instanceof LongIdentifierToken) {
            let st = state.getAndResolveDynamicStructure(<LongIdentifierToken> this.oldname);
            if (st !== undefined) {
                res = st.getValue((<LongIdentifierToken> this.oldname).id.getText());
            }
        } else {
            res = state.getDynamicValue(this.oldname.getText());
        }
        if (res === undefined) {
            throw new EvaluationError('Unbound value identifier "'
                + this.oldname.getText() + '".');
        } else if (res[1] !== IdentifierStatus.EXCEPTION_CONSTRUCTOR) {
            throw new EvaluationError('You cannot transform "'
                + res[0].toString(state, 40) + '" into an exception.');
        }
        state.setDynamicValue(this.name.getText(), res[0], IdentifierStatus.EXCEPTION_CONSTRUCTOR);
    }
}
