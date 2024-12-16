import { Warning, EvaluationError, ElaborationError, InternalInterpreterError } from './errors';
import { IdentifierToken, Token, LongIdentifierToken } from './tokens';
import { Expression } from './expressions';
import { Declaration } from './declarations';
import { Type, TypeVariable, CustomType, TypeVariableBind, FunctionType } from './types';
import { State, DynamicInterface, DynamicStructureInterface, DynamicValueInterface, StaticBasis,
         DynamicTypeInterface, IdentifierStatus, DynamicBasis, DynamicFunctorInformation,
         TypeInformation, Structure, EvaluationResult, EvaluationParameters, EvaluationStack } from './state';
import { Value } from './values';
import { getInitialState } from './initialState';

// Module Expressions

// Structure Expressions



export class StructureExpression extends Expression implements Structure {
// struct <strdec> end
    constructor(public structureDeclaration: Declaration) {
        super();
    }

    simplify(): StructureExpression {
        return new StructureExpression(this.structureDeclaration.simplify());
    }

    elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [StaticBasis, Warning[], Map<string, [Type, boolean]>, string] {
        let nstate = state.getNestedState(0).getNestedState(state.id);
        let tmp = this.structureDeclaration.elaborate(nstate, tyVarBnd, nextName, paramBindings,
                                                      true);
        state.valueIdentifierId = tmp[0].valueIdentifierId;
        return [tmp[0].getStaticChanges(0), tmp[1], tmp[2], tmp[3]];
    }

    computeStructure(params: EvaluationParameters, callStack: EvaluationStack, recCall: Declaration):
        DynamicBasis | Value | undefined {
        if (params.recResult === undefined) {
            let state = params.state;
            let nstate = state.getNestedState(0).getNestedState(state.id);

            callStack.push({'next': recCall, 'params': params});

            callStack.push({
                'next': this.structureDeclaration,
                'params': {'state': nstate, 'modifiable': params.modifiable, 'recResult': undefined}
            });
            return;
        }
        // braced so linter does not complain about shadowed names
        {
            let tmp = params.recResult;
            if (tmp === undefined || tmp.newState === undefined) {
                throw new InternalInterpreterError('RAINBOW!');
            }
            let nstate = <State> tmp.newState;

            if (tmp.hasThrown) {
                return <Value> tmp.value;
            }
            return nstate.getDynamicChanges(0);
        }
    }

    toString(): string {
        return 'struct ' + this.structureDeclaration + ' end';
    }
}

export class StructureIdentifier extends Expression implements Structure {
// longstrid
    constructor(public identifier: Token) {
        super();
    }

    simplify(): StructureIdentifier {
        return this;
    }

    elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [StaticBasis, Warning[], Map<string, [Type, boolean]>, string] {
        let res: StaticBasis | undefined = undefined;
        if (this.identifier instanceof LongIdentifierToken) {
            let st = state.getAndResolveStaticStructure(<LongIdentifierToken> this.identifier);

            if (st !== undefined) {
                res = (<StaticBasis> st).getStructure(
                    (<LongIdentifierToken> this.identifier).id.getText());
            }
        } else {
            res = state.getStaticStructure(this.identifier.getText());
        }

        if (res === undefined) {
            throw new ElaborationError('Undefined module "'
                + this.identifier.getText() + '".');
        }
        return [res, [], tyVarBnd, nextName];
    }

    computeStructure(params: EvaluationParameters, callStack: EvaluationStack, recCall: Declaration):
        DynamicBasis | Value | undefined {
        let state = params.state;
        let res: DynamicBasis | undefined = undefined;
        if (this.identifier instanceof LongIdentifierToken) {
            let st = state.getAndResolveDynamicStructure(<LongIdentifierToken> this.identifier);

            if (st !== undefined) {
                res = (<DynamicBasis> st).getStructure(
                    (<LongIdentifierToken> this.identifier).id.getText());
            }
        } else {
            res = state.getDynamicStructure(this.identifier.getText());
        }

        if (res === undefined) {
            throw new EvaluationError('Undefined module "'
                + this.identifier.getText() + '".');
        }
        return <DynamicBasis> res;
    }

    toString(): string {
        return this.identifier.getText();
    }
}

export class TransparentConstraint extends Expression implements Structure {
// strexp : sigexp
    static restrict(sig: StaticBasis, str: StaticBasis, state: State,
                    tyVarBnd: Map<string, [Type, boolean]>, nextName: string):
        [StaticBasis, Warning[], Map<string, [Type, boolean]>, string] {
        let res = new StaticBasis({}, {}, {}, {}, {});

        let nstate = state.getNestedState(state.id);
        nstate.staticBasis = str;
        let nstate2 = state.getNestedState(state.id);
        nstate2.staticBasis = sig;

        for (let i in sig.typeEnvironment) {
            if (sig.typeEnvironment.hasOwnProperty(i)) {
                if (!str.typeEnvironment.hasOwnProperty(i)) {
                    throw new ElaborationError(
                        'Signature mismatch: Unimplemented type "' + i + '".');
                }
                let sg = <TypeInformation> sig.typeEnvironment[i];
                let st = <TypeInformation> str.typeEnvironment[i];

                if (sg.arity !== st.arity) {
                    throw new ElaborationError(
                        'Signature mismatch: Implementation of type "' + i
                        + '" has the wrong arity.');
                }

                try {
                    let sgtp = <CustomType> sg.type;
                    let sttp = st.type;
                    let eqtp = st.type;
                    if (!(sg.type instanceof FunctionType) && st.type instanceof FunctionType) {
                        eqtp = (<FunctionType> st.type).returnType;
                        sttp = (<FunctionType> st.type).parameterType;
                    }
                    let tp = sgtp.merge(nstate, tyVarBnd, sttp);

                    if (sg.allowsEquality && !eqtp.admitsEquality(nstate)) {
                        throw new ElaborationError(
                            'Signature mismatch: Implementation of type "' + i
                            + '" should admit equality.');
                    }

                    let insttp = sttp.instantiate(nstate2, tp[1]);

                    if( insttp instanceof FunctionType ) {
                        res.setType(i, sttp, sg.constructors, sg.arity, true);
                    } else {
                        res.setType(i, st.type.instantiate(nstate2, tp[1]), sg.constructors,
                                    sg.arity, true);
                    }

                    tyVarBnd = tp[1];
                } catch (e) {
                    if (!(e instanceof Array)) {
                        throw e;
                    }
                    throw new ElaborationError(
                        'Signature mismatch: Wrong implementation of type "' + i + '": ' + e[0]);
                }
            }
        }

        for (let i in sig.valueEnvironment) {
            if (sig.valueEnvironment.hasOwnProperty(i)) {
                if (!str.valueEnvironment.hasOwnProperty(i)) {
                    throw new ElaborationError(
                        'Signature mismatch: Unimplemented value "' + i + '".');
                }
                let sg = <[Type, IdentifierStatus]> sig.valueEnvironment[i];
                let st = <[Type, IdentifierStatus]> str.valueEnvironment[i];

                let repl = new Map<string, string>();
                let vsg = sg[0].getTypeVariables();
                let vst = st[0].getTypeVariables();
                while (st[0] instanceof TypeVariableBind) {
                    while (true) {
                        let cur = +nextName.substring(3);
                        let nname = '\'' + nextName[1] + nextName[2] + (cur + 1);
                        nextName = nname;

                        if (!tyVarBnd.has(nname) && !vsg.has(nname) && !vst.has(nname)) {
                            if ((<TypeVariableBind> st[0]).name[1] === '\'') {
                                nname = '\'' + nname;
                            }
                            repl = repl.set((<TypeVariableBind> st[0]).name, nname);
                            break;
                        }

                    }
                    st[0] = (<TypeVariableBind> st[0]).type;
                }
                st[0] = st[0].replaceTypeVariables(repl);

                let nsg = sg[0];
                while (nsg instanceof TypeVariableBind) {
                    while (true) {
                        let cur = +nextName.substring(3);
                        let nname = '\'' + nextName[1] + nextName[2] + (cur + 1);
                        nextName = nname;

                        if (!tyVarBnd.has(nname) && !vsg.has(nname) && !vst.has(nname)) {
                            if ((<TypeVariableBind> nsg).name[1] === '\'') {
                                nname = '\'' + nname;
                            }
                            repl = repl.set((<TypeVariableBind> nsg).name, nname);
                            break;
                        }

                    }
                    nsg = (<TypeVariableBind> nsg).type;
                }
                nsg = nsg.replaceTypeVariables(repl);

                try {
                    let mg = nsg.merge(nstate, tyVarBnd, st[0]);
                    if (mg[0].getTypeVariables().size < nsg.getTypeVariables().size) {
                        throw new ElaborationError(
                            'Signature mismatch: Implementation of value "' + i
                            + '" has type "' + mg[0].normalize()[0]
                            + '" which is less general than the'
                            + ' required type "' + nsg.normalize()[0] + '".');
                    }
                    res.setValue(i, sg[0].instantiate(nstate, mg[1]), sg[1]);
                } catch (e) {
                    if (!(e instanceof Array)) {
                        throw e;
                    }
                    throw new ElaborationError(
                        'Signature mismatch: Wrong implementation of value "' + i + '": ' + e[0]);
                }
            }
        }

        for (let i in sig.structureEnvironment) {
            if (sig.structureEnvironment.hasOwnProperty(i)) {
                if (!str.structureEnvironment.hasOwnProperty(i)) {
                    throw new ElaborationError(
                        'Unimplemented structure "' + i + '".');
                }

                try {
                    let tmp = TransparentConstraint.restrict(<StaticBasis> sig.getStructure(i),
                        <StaticBasis> str.getStructure(i), nstate, tyVarBnd, nextName);
                    res.setStructure(i, tmp[0]);
                    tyVarBnd = tmp[2];
                    nextName = tmp[3];
                } catch (e) {
                    throw new ElaborationError(
                        'Signature Mismatch: Wrong implementation of structure "' + i + '": '
                        + e.message);
                }
            }
        }

        return [res, [], tyVarBnd, nextName];
    }

    constructor(public structureExpression: Expression & Structure,
                public signatureExpression: Expression & Signature) {
        super();
    }

    simplify(): TransparentConstraint {
        return new TransparentConstraint(
            <Expression & Structure> this.structureExpression.simplify(),
            <Expression & Signature> this.signatureExpression.simplify());
    }

    elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [StaticBasis, Warning[], Map<string, [Type, boolean]>, string] {
        let str = this.structureExpression.elaborate(state, tyVarBnd, nextName, paramBindings);
        let sig = this.signatureExpression.elaborate(state, str[2], str[3], paramBindings);

        return TransparentConstraint.restrict(sig[0], str[0], state, sig[2], sig[3]);
    }

    computeStructure(params: EvaluationParameters, callStack: EvaluationStack, recCall: Declaration):
        DynamicBasis | Value | undefined {
        let tmp = this.structureExpression.computeStructure(params, callStack, recCall);
        if (tmp === undefined) {
            return undefined;
        }
        if (tmp instanceof Value) {
            return tmp;
        }
        let sig = this.signatureExpression.computeInterface(params.state);
        return (<DynamicBasis> tmp).restrict(sig);
    }

    toString(): string {
        return this.structureExpression + ' : ' + this.signatureExpression;
    }
}

export class OpaqueConstraint extends Expression implements Structure {
// strexp :> sigexp
    static restrict(sig: StaticBasis, str: StaticBasis, state: State,
                    tyVarBnd: Map<string, [Type, boolean]>, nextName: string):
        [StaticBasis, Warning[], Map<string, [Type, boolean]>, string] {

        let res = new StaticBasis({}, {}, {}, {}, {});

        let nstate = state.getNestedState(state.id);
        nstate.staticBasis = str;
        let nstate2 = state.getNestedState(state.id);
        nstate2.staticBasis = sig;
        nstate2 = nstate2.getNestedState(state.id);

        for (let i in sig.typeEnvironment) {
            if (sig.typeEnvironment.hasOwnProperty(i)) {
                if (!str.typeEnvironment.hasOwnProperty(i)) {
                    throw new ElaborationError(
                        'Signature mismatch: Unimplemented type "' + i + '".');
                }
                let sg = <TypeInformation> sig.typeEnvironment[i];
                let st = <TypeInformation> str.typeEnvironment[i];

                if (sg.arity !== st.arity) {
                    throw new ElaborationError(
                        'Signature mismatch: Implementation of type "' + i
                        + '" has the wrong arity.');
                }

                try {
                    let sgtp = <CustomType> sg.type;
                    let sttp = st.type;
                    let eqtp = st.type;
                    if (!(sg.type instanceof FunctionType) && st.type instanceof FunctionType) {
                        eqtp = (<FunctionType> st.type).returnType;
                        sttp = (<FunctionType> st.type).parameterType;
                    }
                    let tp = sgtp.merge(nstate, tyVarBnd, sttp);
                    // We need to create a new type here because of reference stuff
                    sgtp = new CustomType(sgtp.name, sgtp.typeArguments,
                        sgtp.qualifiedName, true);

                    if (sg.allowsEquality && !eqtp.admitsEquality(nstate)) {
                        throw new ElaborationError(
                            'Signature mismatch: Implementation of type "' + i
                            + '" should admit equality.');
                    }

                    if( sttp instanceof FunctionType ) {
                        res.setType(i, sttp, sg.constructors,
                                sg.arity, true);
                        nstate2.staticBasis.setType(i, sttp, sg.constructors,
                                sg.arity, true);
                    } else {
                        res.setType(i, sgtp, [], sg.arity, sg.allowsEquality);
                        nstate2.staticBasis.setType(i, sgtp, sg.constructors, sg.arity,
                                                    sg.allowsEquality);
                    }

                    tyVarBnd = tp[1];
                } catch (e) {
                    if (!(e instanceof Array)) {
                        throw e;
                    }
                    throw new ElaborationError(
                        'Signature mismatch: Wrong implementation of type "' + i + '": ' + e[0]);
                }
            }
        }

        for (let i in sig.valueEnvironment) {
            if (sig.valueEnvironment.hasOwnProperty(i)) {
                if (!str.valueEnvironment.hasOwnProperty(i)) {
                    throw new ElaborationError(
                        'Signature mismatch: Unimplemented value "' + i + '".');
                }
                let sg = <[Type, IdentifierStatus]> sig.valueEnvironment[i];
                let st = <[Type, IdentifierStatus]> str.valueEnvironment[i];

                let repl = new Map<string, string>();
                let vsg = sg[0].getTypeVariables();
                let vst = st[0].getTypeVariables();
                let nst = st[0];
                while (nst instanceof TypeVariableBind) {
                    while (true) {
                        let cur = +nextName.substring(3);
                        let nname = '\'' + nextName[1] + nextName[2] + (cur + 1);
                        nextName = nname;

                        if (!tyVarBnd.has(nname) && !vsg.has(nname) && !vst.has(nname)) {
                            if ((<TypeVariableBind> nst).name[1] === '\'') {
                                nname = '\'' + nname;
                            }
                            repl = repl.set((<TypeVariableBind> nst).name, nname);
                            break;
                        }

                    }
                    nst = (<TypeVariableBind> nst).type;
                }
                nst = nst.replaceTypeVariables(repl);

                let nsg = sg[0];
                while (nsg instanceof TypeVariableBind) {
                    while (true) {
                        let cur = +nextName.substring(3);
                        let nname = '\'' + nextName[1] + nextName[2] + (cur + 1);
                        nextName = nname;

                        if (!tyVarBnd.has(nname) && !vsg.has(nname) && !vst.has(nname)) {
                            if ((<TypeVariableBind> nsg).name[1] === '\'') {
                                nname = '\'' + nname;
                            }
                            repl = repl.set((<TypeVariableBind> nsg).name, nname);
                            break;
                        }

                    }
                    nsg = (<TypeVariableBind> nsg).type;
                }
                nsg = nsg.replaceTypeVariables(repl);

                try {
                    let mg = nsg.merge(nstate, tyVarBnd, nst);

                    if (mg[0].getTypeVariables().size < nsg.getTypeVariables().size) {
                        throw new ElaborationError(
                            'Signature mismatch: Implementation of value "' + i
                            + '" has type "' + mg[0].normalize()[0]
                            + '" which is less general than the'
                            + ' required type "' + sg[0] + '".');
                    }

                    res.setValue(i, sg[0].instantiate(nstate2, tyVarBnd), sg[1]);
                } catch (e) {
                    if (!(e instanceof Array)) {
                        throw e;
                    }
                    throw new ElaborationError(
                        'Signature mismatch: Wrong implementation of type "' + i + '": ' + e[0]);
                }
            }
        }

        for (let i in sig.structureEnvironment) {
            if (sig.structureEnvironment.hasOwnProperty(i)) {
                if (!str.structureEnvironment.hasOwnProperty(i)) {
                    throw new ElaborationError('Unimplemented structure "' + i + '".');
                }

                try {
                    let tmp = OpaqueConstraint.restrict(<StaticBasis> sig.getStructure(i),
                        <StaticBasis> str.getStructure(i), nstate, tyVarBnd, nextName);
                    res.setStructure(i, tmp[0]);
                    tyVarBnd = tmp[2];
                    nextName = tmp[3];
                } catch (e) {
                    throw new ElaborationError(
                        'Signature Mismatch: Wrong implementation of structure "' + i + '": '
                        + e.message);
                }
            }
        }

        return [res, [], tyVarBnd, nextName];
    }

    constructor(public structureExpression: Expression & Structure,
                public signatureExpression: Expression & Signature) {
        super();
    }

    simplify(): OpaqueConstraint {
        return new OpaqueConstraint(<Expression & Structure> this.structureExpression.simplify(),
            <Expression & Signature> this.signatureExpression.simplify());
    }

    elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [StaticBasis, Warning[], Map<string, [Type, boolean]>, string] {

        let str = this.structureExpression.elaborate(state, tyVarBnd, nextName, paramBindings);
        let sig = this.signatureExpression.elaborate(state, str[2], str[3], paramBindings);

        return OpaqueConstraint.restrict(sig[0], str[0], state, sig[2], sig[3]);
   }

    computeStructure(params: EvaluationParameters, callStack: EvaluationStack, recCall: Declaration):
        DynamicBasis | Value | undefined {
        let tmp = this.structureExpression.computeStructure(params, callStack, recCall);
        if (tmp === undefined) {
            return undefined;
        }
        if (tmp instanceof Value) {
            return tmp;
        }
        let sig = this.signatureExpression.computeInterface(params.state);
        return (<DynamicBasis> tmp).restrict(sig);
    }

    toString(): string {
        return this.structureExpression + ' :> ' + this.signatureExpression;
    }
}

export class FunctorApplication extends Expression implements Structure {
// funid ( strexp )
    constructor(public functorId: IdentifierToken,
                public structureExpression: Expression & Structure) {
        super();
    }

    simplify(): FunctorApplication {
        return new FunctorApplication(this.functorId,
            <Expression & Structure> this.structureExpression.simplify());
    }

    elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [StaticBasis, Warning[], Map<string, [Type, boolean]>, string] {

        let str = this.structureExpression.elaborate(state, tyVarBnd, nextName, paramBindings);

        let fun = state.getStaticFunctor(this.functorId.getText());

        if (fun === undefined) {
            throw new EvaluationError('Undefined functor "' + this.functorId.getText() + '".');
        }

        let tst = state.getNestedState(state.id);
        let res = new StaticBasis({}, {}, {}, {}, {});
        let warns: Warning[] = [];
        if (fun[3]) {
            // Dirty hack: we have to open the structure given as parameter
            tst.staticBasis = tst.staticBasis.extend(str[0]);
            warns.push(new Warning(0, 'Using approximately elaborated functor "' +
                                   this.functorId.getText() + '".\n'));
        }
        tst.setStaticStructure(fun[2], TransparentConstraint.restrict(fun[0], str[0], state,
                                                                      tyVarBnd, nextName)[0]);
        res = res.extend(fun[1]);
        tst.staticBasis = tst.staticBasis.extend(fun[1]);

        for (let i in res.valueEnvironment) {
            if (res.valueEnvironment.hasOwnProperty(i)) {
                let tmp = res.valueEnvironment[i];

                if (tmp !== undefined && tmp[0] !== undefined) {
                    res.valueEnvironment[i] = [tmp[0].instantiate(tst, str[2]), tmp[1]];
                }
            }
        }
        for (let i in res.typeEnvironment) {
            if (res.typeEnvironment.hasOwnProperty(i)) {
                if (res.typeEnvironment[i].type instanceof FunctionType) {
                    let oldtp: FunctionType = (res.typeEnvironment[i].type as FunctionType);
                    res.typeEnvironment[i].type =
                        new FunctionType(oldtp.parameterType,
                                         oldtp.returnType.instantiate(tst, str[2]));
                }
            }
        }

        return [res, str[1].concat(warns), str[2], str[3]];
    }

    computeStructure(params: EvaluationParameters, callStack: EvaluationStack, recCall: Declaration):
        DynamicBasis | Value | undefined {

        let state = params.state;
        let fun = state.getDynamicFunctor(this.functorId.getText());

        if (fun === undefined) {
            throw new EvaluationError('Undefined functor "' + this.functorId.getText() + '".');
        }

        let res = this.structureExpression.computeStructure(params, callStack, recCall);
        if (res === undefined || res instanceof Value) {
            return res;
        }

        let nnstate = fun.state.getNestedState(fun.state.id);
        nnstate.dynamicBasis = nnstate.dynamicBasis.extend(<DynamicBasis> res);

        nnstate.setDynamicStructure(fun.paramName.getText(),
                                    (<DynamicBasis> res).restrict(fun.param));

        // we have to fake our state for a short time, so computeStructure will add the
        // correct recursive state

        if (params[this + ''] === undefined) {
            params.recResult = undefined;
            params[this + ''] = true;
        }
        params.state = nnstate;
        let nres = fun.body.computeStructure(params, callStack, recCall);
        params.state = state;

        return nres;
    }

    toString(): string {
        return this.functorId.getText() + '( ' + this.structureExpression + ' )';
    }
}

export class LocalDeclarationStructureExpression extends Expression implements Structure {
// let strdec in exp1; ...; expn end
// A sequential expression exp1; ... ; expn is represented as such,
// despite the potentially missing parentheses
    constructor(public declaration: Declaration,
                public expression: Expression & Structure) { super(); }

    simplify(): LocalDeclarationStructureExpression {
        return new LocalDeclarationStructureExpression(this.declaration.simplify(),
            <Expression & Structure> this.expression.simplify());
    }

    elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [StaticBasis, Warning[], Map<string, [Type, boolean]>, string] {
        let nstate = state.getNestedState(state.id);
        tyVarBnd.forEach((val: [Type, boolean], key: string) => {
            if (key[1] === '*' && key[2] === '*') {
                nstate.setStaticValue(key.substring(3),
                    val[0].instantiate(state, tyVarBnd), IdentifierStatus.VALUE_VARIABLE);
            }
        });

        let res = this.declaration.elaborate(nstate, tyVarBnd, nextName, paramBindings);
        nextName = res[3];

        let nbnds = new Map<string, [Type, boolean]>();
        tyVarBnd.forEach((val: [Type, boolean], key: string) => {
            nbnds = nbnds.set(key, [val[0].instantiate(res[0], res[2]), val[1]]);
        });

        let r2 = this.expression.elaborate(res[0], res[2], nextName, paramBindings);
        return [r2[0], res[1].concat(r2[1]), r2[2], r2[3]];
    }

    toString(): string {
        return 'let ' + this.declaration + ' in ' + this.expression + ' end';
    }

    computeStructure(params: EvaluationParameters, callStack: EvaluationStack, recCall: Declaration):
        DynamicBasis | Value | undefined {
        let state = params.state;
        if (params.ldseRes === undefined) {
            if (params.recResult === undefined) {
                let nstate = state.getNestedState(0).getNestedState(state.id);

                callStack.push({'next': recCall, 'params': params});

                callStack.push({
                    'next': this.declaration,
                    'params': {'state': nstate, 'modifiable': params.modifiable, 'recResult': undefined}
                });
                return undefined;
            }
            params.ldseRes = params.recResult;
            params.recResult = undefined;
        }
        let res = <EvaluationResult> params.ldseRes;
        if (res === undefined
            || res.newState === undefined) {
                throw new InternalInterpreterError('Anpan.');
        }
        // braced so linter does not complain about shadowing
        {
            let nstate = <State> res.newState;
            if (res.hasThrown) {
                return <Value> res.value;
            }

            // we have to fake the state in order for the recursion to work correctly
            params.state = nstate;
            let nres = this.expression.computeStructure(params, callStack, recCall);
            params.state = state;

            return nres;
        }
    }
}


// Signature Expressions

export interface Signature {
    computeInterface(state: State): DynamicInterface;
    elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [StaticBasis, Warning[], Map<string, [Type, boolean]>, string];
}

export class SignatureExpression extends Expression implements Signature {
// sig spec end
    constructor(public specification: Specification) {
        super();
    }

    simplify(): SignatureExpression {
        return new SignatureExpression(this.specification.simplify());
    }

    elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [StaticBasis, Warning[], Map<string, [Type, boolean]>, string] {
        return this.specification.elaborate(state, tyVarBnd, nextName, paramBindings);
    }

    computeInterface(state: State): DynamicInterface {
        return this.specification.computeInterface(state);
    }

    toString(): string {
        return 'sig ' + this.specification + ' end';
    }
}

export class SignatureIdentifier extends Expression implements Signature {
// sigid
    constructor(public identifier: Token) {
        super();
    }

    simplify(): SignatureIdentifier {
        return this;
    }

    elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [StaticBasis, Warning[], Map<string, [Type, boolean]>, string] {
        let res: StaticBasis | undefined = undefined;
        if (this.identifier instanceof LongIdentifierToken) {
            let st = state.getAndResolveStaticStructure(<LongIdentifierToken> this.identifier);

            if (st !== undefined) {
                res = (<StaticBasis> st).getSignature(
                    (<LongIdentifierToken> this.identifier).id.getText());
            }
        } else {
            res = state.getStaticSignature(this.identifier.getText());
        }

        if (res === undefined) {
            throw new EvaluationError('Undefined signature "'
                + this.identifier.getText() + '".');
        }
        return [<StaticBasis> res, [], tyVarBnd, nextName];

    }

    computeInterface(state: State): DynamicInterface {
        let res: DynamicInterface | undefined = undefined;
        if (this.identifier instanceof LongIdentifierToken) {
            let st = state.getAndResolveDynamicStructure(<LongIdentifierToken> this.identifier);

            if (st !== undefined) {
                res = (<DynamicBasis> st).getSignature(
                    (<LongIdentifierToken> this.identifier).id.getText());
            }
        } else {
            res = state.getDynamicSignature(this.identifier.getText());
        }

        if (res === undefined) {
            throw new EvaluationError('Undefined signature "'
                + this.identifier.getText() + '".');
        }
        return <DynamicInterface> res;
    }

    toString(): string {
        return this.identifier.getText();
    }
}

export class TypeRealisation extends Expression implements Signature {
// sigexp where type tyvarseq longtycon = ty
    constructor(public signatureExpression: Expression & Signature,
                public tyvarseq: TypeVariable[], public name: Token,
                public type: Type) {
        super();
    }

    simplify(): TypeRealisation {
        return new TypeRealisation(<Expression & Signature> this.signatureExpression.simplify(),
            this.tyvarseq, this.name, this.type.simplify());
    }

    elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [StaticBasis, Warning[], Map<string, [Type, boolean]>, string] {
        let sig = this.signatureExpression.elaborate(state, tyVarBnd, nextName, paramBindings);

        let tmpst = state.getNestedState(0);
        tmpst.staticBasis = sig[0];

        let res: TypeInformation | undefined = undefined;
        let st: StaticBasis | undefined = sig[0];
        let nm = this.name.getText();
        if (this.name instanceof LongIdentifierToken) {
            st = tmpst.getAndResolveStaticStructure(<LongIdentifierToken> this.name);

            if (st !== undefined) {
                res = (<StaticBasis> st).getType(
                    (<LongIdentifierToken> this.name).id.getText());
                nm = (<LongIdentifierToken> this.name).id.getText();
            }
        } else {
            res = tmpst.getStaticType(this.name.getText());
        }

        if (res === undefined || st === undefined) {
            throw new ElaborationError('Undefined type "'
                + this.name.getText() + '".');
        }

        // TODO: TEST THIS!
        st.setType(nm, new FunctionType(res.type, this.type), [],
            this.tyvarseq.length, res.allowsEquality);
        return sig;
    }

    computeInterface(state: State): DynamicInterface {
        return this.signatureExpression.computeInterface(state);
    }

    toString(): string {
        return this.signatureExpression + ' where type <stuff> ' + this.name.getText()
            + ' = ' + this.type;
    }
}


// Module declarations and bindings

// Sutrcture declaration

export class StructureDeclaration extends Declaration {
// structure strbind
    constructor(public structureBinding: StructureBinding[]) {
        super();
    }

    elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>, isTopLevel: boolean):
        [State, Warning[], Map<string, [Type, boolean]>, string] {
        let warns: Warning[] = [];
        for (let i = 0; i < this.structureBinding.length; ++i) {
            let tmp = this.structureBinding[i].elaborate(state, tyVarBnd, nextName, paramBindings);
            state = tmp[0];
            warns = warns.concat(tmp[1]);
            tyVarBnd = tmp[2];
            nextName = tmp[3];
        }
        return [state, warns, tyVarBnd, nextName];
    }

    evaluate(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        if (params.whichBind === undefined) {
            params.whichBind = 0;
        }

        let whichBind: number = params.whichBind;

        let tmp = this.structureBinding[whichBind].evaluate(params, callStack, this);
        if (tmp !== undefined) {
            if (tmp.hasThrown) {
                return tmp;
            }
            params.state = <State> tmp.newState;
            params.whichBind = params.whichBind + 1;
            params.recResult = undefined;

            if (params.whichBind === this.structureBinding.length) {
                return {
                    'newState': params.state,
                    'value': undefined,
                    'hasThrown': false,
                };
            }

            callStack.push({'next': this, 'params': params});
        }
        return undefined;
    }

    simplify(): StructureDeclaration {
        let bnd: StructureBinding[] = [];
        for (let i = 0; i < this.structureBinding.length; ++i) {
            bnd.push(this.structureBinding[i].simplify());
        }
        return new StructureDeclaration(bnd);
    }

    toString(): string {
        let res = 'structure';
        for (let i = 0; i < this.structureBinding.length; ++i) {
            res += ' ' + this.structureBinding[i];
        }
        return res + ';';
    }
}

export class SignatureDeclaration extends Declaration {
// signature sigbind
    constructor(public signatureBinding: SignatureBinding[]) {
        super();
    }

    elaborate(state: State,
              tyVarBnd: Map<string, [Type, boolean]> = new Map<string, [Type, boolean]>(),
              nextName: string = '\'*t0',
              paramBindings: Map<string, Type> = new Map<string, Type>()):
              [State, Warning[], Map<string, [Type, boolean]>, string] {
        let warns: Warning[] = [];
        for (let i = 0; i < this.signatureBinding.length; ++i) {
            let tmp = this.signatureBinding[i].elaborate(state, tyVarBnd, nextName, paramBindings);
            state = tmp[0];
            warns = warns.concat(tmp[1]);
            tyVarBnd = tmp[2];
            nextName = tmp[3];
        }
        return [state, warns, tyVarBnd, nextName];
    }

    evaluate(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        let state = params.state;
        for (let i = 0; i < this.signatureBinding.length; ++i) {
            state = this.signatureBinding[i].evaluate(state);
        }
        return {
            'newState': state,
            'value': undefined,
            'hasThrown': false,
        };
    }

    simplify(): SignatureDeclaration {
        let bnd: SignatureBinding[] = [];
        for (let i = 0; i < this.signatureBinding.length; ++i) {
            bnd.push(this.signatureBinding[i].simplify());
        }
        return new SignatureDeclaration(bnd);
    }

    toString(): string {
        let res = 'structure';
        for (let i = 0; i < this.signatureBinding.length; ++i) {
            res += ' ' + this.signatureBinding[i];
        }
        return res + ';';
    }
}

export class FunctorDeclaration extends Declaration {
// functor funbind
    constructor(public functorBinding: FunctorBinding[]) {
        super();
    }

    elaborate(state: State,
              tyVarBnd: Map<string, [Type, boolean]> = new Map<string, [Type, boolean]>(),
              nextName: string = '\'*t0',
              paramBindings: Map<string, Type> = new Map<string, Type>()):
              [State, Warning[], Map<string, [Type, boolean]>, string] {

        let warns: Warning[] = [];
        for (let i = 0; i < this.functorBinding.length; ++i) {
            let tmp: Warning[] = [];
            [state, tmp, tyVarBnd, nextName] =
                this.functorBinding[i].elaborate(state, tyVarBnd, nextName, paramBindings);
            warns = warns.concat(tmp);
        }

        return [state, warns, tyVarBnd, nextName];
    }

    evaluate(params: EvaluationParameters, callStack: EvaluationStack): EvaluationResult {
        let state = params.state;
        for (let i = 0; i < this.functorBinding.length; ++i) {
            state = this.functorBinding[i].evaluate(state);
        }
        return {
            'newState': state,
            'value': undefined,
            'hasThrown': false,
        };
    }

    simplify(): FunctorDeclaration {
        let bnd: FunctorBinding[] = [];
        for (let i = 0; i < this.functorBinding.length; ++i) {
            bnd.push(this.functorBinding[i].simplify());
        }
        return new FunctorDeclaration(bnd);
    }

    toString(): string {
        let res = 'functor';
        for (let i = 0; i < this.functorBinding.length; ++i) {
            res += ' ' + this.functorBinding[i];
        }
        return res + ';';
    }
}


export class StructureBinding {
// strid = strexp
    constructor(public name: IdentifierToken,
                public binding: Expression & Structure) {
    }

    simplify(): StructureBinding {
        return new StructureBinding(this.name,
            <Expression & Structure> this.binding.simplify());
    }

    elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [State, Warning[], Map<string, [Type, boolean]>, string] {
        let tmp = this.binding.elaborate(state, tyVarBnd, nextName, paramBindings);
        state.setStaticStructure(this.name.getText(), tmp[0]);

        return [state, tmp[1], tmp[2], tmp[3]];
    }

    evaluate(params: EvaluationParameters, callStack: EvaluationStack, recCall: Declaration): EvaluationResult {
        let tmp = this.binding.computeStructure(params, callStack, recCall);
        if (tmp === undefined) {
            return undefined;
        }
        let state = params.state;
        if (tmp instanceof Value) {
            return {
                'newState': state,
                'value': <Value> tmp,
                'hasThrown': true,
            };
        }

        state.setDynamicStructure(this.name.getText(), <DynamicBasis> tmp);
        return {
            'newState': state,
            'value': undefined,
            'hasThrown': false,
        };
    }

    toString(): string {
        return this.name.getText() + ' = ' + this.binding;
    }
}

export class SignatureBinding {
// sigid = sigexp
    constructor(public name: IdentifierToken,
                public binding: Expression & Signature) {
    }

    simplify(): SignatureBinding {
        return new SignatureBinding(this.name,
            <Expression & Signature> this.binding.simplify());
    }

    elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [State, Warning[], Map<string, [Type, boolean]>, string] {
        let res = this.binding.elaborate(state, tyVarBnd, nextName, paramBindings);
        state.setStaticSignature(this.name.getText(), res[0]);
        return [state, res[1], res[2], res[3]];
    }

    evaluate(state: State): State {
        state.setDynamicSignature(this.name.getText(), this.binding.computeInterface(state));
        return state;
    }

    toString(): string {
        return this.name.getText() + ' = ' + this.binding;
    }
}

export class FunctorBinding {
// funid ( strid : sigexp ) = strexp
    constructor(public name: IdentifierToken,
                public signatureName: IdentifierToken,
                public signatureBinding: Expression & Signature,
                public binding: Expression & Structure) {
    }

    simplify(): FunctorBinding {
        return new FunctorBinding(this.name, this.signatureName,
            <Expression & Signature> this.signatureBinding.simplify(),
            <Expression & Structure> this.binding.simplify());
    }

    elaborate(state: State,
              tyVarBnd: Map<string, [Type, boolean]> = new Map<string, [Type, boolean]>(),
              nextName: string = '\'*t0',
              paramBindings: Map<string, Type> = new Map<string, Type>()):
             [State, Warning[], Map<string, [Type, boolean]>, string] {

        let sig = this.signatureBinding.elaborate(state, tyVarBnd, nextName, paramBindings);
        let nstate = state.getNestedState(state.id);
        nstate.setStaticStructure(this.signatureName.getText(), sig[0]);

        // Dirty hacks time: Check if the binding is a local declaration expression; it it
        // is, we set a special flag in the functor that always opens the structure given
        // as a parameter.

        let openParameter = false;
        let warns: Warning[] = [];
        if (this.binding instanceof LocalDeclarationStructureExpression) {
            openParameter = true;
            warns.push(new Warning(0, 'Functor "' + this.name.getText()
                                   + '" elaborated approximately.\n'));
        }

        let str = this.binding.elaborate(nstate, sig[2], sig[3], paramBindings);
        state.setStaticFunctor(this.name.getText(), [sig[0], str[0],
            this.signatureName.getText()], openParameter);

        return [state, sig[1].concat(warns).concat(str[1]), str[2], str[3]];
    }

    evaluate(state: State): State {
        let inter = this.signatureBinding.computeInterface(state);
        let nstate = getInitialState().getNestedState(state.id);
        nstate.dynamicBasis = state.getDynamicChanges(-1);

        state.setDynamicFunctor(this.name.getText(),
            new DynamicFunctorInformation(this.signatureName, inter, this.binding, nstate));
        return state;
    }

    toString(): string {
        return this.name.getText() + '( ' + this.signatureName + ' : ' + this.signatureBinding
            + ' ) = ' + this.binding;
    }
}



// Specifications

export abstract class Specification {
    abstract elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
                       paramBindings: Map<string, Type>):
        [StaticBasis, Warning[], Map<string, [Type, boolean]>, string];
    abstract computeInterface(state: State): DynamicInterface;

    simplify(): Specification {
        return this;
    }
}

export class ValueSpecification extends Specification {
// val valdesc
    constructor(public valueDescription: [IdentifierToken, Type][]) {
        super();
    }

    elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [StaticBasis, Warning[], Map<string, [Type, boolean]>, string] {
        let res = new StaticBasis({}, {}, {}, {}, {});
        for (let i = 0; i < this.valueDescription.length; ++i) {
            let tp = this.valueDescription[i][1].simplify().instantiate(state, tyVarBnd);
            let tyv = tp.getTypeVariables();

            tyv.forEach((dom: Type[], val: string) => {
                tp = new TypeVariableBind(val, tp, dom);
            });

            res.setValue(this.valueDescription[i][0].getText(), tp, IdentifierStatus.VALUE_VARIABLE);
        }
        return [res, [], tyVarBnd, nextName];
    }

    computeInterface(state: State): DynamicInterface {
        let res: DynamicValueInterface = {};
        for (let i = 0; i < this.valueDescription.length; ++i) {
            res[this.valueDescription[i][0].getText()] = IdentifierStatus.VALUE_VARIABLE;
        }
        return new DynamicInterface({}, res, {});
    }
}

export class TypeSpecification extends Specification {
// type [tyvarseq tycon][]
    constructor(public typeDescription: [TypeVariable[], IdentifierToken][]) {
        super();
    }

    elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [StaticBasis, Warning[], Map<string, [Type, boolean]>, string] {
        let res = new StaticBasis({}, {}, {}, {}, {});
        for (let i = 0; i < this.typeDescription.length; ++i) {
            res.setType(this.typeDescription[i][1].getText(),
                new CustomType(this.typeDescription[i][1].getText(), this.typeDescription[i][0]),
                [], this.typeDescription[i][0].length, false);
        }
        return [res, [], tyVarBnd, nextName];
    }

    computeInterface(state: State): DynamicInterface {
        let res: DynamicTypeInterface = {};
        for (let i = 0; i < this.typeDescription.length; ++i) {
            res[this.typeDescription[i][1].getText()] = [];
        }
        return new DynamicInterface(res, {}, {});
    }
}

export class EqualityTypeSpecification extends Specification {
// eqtype [tyvarseq tycon][]
    constructor(public typeDescription: [TypeVariable[], IdentifierToken][]) {
        super();
    }

    elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [StaticBasis, Warning[], Map<string, [Type, boolean]>, string] {
        let res = new StaticBasis({}, {}, {}, {}, {});
        for (let i = 0; i < this.typeDescription.length; ++i) {
            res.setType(this.typeDescription[i][1].getText(),
                new CustomType(this.typeDescription[i][1].getText(), this.typeDescription[i][0]),
                [], this.typeDescription[i][0].length, true);
        }
        return [res, [], tyVarBnd, nextName];
    }

    computeInterface(state: State): DynamicInterface {
        let res: DynamicTypeInterface = {};
        for (let i = 0; i < this.typeDescription.length; ++i) {
            res[this.typeDescription[i][1].getText()] = [];
        }
        return new DynamicInterface(res, {}, {});
    }
}

export class DatatypeSpecification extends Specification {
// datatype [tyvarseq tycon = [vid <of ty>][]][]
    constructor(public datatypeDescription: [TypeVariable[],
        IdentifierToken, [IdentifierToken, Type|undefined][]][]) {
        super();
    }

    elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [StaticBasis, Warning[], Map<string, [Type, boolean]>, string] {
        let res = new StaticBasis({}, {}, {}, {}, {});

        for (let i = 0; i < this.datatypeDescription.length; ++i) {
            let ctr: string[] = [];
            let ctp = new CustomType(this.datatypeDescription[i][1].getText(),
                this.datatypeDescription[i][0]);
            for (let j = 0; j < this.datatypeDescription[i][2].length; ++j) {
                ctr.push(this.datatypeDescription[i][2][j][0].getText());
                if (this.datatypeDescription[i][2][j][1] === undefined) {
                    let ntp: Type = ctp;
                    ntp.getTypeVariables().forEach((val: Type[], name: string) => {
                        ntp = new TypeVariableBind(name, ntp, val);
                    });
                    res.setValue(this.datatypeDescription[i][2][j][0].getText(), ntp,
                    IdentifierStatus.VALUE_CONSTRUCTOR);
                } else {
                    let ntp: Type = new FunctionType(<Type> this.datatypeDescription[i][2][j][1], ctp);
                    ntp.getTypeVariables().forEach((val: Type[], name: string) => {
                        ntp = new TypeVariableBind(name, ntp, val);
                    });
                    res.setValue(this.datatypeDescription[i][2][j][0].getText(),
                        ntp, IdentifierStatus.VALUE_CONSTRUCTOR);
                }
            }
            res.setType(this.datatypeDescription[i][1].getText(), ctp, ctr,
                this.datatypeDescription[i][0].length, false);
        }

        return [res, [], tyVarBnd, nextName];
    }

    computeInterface(state: State): DynamicInterface {
        let vi: DynamicValueInterface = {};
        let ti: DynamicTypeInterface = {};

        for (let i = 0; i < this.datatypeDescription.length; ++i) {
            let cns: string[] = [];
            for (let j = 0; j < this.datatypeDescription[i][2].length; ++j) {
                vi[this.datatypeDescription[i][2][j][0].getText()]
                    = IdentifierStatus.VALUE_CONSTRUCTOR;
                cns.push(this.datatypeDescription[i][2][j][0].getText());
            }
            ti[this.datatypeDescription[i][1].getText()] = cns;
        }
        return new DynamicInterface(ti, vi, {});
    }

    simplify(): DatatypeSpecification {
        let dds: [TypeVariable[], IdentifierToken, [IdentifierToken, Type|undefined][]][] = [];

        for (let i = 0; i < this.datatypeDescription.length; ++i) {
            let cds: [IdentifierToken, Type|undefined][] = [];
            for (let j = 0; j < this.datatypeDescription[i][2].length; ++j) {
                if (this.datatypeDescription[i][2][j][1] === undefined) {
                    cds.push(this.datatypeDescription[i][2][j]);
                } else {
                    cds.push([this.datatypeDescription[i][2][j][0],
                        (<Type> this.datatypeDescription[i][2][j][1]).simplify()]);
                }
            }
            dds.push([this.datatypeDescription[i][0], this.datatypeDescription[i][1], cds]);
        }

        return new DatatypeSpecification(dds);
    }
}

export class DatatypeReplicationSpecification extends Specification {
// datatype tycon = datatype longtycon
    constructor(public name: IdentifierToken, public oldname: Token) {
        super();
    }

    elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [StaticBasis, Warning[], Map<string, [Type, boolean]>, string] {
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
            throw new ElaborationError('The datatype "' + this.oldname.getText()
                + '" doesn\'t exist.');
        }

        let tp = res.type.instantiate(state, tyVarBnd);

        let stbas = new StaticBasis({}, {}, {}, {}, {});
        stbas.setType(this.name.getText(), new FunctionType(new CustomType(this.name.getText(),
            (<CustomType> tp).typeArguments, (this.oldname instanceof LongIdentifierToken)
            ? this.oldname : undefined), tp), [], res.arity, res.allowsEquality);
        return [stbas, [], tyVarBnd, nextName];
    }

    computeInterface(state: State): DynamicInterface {
        let tp: string[] | undefined = undefined;
        if (this.oldname instanceof LongIdentifierToken) {
            let tmp = state.getAndResolveDynamicStructure(this.oldname);
            if (tmp !== undefined) {
                tp = tmp.getType((<LongIdentifierToken> this.oldname).id.getText());
            }
        } else {
            tp = state.getDynamicType(this.oldname.getText());
        }

        if (tp === undefined) {
            throw new EvaluationError('The datatype "'
                + this.oldname.getText() + '" does not exist.');
        }

        let vi: DynamicValueInterface = {};
        for (let i = 0; i < (<string[]> tp).length; ++i) {
            vi[(<string[]> tp)[i]] = IdentifierStatus.VALUE_CONSTRUCTOR;
        }
        let ti: DynamicTypeInterface = {};
        ti[this.name.getText()] = <string[]> tp;
        return new DynamicInterface(ti, vi, {});
    }
}

export class ExceptionSpecification extends Specification {
// exception [vid <of ty>][]
    constructor(public exceptionDescription: [IdentifierToken, Type|undefined][]) {
        super();
    }

    elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [StaticBasis, Warning[], Map<string, [Type, boolean]>, string] {
        let stbas = new StaticBasis({}, {}, {}, {}, {});

        for (let i = 0; i < this.exceptionDescription.length; ++i) {
            if (this.exceptionDescription[i][1] !== undefined) {
                let tp = (<Type> this.exceptionDescription[i][1]).simplify().instantiate(
                    state, new Map<string, [Type, boolean]>());
                let tyvars: string[] = [];
                tp.getTypeVariables().forEach((dom: Type[], val: string) => {
                    tyvars.push(val);
                });
                if (tyvars.length > 0) {
                    throw ElaborationError.getUnguarded(tyvars);
                }

                stbas.setValue(this.exceptionDescription[i][0].getText(),
                    new FunctionType(tp, new CustomType('exn')).normalize()[0],
                    IdentifierStatus.EXCEPTION_CONSTRUCTOR);
            } else {
                stbas.setValue(this.exceptionDescription[i][0].getText(),
                    new CustomType('exn').normalize()[0], IdentifierStatus.EXCEPTION_CONSTRUCTOR);
            }
        }

        return [stbas, [], tyVarBnd, nextName];
    }

    computeInterface(state: State): DynamicInterface {
        let res: DynamicValueInterface = {};
        for (let i = 0; i < this.exceptionDescription.length; ++i) {
            res[this.exceptionDescription[i][0].getText()] = IdentifierStatus.EXCEPTION_CONSTRUCTOR;
        }
        return new DynamicInterface({}, res, {});
    }

    simplify(): ExceptionSpecification {
        let exns: [IdentifierToken, Type|undefined][] = [];
        for (let i = 0; i < this.exceptionDescription.length; ++i) {
            if (this.exceptionDescription[i][1] === undefined) {
                exns.push(this.exceptionDescription[i]);
            } else {
                exns.push([this.exceptionDescription[i][0],
                    (<Type> this.exceptionDescription[i][1]).simplify()]);
            }
        }

        return new ExceptionSpecification(exns);
    }
}

export class StructureSpecification extends Specification {
// structure [strid: sigexp][]
    constructor(public structureDescription: [IdentifierToken, Expression & Signature][]) {
        super();
    }

    elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [StaticBasis, Warning[], Map<string, [Type, boolean]>, string] {

        let stbas = new StaticBasis({}, {}, {}, {}, {});
        let warns: Warning[] = [];
        let tv = tyVarBnd;
        let nN = nextName;

        for (let i = 0; i < this.structureDescription.length; ++i) {
            let sig = this.structureDescription[i][1].elaborate(state, tv, nN, paramBindings);
            warns.concat(sig[1]);
            tv = sig[2];
            nN = sig[3];
            stbas.setStructure(this.structureDescription[i][0].getText(), sig[0]);
        }

        // TODO: Test this!
        return [stbas, warns, tv, nN];
    }

    computeInterface(state: State): DynamicInterface {
        let res: DynamicStructureInterface = {};
        for (let i = 0; i < this.structureDescription.length; ++i) {
            res[this.structureDescription[i][0].getText()] = this.structureDescription[i][1].computeInterface(state);
        }
        return new DynamicInterface({}, {}, res);
    }

    simplify(): StructureSpecification {
        let res: [IdentifierToken, Expression & Signature][] = [];
        for (let i = 0; i < this.structureDescription.length; ++i) {
            res.push([this.structureDescription[i][0],
                <Expression & Signature> this.structureDescription[i][1].simplify()]);
        }
        return new StructureSpecification(res);
    }
}

export class IncludeSpecification extends Specification {
// include sigexp
    constructor(public expression: (Expression & Signature)[]) {
        super();
    }

    elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [StaticBasis, Warning[], Map<string, [Type, boolean]>, string] {
        let res = new StaticBasis({}, {}, {}, {}, {});
        let warns: Warning[] = [];
        for (let i = 0; i < this.expression.length; ++i) {
            let tmp = this.expression[i].elaborate(state, tyVarBnd, nextName, paramBindings);
            res = res.extend(tmp[0]);
            warns = warns.concat(tmp[1]);
            tyVarBnd = tmp[2];
            nextName = tmp[3];
        }
        return [res, warns, tyVarBnd, nextName];
    }

    computeInterface(state: State): DynamicInterface {
        let res = new DynamicInterface({}, {}, {});
        for (let i = 0; i < this.expression.length; ++i) {
            res = res.extend(this.expression[i].computeInterface(state));
        }
        return res;
    }

    simplify(): IncludeSpecification {
        let res: (Expression & Signature)[] = [];
        for (let i = 0; i < this.expression.length; ++i) {
            res.push(<Expression & Signature> this.expression[i].simplify());
        }
        return new IncludeSpecification(res);
    }
}

export class EmptySpecification extends Specification {
//
    constructor() {
        super();
    }

    elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [StaticBasis, Warning[], Map<string, [Type, boolean]>, string] {
        return [new StaticBasis({}, {}, {}, {}, {}), [], tyVarBnd, nextName];
    }

    computeInterface(state: State): DynamicInterface {
        return new DynamicInterface({}, {}, {});
    }
}

export class SequentialSpecification extends Specification {
// spec[]
    constructor(public specifications: Specification[]) {
        super();
    }

    elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [StaticBasis, Warning[], Map<string, [Type, boolean]>, string] {
        let res = new StaticBasis({}, {}, {}, {}, {});
        let warns: Warning[] = [];
        let nstate = state;
        for (let i = 0; i < this.specifications.length; ++i) {
            let tmp = this.specifications[i].elaborate(nstate, tyVarBnd, nextName, paramBindings);
            res = res.extend(tmp[0]);
            warns = warns.concat(tmp[1]);
            tyVarBnd = tmp[2];
            nextName = tmp[3];
            nstate = nstate.getNestedState(nstate.id);
            nstate.staticBasis = tmp[0];
        }
        return [res, warns, tyVarBnd, nextName];
    }

    computeInterface(state: State): DynamicInterface {
        let res = new DynamicInterface({}, {}, {});
        for (let i = 0; i < this.specifications.length; ++i) {
            res = res.extend(this.specifications[i].computeInterface(state));
        }
        return res;
    }

    simplify(): SequentialSpecification {
        let res: Specification[] = [];
        for (let i = 0; i < this.specifications.length; ++i) {
            res.push(this.specifications[i].simplify());
        }
        return new SequentialSpecification(res);
    }
}

export class SharingSpecification extends Specification {
// spec sharing type longtycon = ... = longtycon
    constructor(public specification: Specification,
                public typeNames: Token[]) {
        super();
    }

    elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [StaticBasis, Warning[], Map<string, [Type, boolean]>, string] {
        let spec = this.specification.elaborate(state, tyVarBnd, nextName, paramBindings);

        let tmpst = state.getNestedState(0);
        tmpst.staticBasis = spec[0];

        let tps: TypeInformation[] = [];
        let sts: StaticBasis[] = [];
        let nms: string[] = [];
        let eq = false;
        for (let i = 0; i < this.typeNames.length; ++i) {
            let res: TypeInformation | undefined = undefined;
            let st: StaticBasis | undefined = spec[0];
            let nm = this.typeNames[i].getText();
            if (this.typeNames[i] instanceof LongIdentifierToken) {
                st = tmpst.getAndResolveStaticStructure(<LongIdentifierToken> this.typeNames[i]);

                if (st !== undefined) {
                    res = (<StaticBasis> st).getType(
                        (<LongIdentifierToken> this.typeNames[i]).id.getText());
                    nm = (<LongIdentifierToken> this.typeNames[i]).id.getText();
                }
            } else {
                res = tmpst.getStaticType(this.typeNames[i].getText());
            }

            if (res === undefined || st === undefined) {
                throw new ElaborationError('Undefined type "'
                    + this.typeNames[i].getText() + '".');
            }

            eq = eq || res.allowsEquality;
            tps.push(res);
            sts.push(st);
            nms.push(nm);
        }

        // TODO: TEST THIS!
        for (let i = 1; i < this.typeNames.length; ++i) {
            sts[i].setType(nms[i], new FunctionType(tps[i].type, tps[0].type), [], 0, eq);
        }

        return spec;
    }

    computeInterface(state: State): DynamicInterface {
        return this.specification.computeInterface(state);
    }
}

// Derived forms
export class TypeAliasSpecification extends Specification {
// type tyvarseq tycon = ty and ... and tyvarseq tycon = ty
    constructor(public alias: [TypeVariable[], IdentifierToken, Type][]) {
        super();
    }

    elaborate(state: State, tyVarBnd: Map<string, [Type, boolean]>, nextName: string,
              paramBindings: Map<string, Type>):
        [StaticBasis, Warning[], Map<string, [Type, boolean]>, string] {
        throw new InternalInterpreterError('And you don\'t seem to understand…');
    }

    computeInterface(state: State): DynamicInterface {
        throw new InternalInterpreterError('Being an interpreter is suffering.');
    }

    simplify(): Specification {
        let tpspc: [TypeVariable[], IdentifierToken][] = [];
        for (let i = 0; i < this.alias.length; ++i) {
            tpspc.push([this.alias[i][0], this.alias[i][1]]);
        }
        let sg: Expression & Signature = new SignatureExpression(new TypeSpecification(tpspc));

        for (let i = 0; i < this.alias.length; ++i) {
            sg = new TypeRealisation(sg, this.alias[i][0], this.alias[i][1], this.alias[i][2]);
        }

        return new IncludeSpecification([sg]).simplify();
    }
}
