import { InternalInterpreterError } from './errors';
import { IState as State } from './basic';
import { EvaluationResult, EvaluationStack, Declaration } from './state';

export function evaluate(state: State, ast: Declaration): EvaluationResult {
    let modifiable = state.getNestedState();

    let callStack: EvaluationStack = [];
    callStack.push({'next': ast, 'params': {'state': state, 'modifiable': modifiable, 'recResult': undefined}});

    let lastResult: EvaluationResult = undefined;

    while (callStack.length > 0) {
        let next = callStack.pop();
        if (next === undefined) {
            throw new InternalInterpreterError('How is this undefined?');
        }
        let target = next.next;

        if (next.next === undefined) {
            throw new InternalInterpreterError('I\'ll solve everything by burning it!');
        }

        let params = next.params;
        params.recResult = lastResult;
        if (target instanceof Declaration) {
            lastResult = target.evaluate(params, callStack);
        } else {
            lastResult = target.compute(params, callStack);
        }
    }

    if (lastResult !== undefined) {
        let newState = lastResult.newState;
        if (newState !== undefined) {
            newState.setWarnings(modifiable.getWarnings());
            for (let change of modifiable.getMemoryChanges(state.id)) {
                newState.setCell(change[0], change[1]);
            }
            let idChanges = modifiable.getIdChanges(state.id);
            for (let i in idChanges) {
                if (idChanges.hasOwnProperty(i)) {
                    newState.setValueIdentifierId(i, idChanges[i]);
                }
            }
            newState.exceptionEvalId = modifiable.exceptionEvalId;
        }
    }

    return lastResult;
}
