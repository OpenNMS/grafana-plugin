import { TemplateSrv } from '@grafana/runtime'
import { API } from 'opennms'
import { EntityTypes } from '../../../constants/constants'
import { EntityQuery, EntityQueryRequest } from '../types'

const isAllVariable = (templateVar, templateSrv) => {
    return templateVar.current.value &&
        templateSrv.isAllValue(templateVar.current.value)
}

const isMultiVariable = (templateVar) => {
    return templateVar && templateVar.isMulti
}

const isNumber = (num: any) => {
    return ((parseInt(num, 10) + '') === (num + ''))
}

const isEmptyNodeRestriction = (clause: API.Clause) => {
    const restriction = clause.restriction
    return restriction.attribute === 'node' && restriction.value === '{}'
}

// annoyingly, depending on how you interact with the UI, if one value is selected it will
// *either* be an array with 1 entry, or just the raw value >:|
// so we normalize it back to just the raw value here if necessary
const normalizeSingleArrayValue = (value: any) => {
    if (Array.isArray(value) && value.length === 1) {
        return value[0]
    }

    return value
}

const removeEmptyClauses = (clauses: API.Clause[], remove: API.Clause[]) => {
    for (const r of remove) {
        const i = clauses.indexOf(r)
        if (i >= 0) {
            clauses.splice(i, 1)
        }
    }
}

const simpleVariableSubstitution = (value, variableName, request: EntityQueryRequest<EntityQuery>, templateSrv) => {
    // Range must be of type date, otherwise it is not parseable by the OpenNMS client
    if (variableName === 'range_from') {
        return request.range.from
    } else if (variableName === 'range_to') {
        return request.range.to
    } else {
        return templateSrv.replace(value, request.scopedVars)
    }
}

const subtituteNodeRestriction = (clause: API.Clause) => {
    const restriction = clause.restriction

    // Handle "node" as a special case, updating restrictions to either foreignSource+foreignId or node.id
    if (restriction.attribute === 'node') {
        if (restriction.value.indexOf(':') > 0) {
            if (restriction.comparator.id !== API.Comparators.EQ.id) {
                console.warn('Using a comparator other than EQ will probably not work as expected with a foreignSource:foreignId node criteria.')
            }

            const nodeCriteria = restriction.value.split(':')
            const replacement = new API.NestedRestriction(
                new API.Clause(new API.Restriction('node.foreignSource', restriction.comparator, nodeCriteria[0]), API.Operators.AND),
                new API.Clause(new API.Restriction('node.foreignId', restriction.comparator, nodeCriteria[1]), API.Operators.AND),
            )
            clause.restriction = replacement
        } else if (isNumber(restriction.value)) {
            clause.restriction = new API.Restriction('node.id', restriction.comparator, restriction.value)
        } else {
            console.warn('Found a "node" criteria but it does not appear to be a node ID nor a foreignSource:foreignId tuple.', restriction)
        }
    }
}

// Perform variable substitution for clauses.
// This will be called recursively if clause.restriction is actually a NestedRestriction.
// Note: templateSrv is derived from '@grafana/runtime' TemplateSrv but is actually a class having
// quite a few more methods, etc., so we use 'any' instead. See:
// https://github.com/grafana/grafana/blob/main/public/app/features/templating/template_srv.ts
const substitute = (clauses: API.Clause[], request: EntityQueryRequest<EntityQuery>, templateSrv: any) => {
    const remove: API.Clause[] = []
    const clausesWithRestrictions = clauses.filter(c => c.restriction)

    for (let clause of clausesWithRestrictions) {
        if (clause.restriction instanceof API.NestedRestriction) {
            // this is actually a NestedRestriction, recurse through subclauses
            substitute(clause.restriction.clauses, request, templateSrv)
        } else if (clause.restriction.value) {
            const restriction = clause.restriction as API.Restriction
            const variableName = templateSrv.getVariableName(restriction.value)
            const templateVariable = getTemplateVariable(templateSrv, variableName)

            // Process multi-selects
            if (isMultiVariable(templateVariable) && isAllVariable(templateVariable, templateSrv)) {
                // if we're querying "all" we just dump the clause altogether
                remove.push(clause)
                continue
            }

            if (isMultiVariable(templateVariable)) {
                templateVariable.current.value = normalizeSingleArrayValue(templateVariable.current.value)

                // now if it's *still* an array, we chop it up into nested restrictions
                if (Array.isArray(templateVariable.current.value)) {
                    const replacement = new API.NestedRestriction()
                    let values: [] = templateVariable.current.value

                    for (const value of values) {
                        if (restriction.comparator.id === API.Comparators.EQ.id) {
                            replacement.withOrRestriction(new API.Restriction(restriction.attribute, restriction.comparator, value))
                        } else if (restriction.comparator.id === API.Comparators.NE.id) {
                            replacement.withAndRestriction(new API.Restriction(restriction.attribute, restriction.comparator, value))
                        } else {
                            throw new Error(`Unable to query "${restriction.attribute}": multi-select values with variable substitution must be either "=" or "!="`)
                        }
                    }

                    // we've turned a single restriction into a nested one, so re-process it as a
                    // collection and skip the simple replacement below
                    clause.restriction = replacement
                    substitute(clause.restriction.clauses, request, templateSrv)
                    return
                }
            }

            restriction.value = simpleVariableSubstitution(restriction.value, variableName, request, templateSrv)

            if (isEmptyNodeRestriction(clause)) {
                remove.push(clause)
            } else if (clause.restriction.attribute === 'node') {
                subtituteNodeRestriction(clause)
            }
        }
    }

    removeEmptyClauses(clauses, remove)
}

export const getTemplateVariable = (templateSrv: any, name: string) => {
    const variables = templateSrv.getVariables()

    if (variables && variables.length) {
        return variables.find(v => v.name === name)
    }

    return undefined
}

// Given a query filter, build a cloned one which does template variable substitution, etc.
// Clone Filter to make substitution possible
// (otherwise substitution would happen in original query,
// and overwriting the $<variable> or [[variable]] in restrictions which may not be the intention)
export const buildQueryFilter = (filter: API.Filter, request: EntityQueryRequest<EntityQuery>, templateSrv: TemplateSrv): API.Filter => {
    const clonedFilter = API.Filter.fromJson(filter)

    // Before replacing any variables, add a global time range restriction (which is hidden to the user)
    if (request && request.enforceTimeRange) {
        if (!request.entityType || request.entityType === EntityTypes.Alarms) {
            clonedFilter.withAndRestriction(new API.NestedRestriction()
                .withAndRestriction(new API.Restriction('lastEventTime', API.Comparators.GE, '$range_from'))
                .withAndRestriction(new API.Restriction('lastEventTime', API.Comparators.LE, '$range_to'))
            )
        }
    }

    // Substitute $<variable> or [[variable]] in the restriction value
    substitute(clonedFilter.clauses, request, templateSrv)
    return clonedFilter
}
