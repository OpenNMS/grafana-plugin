import { ScopedVars, VariableModel } from '@grafana/data'
import { TemplateSrv } from '@grafana/runtime'
import cloneDeep from 'lodash/cloneDeep'
import { ALL_SELECTION_VALUE } from 'constants/constants'
import { isString } from '../../../lib/utils'

export interface InterpolationArrayVariable {
    name: string;
    value: string[];
}

export interface InterpolationVariable {
    name: string;
    value: string | string[];
}

// Our version of @grafana/data VariableOption
interface TemplateVariableOption {
    value: string | string[];
}

// Our version of @grafana/data VariableWithOptions
interface TemplateSrvVariable {
    name: string;
    current: TemplateVariableOption;
    options: TemplateVariableOption[];
}

/**
 * Collect any template variables that need to be interpolated, also taking into account scoped variables.
 *
 * Note, in latest Grafana, templateSrv.getVariables() items are VariableWithOptions
 * We don't use all the properties of these, and they may not be present in earlier Grafana versions,
 * so we use the above interface definitions to provide some typing information.
 * See: https://github.com/grafana/grafana/blob/main/packages/grafana-data/src/types/templateVars.ts
 */
export const collectInterpolationVariables = (templateSrv: TemplateSrv, scopedVars?: ScopedVars): InterpolationVariable[] => {
    // Reformat the variables to work with our interpolate function
    const variables = [] as InterpolationVariable[];

    templateSrv.getVariables().forEach((templateVariable: VariableModel) => {
        const variable = {
            name: templateVariable.name,
            value: [] as string[]
        } as InterpolationArrayVariable

        // If this templateVar exists in scopedVars, we need to look at the scoped values
        if (scopedVars && scopedVars[variable.name] !== undefined && scopedVars[variable.name] !== null) {
            variable.value = [scopedVars[variable.name]?.value?.toString()];
        } else {
            // TODO: templateSrv.getVariables() in Grafana 8.5 returns VariableModel[],
            // VariableModel does NOT contain 'current' or 'current.value'
            // But latest Grafana, 9.4.0, returns TypedVariableModel[], which is actually
            // instances of DashboardVariableModel, which is SystemVariable<DashboardProps> which
            // DOES contain 'current' and 'current.value'
            const templateSrvVariable = (templateVariable as any) as TemplateSrvVariable
            const currentValue = templateSrvVariable.current.value

            if (isString(currentValue)) {
                // If currentValue is a single-valued string
                variable.value = [currentValue as string]
            } else {
                // If currentValue is a string[]
                (currentValue as string[])?.forEach(value => {
                    if (value === ALL_SELECTION_VALUE) {(
                        templateSrvVariable.options
                        .filter(o => o.value !== ALL_SELECTION_VALUE)
                        .forEach(option => variable.value.push(Array.isArray(option.value) ? (option.value as string[])[0] : option.value as string))
                    )
                    } else {
                        variable.value.push(value);
                    }
                })
            }
        }

        variables.push(variable);
    })

    return variables
}

const defaultContainsVariable = (value: any | undefined, variableName: string) => {
    if (value === null || value === undefined || !isString(value)) {
        return false
    }

    return (value as string).indexOf('$' + variableName) >= 0
}

const defaultReplace = (value: any, variables: InterpolationVariable[]) => {
    if (value === null || value === undefined || value === '') {
        return value;
    }

    let interpolatedValue = value;

    variables.forEach(variable => {
        const regexVarName = "\\$" + variable.name.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");

        interpolatedValue = interpolatedValue.replace(new RegExp(regexVarName, 'g'), variable.value);
    })

    return interpolatedValue;
}

const cartesianProductOfArrays = <T>(arrays: T[][]): T[][] => {
    // Based on the code from http://stackoverflow.com/questions/15298912/
    // javascript-generating-combinations-from-n-arrays-with-m-elements
    const r: T[][] = []
    const max = arrays.length - 1;

    const helper = (arr: T[], i: number) => {
        for (let j = 0, l = arrays[i].length; j < l; j++) {
            const a = arr.slice(0); // clone arr
            a.push(arrays[i][j]);

            if (i === max) {
                r.push(a);
            } else {
                helper(a, i + 1);
            }
        }
    }

    helper([], 0);
    return r;
}

const cartesianProductOfVariables = (variables: InterpolationVariable[]) => {
    // Collect the values from all of the variables
    const allValues: string[][] = variables.map(v => Array.isArray(v.value) ? v.value : [v.value])

    // Generate the cartesian product
    const productOfAllValues = cartesianProductOfArrays<string>(allValues)

    // Rebuild the variables
    const productOfAllVariables = [] as InterpolationVariable[][]

    productOfAllValues.forEach((rowOfValues: string[]) => {
        const rowOfVariables = [] as InterpolationVariable[]

        for (let i = 0, l = variables.length; i < l; i++) {
            const variable = cloneDeep(variables[i])
            variable.value = rowOfValues[i]
            rowOfVariables.push(variable)
        }

        productOfAllVariables.push(rowOfVariables)
    })

    return productOfAllVariables
}

/**
 * Replaces the source's attributes with the values of the referenced variables.
 *
 * If a referenced variable contains multiple values or if there are multiple referenced variables
 * then we generate copies of the source with all of the possible permutations.
 *
 * See interpolate_spec.js for examples.
 *
 * T is actually going to be:
 * - OnmsMeasurementsQuerySource for Attribute queries
 * - OnmsMeasurementsQueryExpression for Expression queries
 * - PerformanceQuery.filterState (just an object) for Filter queries
 *
 * @param object
 *    the object to interpolate
 * @param attributes
 *    a list of attributes on a given object that should be checked for variables
 * @param variables
 *    a list of variables of the form [{name: 'varname', value: ['value1', 'value2']}, ...]
 * @param callback
 *    an optional callback made with the object after variable substitution has been performed
 * @param containsVariable
 *    optionally override the function used to determine if a string contains a reference to the named variable
 * @param replace
 *    optionally override the function used to substitute the variable reference in a string with the variables's value
 * @returns an array of objects, if no substitutions were performed, the array will contain the original object
 */
export const interpolate = <T extends any>(object: T, attributes: string[],
    interpolationVars: InterpolationVariable[],
    callback: (src: T) => void = () => {}): T[] => {

    // Add the index variable with a single value
    const variablesWithIndex = [...interpolationVars]
    variablesWithIndex.push({ name: 'index', value: ['0'] })

    // Collect the list of variables that are referenced by one or more of the keys
    const referencedVariables = [] as InterpolationVariable[]

    variablesWithIndex.forEach(variable => {
        const isVariableReferenced = attributes.find(attribute => defaultContainsVariable(object[attribute], variable.name))

        if (isVariableReferenced) {
            referencedVariables.push(variable)
        }
    })

    if (referencedVariables.length < 1) {
        // No variables are referenced, nothing to substitute
        callback(object)
        return [object]
    }

    // Generate all possible permutations of the referenced variable's values
    const productOfAllVariables = cartesianProductOfVariables(referencedVariables)

    // Perform the required variable substitution
    const objects = [] as T[]
    let index = 0

    productOfAllVariables.forEach((rowOfReferencedVariables: InterpolationVariable[]) => {
        // Update the value of the index variable to reflect the index of the row
        rowOfReferencedVariables.forEach(variable => {
            if (variable.name === 'index') {
                variable.value = 'idx' + index
                index += 1
            }
        })

        const o = { ...(object as any) }

        attributes.forEach(attribute => {
            o[attribute] = defaultReplace(o[attribute], rowOfReferencedVariables)
        })

        callback(o)
        objects.push(o)
    })

    return objects
}
