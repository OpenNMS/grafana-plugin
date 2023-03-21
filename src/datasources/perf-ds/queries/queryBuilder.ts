import { isEmpty, isNil, isNumber, isObject, isString } from "lodash"
import {
    OnmsMeasurementsQueryRequest,
    OnmsMeasurementsQueryExpression,
    OnmsMeasurementsQueryFilter,
    OnmsMeasurementsQueryFilterParam,
    OnmsMeasurementsQuerySource,
    PerformanceQuery,
    PerformanceQueryFilterStateItem
} from '../types';

export const buildPerformanceMeasurementQuery = (start: number, end: number, step: number, maxRows: number) => {
    return {
        start: start,
        end: end,
        step: step,
        maxrows: maxRows,
        relaxed: true, // enable relaxed mode, which allows for missing attributes
        source: [] as any[],
        expression: [] as any[],
        filter: [] as any[]
    } as OnmsMeasurementsQueryRequest
}

export const getRemoteResourceId = (nodeId: string | number, resourceId: string) => {
    if (resourceId.startsWith('node[') || resourceId.startsWith('nodeSource[')) {
        return resourceId
    }

    const prefix = ('' + nodeId).indexOf(':') >= 0 ? 'nodeSource' : 'node'

    return `${prefix}[${nodeId}].${resourceId}`
}

export const isValidMeasurementQuery = (query: OnmsMeasurementsQueryRequest) => {
    const ok =
        (query.source.length > 0 && query.source[0].attribute && query.source[0].resourceId) ||
        (query.expression.length > 0 && query.expression[0].value) ||
        (query.filter.length > 0 && query.filter[0].name && query.filter[0].parameter)

    return !!ok
}

export const isValidAttributeTarget = (target: PerformanceQuery) => {
    if (!target ||
        target.hide ||
        !(target.attribute &&
          target.attribute.attribute.name &&
          (target.attribute.resource.id || target.attribute.resource.label) &&
          (target.attribute.node.id || target.attribute.node.label))) {
        return false
    }

    return true
}

export const isValidExpressionTarget = (target: PerformanceQuery) => {
    return (!target || target.hide || !(target.label && target.expression)) ? false : true
}

// must specify basic filter info plus any required parameter values in filterState
export const isValidFilterTarget = (target: PerformanceQuery) => {
    if (!target || !target?.filter?.name || target.hide) {
        return false
    }

    const requiredKeys = target.filter.parameter?.filter(p => p.required && p.key).map(p => p.key || '') || []

    if (requiredKeys) {
        // keys in filterState that match required parameters
        const existingKeys = Object.keys(target.filterState).filter(k => requiredKeys.includes(target.filterState[k].filter.key || ''))

        if (!existingKeys || existingKeys.length !== requiredKeys.length) {
            return false
        }

        return existingKeys.every(k => isValidFilterStateItemValue(target.filterState[k]))
    }

    return true
}

// return whether item has some kind of non-empty value
const isValidFilterStateItemValue = (item: PerformanceQueryFilterStateItem) => {
    if (item.value) {
        return (
            (isString(item.value) && item.value.length > 0) ||
            isNumber(item.value) ||
            (isObject(item.value) && !isNil(item.value.value) && ('' + item.value.value).length > 0)
        )
    }

    return false
}

export const buildAttributeQuerySource = (target: PerformanceQuery) => {
    // Note: Have to add 'nodeId' here in case it gets added to 'resourceId' during interpolation,
    // even if the field is removed later after interpolation but before calling the Rest API
    const nodeId = target.attribute.node.id || target.attribute.node.label || ''
    const resourceId = target.attribute.resource.id || target.attribute.resource.label || ''

    const source = {
        label: target.attribute.label || target.attribute.attribute.name,
        nodeId: nodeId,
        resourceId: resourceId.replace('node[', 'nodeSource['),
        attribute: target.attribute.attribute.name,
        ['fallback-attribute']: target.attribute.fallbackAttribute?.name || undefined,
        aggregation: target.attribute.aggregation?.label?.toUpperCase() || 'AVERAGE',
        transient: false
    } as OnmsMeasurementsQuerySource

    return source;
}

export const buildExpressionQuery = (target: PerformanceQuery, index: number) => {
    const expression = {
        label: target.label || 'expression' + index,
        value: target.expression,
        transient: target.hide
    } as OnmsMeasurementsQueryExpression

    return expression
}

export const buildFilterQueryOLD = (target: PerformanceQuery) => {
    const filter = [] as OnmsMeasurementsQueryFilterParam[]

    // TODO: Interpolate the filter params

    for (let [, item] of Object.entries(target.filterState)) {
        const filterItem = item as { value: { value: string }, filter: { key: string } }
        let value: any = filterItem.value
        if (value.value) {
            value = value.value
        }
        if (value) {
            filter.push({ key: filterItem.filter.key, value })
        }
    }

    return {
        parameter: filter,
        name: target.filter.name
    } as OnmsMeasurementsQueryFilter
}


export const buildFilterQuery = (target: PerformanceQuery, interpolatedFilterParams: any[]) => {
    // Shape of interpolatedFilterParams is various entries such as:
    // {
    //     "Input": {
    //         value: "HeapUsageUsed",
    //         filter: {
    //             key: "inputColumn"
    //             // default, description, required, etc.
    //         }
    //     },
    //     "Output": {
    //         value: "HeapUsageUsedPct",
    //         filter: {
    //             key: "outputColumn"
    //         }
    //     }
    // }
    // need to be converted to:
    // [
    //   {
    //     key: "inputColumn",
    //     value: "HeapUsageUsed"
    //   },
    //   {
    //     key: "outputColumn",
    //     value: "HeapUsageUsedPct"
    //   },
    // ...
    // }

    const filters = interpolatedFilterParams.map(filterParams => {
        const parameters: OnmsMeasurementsQueryFilterParam[] = (
            Object.entries(filterParams)
            .filter(([, value]) => !isNil(value) && !isEmpty(value))
            .map(([key, value]) => {
                const v = value as any
                return { key: v.filter?.key || '', value: v.value || '' } as OnmsMeasurementsQueryFilterParam
            })
            .filter(p => p.key && p.value)
        )

        return {
            name: target.filter.name,
            parameter: parameters
        } as OnmsMeasurementsQueryFilter;
    });

    return filters
}
