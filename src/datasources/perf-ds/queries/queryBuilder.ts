import {
    OnmsMeasurementsQueryRequest,
    OnmsMeasurementsQueryExpression,
    OnmsMeasurementsQueryFilter,
    OnmsMeasurementsQueryFilterParam,
    OnmsMeasurementsQuerySource,
    PerformanceQuery,
    PerformanceQueryFilterStateItem
} from '../types'
import { OnmsNode } from 'opennms/src/model/OnmsNode'
import { OpenNMSGlob, isString } from '../../../lib/utils'

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
        !target.attribute ||
        !((target.attribute.attribute?.name || (target.attribute.attribute?.label && OpenNMSGlob.hasGlob(target.attribute.attribute?.label))) &&
          (target.attribute.resource?.id || target.attribute.resource?.label) &&
          (target.attribute.node?.id || target.attribute.node?.label))) {
        return false
    }

    return true
}

export const isValidExpressionTarget = (target: PerformanceQuery) => {
    return (!target || !(target.label && target.expression)) ? false : true
}

// must specify basic filter info plus any required parameter values in filterState
export const isValidFilterTarget = (target: PerformanceQuery) => {
    if (!target || !target?.filter?.name) {
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
            (isString(item.value) && (item.value as string).length > 0) ||
            (typeof(item.value === 'number')) ||
            (
              typeof(item.value) === 'object' &&
              item.value.value &&
              ('' + item.value.value).length > 0
            )
        )
    }

    return false
}

export const buildAttributeQuerySource = (target: PerformanceQuery) => {
    // Note: Have to add 'nodeId' here in case it gets added to 'resourceId' during interpolation,
    // even if the field is removed later after interpolation but before calling the Rest API
    let nodeId = target.attribute.node.id || target.attribute.node.label || ''

    // if node is an OnmsNode and has valid foreign source and foreign id, use that instead
    const onmsNode = target.attribute.node as any as OnmsNode

    if (onmsNode?.foreignSource && onmsNode?.foreignId) {
      nodeId = `${onmsNode.foreignSource}:${onmsNode.foreignId}`
    }

    const resourceId = target.attribute.resource.id || target.attribute.resource.label || ''
    const attribute = target.attribute.attribute.name || target.attribute.attribute.label || ''

    const source = {
        label: target.attribute.label || target.attribute.attribute.name || target.attribute.attribute.label,
        nodeId: nodeId,
        resourceId: resourceId.replace('node[', 'nodeSource['),
        attribute: attribute,
        ['fallback-attribute']: target.attribute.fallbackAttribute?.name || undefined,
        aggregation: target.attribute.aggregation?.label?.toUpperCase() || undefined,
        transient: target.hide === true
    } as OnmsMeasurementsQuerySource

    return source;
}

export const buildExpressionQuery = (target: PerformanceQuery, index: number) => {
    const expression = {
        label: target.label || 'expression' + index,
        value: target.expression,
        transient: target.hide === true
    } as OnmsMeasurementsQueryExpression

    return expression
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
            .filter(([, value]) => value !== null)
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
