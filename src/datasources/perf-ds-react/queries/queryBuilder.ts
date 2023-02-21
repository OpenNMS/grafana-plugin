import { isEmpty, isNil } from "lodash"
import {
    OnmsMeasurementsQueryRequest,
    OnmsMeasurementsQueryExpression,
    OnmsMeasurementsQueryFilter,
    OnmsMeasurementsQueryFilterParam,
    OnmsMeasurementsQuerySource,
    PerformanceQuery
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

export const isValidMeasurementQuery = (query: OnmsMeasurementsQueryRequest) => {
    const ok =
        (query.source.length > 0 && query.source[0].attribute && query.source[0].resourceId) ||
        (query.expression.length > 0 && query.expression[0].value) ||
        (query.filter.length > 0 && query.filter[0].name && query.filter[0].parameter)

    return ok
}

export const isValidAttributeTarget = (target: PerformanceQuery) => {
    if (!target ||
        !(target.attribute &&
          target.attribute.attribute &&
          target.attribute.resource.id &&
          (target.attribute.node.id || target.attribute.node.label))) {
        return false
    }

    return true
}

export const isValidExpressionTarget = (target: PerformanceQuery) => {
    return (!target || !(target.label && target.expression)) ? false : true
}

export const isValidFilterTarget = (target: PerformanceQuery) => {
    return (!target || !target.filter || !target.filter.name) ? false : true
}

export const buildAttributeQuerySource = (target: PerformanceQuery) => {
    const source = {
        label: target.attribute.label || target.attribute.attribute.name,
        resourceId: target.attribute.resource.id.replace('node[', 'nodeSource['),
        attribute: target.attribute.attribute.name,
        ['fallback-attribute']: target.attribute.fallbackAttribute.name,
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
