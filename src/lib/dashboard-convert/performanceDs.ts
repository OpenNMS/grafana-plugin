import { PerformanceTypeOptions } from '../../datasources/perf-ds/constants'
import { PerformanceQuery, StringPropertyQuery } from '../../datasources/perf-ds/types'

export const updatePerformanceQuery = (source: any) => {
  // Note, target.datasource will be set in caller

  if (source.type) {
    if (source.type === 'attribute') {
      return convertAttributeQuery(source)
    } else if (source.type === 'expression') {
      return convertExpressionQuery(source)
    } else if (source.type === 'filter') {
      return convertFilterQuery(source)
    } else if (source.type === 'stringProperty') {
      return convertStringPropertyQuery(source)
    }
  }

  // should not get here
  return source
}

// Convert old-style Attribute Query to new PerformanceQuery with PerformanceAttributeState
// Old:
// {
//   "aggregation": "AVERAGE",
//   "attribute": "loadavg5",
//   "hide": true,
//   "nodeId": "$node",
//   "refId": "A",
//   "resourceId": "nodeSnmp[]",
//   "type": "attribute"
// }
const convertAttributeQuery = (source: any): PerformanceQuery | any => {
  // TODO: Handle where source.datasource is { type, uid }, not a template variable
  const query = {
    datasource: source.datasource,
    hide: source.hide || false,
    key: source.key || '',
    label: source.label || '',
    queryType: source.queryType || '',
    refId: source.refId || '',
    performanceType: PerformanceTypeOptions.Attribute,
    // PerformanceAttributeState
    attribute: {
      node: {
        id: source.nodeId,
        label: source.nodeId
      },
      resource: {
        id: source.resourceId,
        label: source.resourceId
      },
      attribute: {
        name: source.attribute || '',
        label: source.attribute || ''
      },
      subAttribute: source.subAttribute || undefined,
      fallbackAttribute: source.fallbackAttribute || source['fallback-attribute'] || undefined,
      aggregation: {
        label: source.aggregation || ''
      }
    },
    filter: {},
    filterState: {},
    performanceState: {},
    format: source.format || undefined,
    rawSql: source.rawSql || undefined
  }

  // TODO: Figure out "new" representation of these "time_series" attribute queries with "rawSql"
  // {
  //   ...
  //   "format": "time_series",
  //   "hide": true,
  //   "nodeId": "$node",
  //   "rawSql": "SELECT\n  $__time(time_column),\n  value1\nFROM\n  metric_table\nWHERE\n  $__timeFilter(time_column)\n",
  // }

  return query
}

// Convert old-style Expression Query to new PerformanceQuery with expression
// Old:
// {
//   "attribute": "loadavg5Pct",
//   "expression": "loadavg5 / 100",
//   "label": "Load (Avg)",
//   "refId": "B",
//   "type": "expression"
// }
const convertExpressionQuery = (source: any): PerformanceQuery | any => {
  const query = {
    datasource: source.datasource,
    hide: source.hide || false,
    key: source.key || '',
    label: source.label || '',
    queryType: source.queryType || '',
    refId: source.refId || '',
    performanceType: PerformanceTypeOptions.Expression,
    expression: source.expression || '',
    attribute: {},
    filter: {},
    filterState: {},
    performanceState: {}
  }

  if (source.attribute) {
    query.attribute = {
      attribute: {
        name: source.attribute
      }
    }
  }

  return query
}

// Convert old-style Filter Query to new PerformanceQuery format
// Old:
// {
//   "filter": {
//     "backend": "R",
//     "canonicalName": "org.opennms.netmgt.measurements.filters.impl.TrendLine",
//     "description": "Fits a trend line or polynomial to a given column.",
//     "name": "Trend",
//     "parameter": [
//       {
//         "default": null,
//         "description": "Input column.",
//         "displayName": "Input",
//         "key": "inputColumn",
//         "required": true,
//         "type": "string"
//       },
//       ...
//       {
//         "default": "1",
//         "description": "Polynomial order of the trend line/curve. Set this to 1 for a line.",
//         "displayName": "Order",
//         "key": "polynomialOrder",
//         "required": false,
//         "type": "int"
//       }
//     ]
//   },
//   "filterParameters": {
//     "inputColumn": "dskUsed",
//     "outputColumn": "dskTrend",
//     "secondsAhead": "57600"
//   },
//   "refId": "C",
//   "type": "filter"
// }
const convertFilterQuery = (source: any): PerformanceQuery | any => {
  if (source.filter) {
    const query = {
      datasource: source.datasource,
      hide: source.hide || false,
      key: source.key || '',
      label: source.label || '',
      queryType: source.queryType || '',
      refId: source.refId || '',
      performanceType: PerformanceTypeOptions.Filter,
      expression: '',
      attribute: {},
      performanceState: {},
      filter: {
        backend: source.filter.backend,
        canonicalName: source.filter.canonicalName,
        description: source.filter.description,
        label: source.filter.label,
        name: source.filter.name,
        // these seem to have the same contents, see PerformanceQueryFilterParameter
        parameter: [...source.filter.parameter]
      },
      filterState: {}
    }

    if (source.filterParameters) {
      Object.keys(source.filterParameters).forEach(k => {
        query.filterState[k] = {
          filter: {},
          value: source.filterParameters[k]
        }
      })
    }

    return query
  }

  return source
}

const convertStringPropertyQuery = (source: any): StringPropertyQuery | any => {
  // TODO!

  return source
}
