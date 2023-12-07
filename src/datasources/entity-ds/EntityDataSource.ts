import { DataQueryResponse, DataSourceApi, DataSourceInstanceSettings, QueryResultMeta } from '@grafana/data'
import { TemplateSrv, getBackendSrv, getTemplateSrv } from '@grafana/runtime'
import { ClientDelegate } from 'lib/client_delegate'
import { SimpleOpenNMSRequest } from 'lib/simpleRequest'
import { capitalize, extractRawVariableName, getNodeFilterMap, isTemplateVariableCandidate } from 'lib/utils'
import { API, Model } from 'opennms'
import { EntityTypes } from '../../constants/constants'
import { loadFilterEditorData } from '../../lib/localStorageService'
import {
  getColumns,
  getEntityTypeFromFuncName,
  getPropertyComparators,
  getQueryEntityType,
  getSearchProperties,
  getTemplateVariable,
  isLocationQuery,
  isMetricMetadataQuery,
  isSituationAttribute,
  metricFindLocations,
  parseFunctionInfo,
  queryEntity
} from './EntityHelper'
import { getAttributeMapping } from './queries/attributeMappings'
import { buildQueryFilter, mergeFilterPanelFilters } from './queries/queryBuilder'
import {
  EntityDataSourceOptions,
  EntityQuery,
  EntityQueryRequest,
  OnmsEntityFunctionInfo,
  OnmsTableData
} from './types'

export interface OnmsQueryResultMeta extends QueryResultMeta {
  entity_metadata: any[]
}

export class EntityDataSource extends DataSourceApi<EntityQuery> {
    type: string
    url?: string
    name: string
    client: ClientDelegate
    simpleRequest: SimpleOpenNMSRequest
    templateSrv: TemplateSrv

    constructor(instanceSettings: DataSourceInstanceSettings<EntityDataSourceOptions>) {
        super(instanceSettings)
        this.type = instanceSettings.type
        this.url = instanceSettings.url
        this.name = instanceSettings.name
        this.client = new ClientDelegate(instanceSettings, getBackendSrv())
        this.simpleRequest = new SimpleOpenNMSRequest(getBackendSrv(), this.url)
        this.templateSrv = getTemplateSrv()
    }

    async query(request: EntityQueryRequest<EntityQuery>): Promise<DataQueryResponse> {
        const fullData: OnmsTableData[] = []

        // - May need to get dashboard info (via backendSrv or direct via Dashboard HTTP API)
        //   and see if there is actually a FilterPanel in the current dashboard.
        //   Right now just checking for localStorage data, which could
        //   possibly exist even if the FilterPanel had been deleted
        // - FilterPanel should perhaps get events to know when it has been deleted and clear
        //   localStorage. May be able to subscribe to EventBus

        // get data from any FilterPanels that may be active
        const dashboardUid = request.dashboardUID || ''
        const filterEditorData = dashboardUid ? loadFilterEditorData(dashboardUid) : null

        const hasFilterEditorData = (filterEditorData &&
            filterEditorData.activeFilters.length > 0 && filterEditorData.selectableValues.length > 0)

        for (let target of request.targets) {
            request.entityType = target?.selectType?.label || EntityTypes.Alarms
            // possibly should be an option in the panel editor
            request.enforceTimeRange = true

            const filter = buildQueryFilter(target?.filter || new API.Filter(), request, this.templateSrv)

            if (hasFilterEditorData) {
                mergeFilterPanelFilters(request.entityType, filter, filterEditorData)
            }

            try {
                const rowData = await queryEntity(request.entityType, filter, this.client)
                fullData.push(rowData)
            } catch (e) {
                console.error(e)
            }
        }

        return { data: fullData }
    }

    async metricFindQuery(query, options) {
        if (isLocationQuery(query)) {
          return metricFindLocations(this.simpleRequest)
        }

        let entityType = getEntityTypeFromFuncName(options.entityType) || getQueryEntityType(query) || ''

        // this may be an attribute, a mapped attribute, or just the original query
        let queryOrAttribute = getAttributeMapping(entityType, query)

        if (isMetricMetadataQuery(options.queryType)) {
          return this.handleMetricMetadataQuery(entityType, options.queryType, queryOrAttribute, options.strategy)
        }

        if (!queryOrAttribute) {
          console.warn('entity-ds: metricFindQuery: no attribute specified')
          return []
        }

        const info: OnmsEntityFunctionInfo = parseFunctionInfo(queryOrAttribute)
        entityType = info.entityType || entityType

        if (info.funcName === 'nodeFilter') {
          return this.metricFindNodeFilterQuery(entityType, info.attribute, info.labelFormat, info.valueFormat)
        }

        if (isSituationAttribute(info.attribute || queryOrAttribute)) {
            return [
                { id: 'false', label: 'false', text: 'false' },
                { id: 'true', label: 'true', text: 'true' }
            ]
        }

        const searchProperties: API.SearchProperty[] = await getSearchProperties(entityType, this.client)
        const searchProperty: API.SearchProperty = searchProperties.filter(p => p.id === info.attribute)?.at(0)

        if (!searchProperty) {
            return []
        }

        // Severity is handled separately as otherwise the severity ordinal vs the severity label would be
        // used, but that may not be ideal for the user
        if (searchProperty.id === 'severity') {
            const items =
                Object.entries(Model.Severities).map(([k, v]) => {
                    const severity = v as any as Model.OnmsSeverity
                    return {
                        id: severity.i,
                        label: capitalize(severity.l),
                        text: capitalize(severity.l),
                        value: severity.l
                    }
                })

            return items
        }

        const propertyValues = await searchProperty.findValues({ limit: 0 })

        return propertyValues.filter(value => value !== null)
            .map(value => {
                return { id: value, label: value, text: value ? String(value) : value, value: value }
            })
    }

    async testDatasource(): Promise<any> {
        return await this.client.testConnection()
    }

    async metricFindNodeFilterQuery(entityType: string, attribute: string, labelFormat = '', valueFormat = '') {
        const filterMap = getNodeFilterMap(attribute)
        let filter = new API.Filter()

        for (const pair of filterMap) {
            const propertyKey = getAttributeMapping(entityType, pair[0])
            const propertyValue = pair[1]

            if (isTemplateVariableCandidate(propertyValue)) {
                const templateVariable = getTemplateVariable(this.templateSrv, extractRawVariableName(propertyValue))

                if (templateVariable && templateVariable.current.value) {
                    filter.withAndRestriction(new API.Restriction(propertyKey, API.Comparators.EQ, templateVariable.current.value))
                }
            } else if (propertyKey && propertyValue) {
                filter.withAndRestriction(new API.Restriction(propertyKey, API.Comparators.EQ, propertyValue))
            }
        }

        filter.limit = 0
        const nodes = await this.client.getNodeByFilter(filter)

        return nodes.map(node => this.formatNodeFilterResult(node, labelFormat, valueFormat))
    }

    /**
     * Format the results of a 'nodeFilter()' call into an enhanced MetricFindValue object,
     * taking into account any 'labelFormat' or 'valueFormat'.
     * By default, label and value will be the node id.
     * See constants/validNodeValueFormats.ts for format options.
     *
     * @param node The OnmsNode from the Rest API call.
     * @param labelFormat The format for the returned MetricFindValue.text and label (what is displayed to user in template variable dropdown, etc.)
     * @param valueFormat The format for the returned MetricFindValue.value (numeric or string value)
     * @returns An object with { id, label, text, value } which is a superset of Grafana MetricFindValue.
     */
    formatNodeFilterResult(node: Model.OnmsNode, labelFormat = 'id', valueFormat = 'id') {
      const { nodeId, nodeLabel, fsFid } = this.parseNodeFields(node)
      const label = this.formatLabelOrValue(nodeId, nodeLabel, fsFid, labelFormat || 'id')
      const value = this.formatLabelOrValue(nodeId, nodeLabel, fsFid, valueFormat || 'id')

      return { id: node.id, label, text: label, value }
    }

    parseNodeFields(node: Model.OnmsNode) {
      const nodeId = String(node.id || '')
      const nodeLabel = node.label || ''
      const foreignSource = node.foreignSource || ''
      const foreignId = node.foreignId || ''
      const fsFid = `${foreignSource}:${foreignId}`

      return {
        nodeId, nodeLabel, fsFid
      }
    }

    formatLabelOrValue(nodeId: string, nodeLabel: string, fsFid: string, format: string) {
      if (format === 'id') {
        return nodeId
      } else if (format === 'label') {
        return nodeLabel
      } else if (format === 'id:label') {
        return `${nodeId}:${nodeLabel}`
      } else if (format === 'label:id') {
        return `${nodeLabel}:${nodeId}`
      } else if (format === 'fs:fid') {
        return fsFid
      }

      return nodeId
    }

    async handleMetricMetadataQuery(entityType: string, queryType: string, attribute: string, strategy?: string) {
        // special case queries to fill in metadata
        if (queryType === 'attributes') {
            if (strategy && strategy === 'featured') {
                return getColumns(entityType).filter(col => col.featured).map(col => {
                    return { id: col.resource, value: col.text }
                })
            }
            // assume all
            return await getSearchProperties(entityType, this.client)
        } else if (queryType === 'comparators') {
            return await getPropertyComparators(entityType, attribute, this.client)
        } else if (queryType === 'operators') {
            return await this.client.findOperators()
        }

        return []
    }
}
