import { DataQueryResponse, DataSourceApi, DataSourceInstanceSettings, QueryResultMeta } from "@grafana/data";
import { getTemplateSrv } from '@grafana/runtime';
import { EntityTypes } from "./constants";
import { ClientDelegate } from "lib/client_delegate";
import { FunctionFormatter } from "lib/function_formatter";
import { SimpleOpenNMSRequest, getNodeFilterMap } from "lib/utils";
import { API, Model } from "opennms";
import { getAttributeMapping } from "./queries/attributeMappings";
import {
    getColumns,
    getEntityTypeFromFuncName,
    getPropertyComparators,
    getQueryEntityType,
    getSearchProperties,
    isLocationQuery,
    metricFindLocations,
    queryEntity
} from "./EntityHelper";
import { EntityDataSourceOptions, EntityQuery, EntityQueryRequest, OnmsTableData } from "./types";

export interface OnmsQueryResultMeta extends QueryResultMeta {
    entity_metadata: any[];
}

export class EntityDataSource extends DataSourceApi<EntityQuery> {
    type: string;
    url?: string | undefined;
    name: string;
    client: ClientDelegate;
    simpleRequest: SimpleOpenNMSRequest;

    constructor(instanceSettings: DataSourceInstanceSettings<EntityDataSourceOptions>, public backendSrv: any, public templateSrv: any) {
        super(instanceSettings);
        this.type = instanceSettings.type;
        this.url = instanceSettings.url;
        this.name = instanceSettings.name;
        this.client = new ClientDelegate(instanceSettings, backendSrv);
        this.simpleRequest = new SimpleOpenNMSRequest(backendSrv, this.url);
    };

    addVariablesToFilter(filter: API.Filter): API.Filter {
        const templateVariables = getTemplateSrv().getVariables();

        const nodeVariable: any = templateVariables.find((d: any) => d.query.startsWith('nodes('));
        let localFilter = new API.Filter();

        filter.clauses?.forEach((existingFilter: API.Filter) => {
            if (existingFilter.operator.label === 'AND') {
                localFilter.withAndRestriction(new API.Restriction(existingFilter.restriction.attribute, existingFilter.restriction.comparator))
            }
        })

        if (nodeVariable) {
            // get the query attribute, this is the restriction key
            const match = nodeVariable.query.match(/^nodes\((\w+)\)/)

            const matchValue = match ? match[1] : ''
            const restrictionKey = getAttributeMapping(EntityTypes.Nodes, matchValue)
            localFilter.withAndRestriction(new API.Restriction(restrictionKey, API.Comparators.EQ, nodeVariable.current.value))
        }

        templateVariables.filter((d: any) => !d.query.startsWith('node')).forEach((key: any, value) => {
            if (key.query.startsWith('locations')) {
                if (key.current.text) {
                    const locationKey = getAttributeMapping(EntityTypes.Nodes, 'location')
                    localFilter.withAndRestriction(new API.Restriction(locationKey, API.Comparators.EQ, key.current.value))
                }
            }
        })

        return localFilter;
    }

    async query(options: EntityQueryRequest<EntityQuery>): Promise<DataQueryResponse> {
        const fullData: OnmsTableData[] = [];

        for (let target of options.targets) {
            const filter = this.addVariablesToFilter(target?.filter || new API.Filter());

            try {
                const rowData = await queryEntity(target?.selectType?.label, filter, this.client);
                fullData.push(rowData);
            } catch (e) {
                console.error(e);
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
        let attribute = getAttributeMapping(entityType, query)

        if (this._isMetricMetadataQuery(options.queryType)) {
            return this._handleMetricMetadataQuery(entityType, options.queryType, attribute, options.strategy)
        }

        if (!attribute) {
            console.warn('entity-ds-react: metricFindQuery: no attribute specified')
            return Promise.resolve([])
        }

        const info = this._parseFunctionInfo(attribute)
        entityType = info.entityType || entityType
        attribute = info.attribute

        if (info.funcName === 'nodeFilter') {
            return this.metricFindNodeFilterQuery(entityType, attribute)
        }

        if (this._attributeIsSituation(attribute)) {
            return Promise.resolve([{ id: 'false', label: 'false', text: 'false'}, {id: 'true', label: 'true', text: 'true'}])
        }

        const searchProperties: API.SearchProperty[] = await getSearchProperties(entityType, this.client)
        const searchProperty: API.SearchProperty = searchProperties.filter(p => p.id === attribute)?.at(0)

        if (!searchProperty) {
            return Promise.resolve([])
        }

        // Severity is handled separately as otherwise the severity ordinal vs the severity label would be
        // used, but that may not be ideal for the user
        if (searchProperty.id === 'severity') {
            return Promise.resolve(Model.Severities.map(severity => {
                return {
                    id: severity.id,
                    label: severity.label
                }
            }))
        }

        const propertyValues = await searchProperty.findValues({limit: 0})

        return propertyValues.filter(value => value !== null)
            .map(value => {
                return { id: value, label: value, text: value ? String(value) : value, value: value }
        })
    }

    async testDatasource(): Promise<any> {
        console.log('Testing the data source!');

        try {
            const metadata = await this.client.getClientWithMetadata();
            console.log('Testing the data source!', metadata);
        } catch (e) {
            console.log('CAUGHT!', e);
        }
        return { status: 'success', message: 'Success' }
    }

    async metricFindNodeFilterQuery(entityType, attribute) {
        const filtermap = getNodeFilterMap(attribute)
        let filter = new API.Filter()

        for (const pair of filtermap) {
            const propertyKey = getAttributeMapping(entityType, pair[0])
            const propertyValue = pair[1]

            if (propertyValue.startsWith('$')) {
                const variableName = this.templateSrv.getVariableName(propertyValue)
                const templateVariable = this._getTemplateVariable(variableName)

                if (templateVariable && templateVariable.current.value) {
                    filter.withAndRestriction(new API.Restriction(propertyKey, API.Comparators.EQ, templateVariable.current.value))
                }
            } else if (propertyKey && propertyValue) {
                filter.withAndRestriction(new API.Restriction(propertyKey, API.Comparators.EQ, propertyValue))
            }
        }

        filter.limit = 0
        const nodes = await this.client.getNodeByFilter(filter)

        return nodes.map(node => {
            return { id: node.id, label: node.id, text: node.id ? String(node.id) : node.id, value: node.id }
        })
    }

    private _isMetricMetadataQuery(queryType) {
        const metadataQueryTypes = ['attributes', 'comparators', 'operators']

        return metadataQueryTypes.includes(queryType)
    }

    private _getTemplateVariable(name) {
        if (this.templateSrv.variables && this.templateSrv.variables.length > 0) {
            return this.templateSrv.variables.filter((v) => {
                return v.name === name
            })[0]
        }
        return undefined
    }

    private async _handleMetricMetadataQuery(entityType: string, queryType: string, attribute: string, strategy?: string) {
        // special case queries to fill in metadata
        if (queryType === 'attributes') {
            if (strategy && strategy === 'featured') {
                return Promise.resolve(getColumns(entityType).filter(col => col.featured).map(col => {
                    return { id: col.resource, value: col.text }
                }))
            }
            // assume all
            return getSearchProperties(entityType, this.client)
        } else if (queryType === 'comparators') {
            return getPropertyComparators(entityType, attribute, this.client)
        } else if (queryType === 'operators') {
            return this.client.findOperators()
        }

        return Promise.resolve([])
    }

    private _parseFunctionInfo(attribute: string) {
        let entityType = ''
        let funcName = ''
        let attr = attribute

        const functions = FunctionFormatter.findFunctions(attribute)

        for (const func of functions) {
            funcName = func.name
            attr = func.arguments[0] || 'id'
            const e = getEntityTypeFromFuncName(func.name)

            if (e) {
                entityType = e
                break
            }
        }

        return {
            entityType,
            funcName,
            attribute: attr
        }
    }

    private _attributeIsSituation(attribute) {
        return attribute === 'isSituation' ||
            attribute === 'isInSituation' ||
            attribute === 'isAcknowledged'
    }
}
