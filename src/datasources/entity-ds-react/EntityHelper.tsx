import { SelectableValue } from '@grafana/data'
import { API } from 'opennms'
import { EntityTypes } from '../../constants/constants'
import {
    ActiveFilter,
    AlarmProperties,
    IpInterfaceProperties,
    MonitoredServiceProperties,
    NodeProperties,
    OnmsColumn,
    OnmsTableData,
    OutagesProperties,
    Properties,
    SearchOption,
    SNMPInterfaceProperties
} from './types';
import { ClientDelegate } from 'lib/client_delegate';
import { FunctionFormatter } from '../../lib/function_formatter';
import { SimpleOpenNMSRequest } from 'lib/utils';
import {
    getAlarmColumns,
    getIPInterfaceColumns,
    getMonitoredServicesColumns,
    getNodeColumns,
    getOutageColumns,
    getSNMPInterfaceColumns,
    queryAlarms,
    queryIPInterfaces,
    queryMonitoredServices,
    queryNodes,
    queryOutages,
    querySNMPInterfaces,
} from './queries/index';

export const getColumns = (entityName: string): readonly OnmsColumn[] => {
    switch (entityName) {
        case EntityTypes.Alarms:
            return getAlarmColumns()
        case EntityTypes.Nodes:
            return getNodeColumns()
        case EntityTypes.IPInterfaces:
            return getIPInterfaceColumns()
        case EntityTypes.SNMPInterfaces:
            return getSNMPInterfaceColumns()
        case EntityTypes.MonitoredServices:
            return getMonitoredServicesColumns()
        case EntityTypes.Outages:
            return getOutageColumns()
    }

    return [] as OnmsColumn[]
}

export const getSearchProperties = async (entityName: string, client: ClientDelegate): Promise<API.SearchProperty[]> => {
    let properties: API.SearchProperty[] = []

    switch (entityName) {
        case EntityTypes.Alarms:
            properties = await client.getAlarmProperties()
            break
        case EntityTypes.Nodes:
            properties = await client.getNodeProperties()
            break
        case EntityTypes.IPInterfaces:
            properties = await client.getIpInterfaceProperties()
            break
        case EntityTypes.SNMPInterfaces:
            properties = await client.getSnmpInterfaceProperties()
            break
        case EntityTypes.MonitoredServices:
            properties = await client.getMonitoredServiceProperties()
            break
        case EntityTypes.Outages:
            properties = await client.getOutageProperties()
            break
    }

    return properties
}

export const getPropertyComparators = async (entityName: string, propertyId: string, client: ClientDelegate): Promise<API.Comparator[]> => {
    let comparators: API.Comparator[] = []

    switch (entityName) {
        case EntityTypes.Alarms:
            comparators = await client.getAlarmPropertyComparators(propertyId)
            break
        case EntityTypes.Nodes:
            comparators = await client.getNodePropertyComparators(propertyId)
            break
        case EntityTypes.IPInterfaces:
            comparators = await client.getIpInterfacePropertyComparators(propertyId)
            break
        case EntityTypes.SNMPInterfaces:
            comparators = await client.getSnmpInterfacePropertyComparators(propertyId)
            break
        case EntityTypes.MonitoredServices:
            comparators = await client.getMonitoredServicePropertyComparators(propertyId)
            break
        case EntityTypes.Outages:
            comparators = await client.getOutagePropertyComparators(propertyId)
            break
    }

    return comparators
}

export const filterProperties = (entityName: string, fullProperties: API.SearchOptions[]): Properties => {
    let properties: Properties = {}

    switch (entityName) {
        case EntityTypes.Alarms:
            properties = filterAlarmProperties(fullProperties);
            break
        case EntityTypes.Nodes:
            properties = filterNodeProperties(fullProperties);
            break
        case EntityTypes.IPInterfaces:
            properties = filterIPInterfaceProperties(fullProperties);
            break
        case EntityTypes.SNMPInterfaces:
            properties = filterSNMPInterfaces(fullProperties);
            break
        case EntityTypes.MonitoredServices:
            properties = filterMonitoredServiceProperties(fullProperties);
            break
        case EntityTypes.Outages:
            properties = filterOutagesProperties(fullProperties);
            break
    }

    return properties
}

const entityQueries = [
    ['alarms', EntityTypes.Alarms],
    ['ipInterface', EntityTypes.IPInterfaces],
    ['monitoredService', EntityTypes.MonitoredServices],
    ['nodes', EntityTypes.Nodes],
    ['nodeFilter', EntityTypes.Nodes],
    ['outage', EntityTypes.Outages],
    ['snmpInterface', EntityTypes.SNMPInterfaces]
]

export const getEntityTypeFromFuncName = (funcName: string) => {
    // key is start of function name, this will also match e.g. 'outage()' and 'outages()'
    if (funcName) {
        const item = entityQueries.find(d => funcName.startsWith(d[0]))
        if (item) {
            return item[1]
        }
    }

    return null
}

/**
 * Given an EntityType, return the entity datasource function name for it. 
 */
export const getFuncNameFromEntityType = (entityType: string) => {

    if (entityType) {
        const item = entityQueries.find(d => entityType === d[1])

        if (item) {
            return item[0]
        }
    }

    return ''
}

export const getFilterIdFromParts = (entity: SelectableValue<string | number>, attribute: SelectableValue) => {
    const entityName = (entity.label || entity.value || '').toString()
    const attrName = attribute.id || attribute.label || ''

    return `${entityName}_${attrName}`
}

export const getFilterId = (filter: ActiveFilter) => {
    return getFilterIdFromParts(filter.entity, filter.attribute)
}

/**
 * Given a query, parse out the function names and get the EntityTypes value for the first function.
 */
export const getQueryEntityType = (query) => {
    const q = query && query.hasOwnProperty('query') ? query.query : query

    if (!q || !q.trim()) {
        // no query defined, assuming 'alarm' entity type
        return EntityTypes.Alarms
    }

    let functionName = FunctionFormatter.findFunctions(q).filter(x => !!x && !!x.name).map(x => x.name)?.at(0)

    return getEntityTypeFromFuncName(functionName)
}

export const generateProperties = (fullProperties: SearchOption[]) => {
    const newProperties: Record<string, {}> = {}

    for (let props of fullProperties) {
        const label = props.name || props.id
        newProperties[label] = { ...props, label }
    }

    return newProperties as unknown
}

const generateFilteredProperties = (fullProperties: SearchOption[], mappedItems: Record<string, string>) => {
    const filteredArray = fullProperties.filter((f) => {
        return !!mappedItems[f.id]
    })

    const filteredProperties: Record<string, {}> = {};

    for (let filter of filteredArray) {
        const label = mappedItems[filter.id]
        filteredProperties[label] = { ...filter, label };
    }

    return filteredProperties as unknown
}

const filterAlarmProperties = (fullProperties: API.SearchOptions[]): AlarmProperties => {
    const mappedItems = {
        'alarmAckTime': 'Alarm Acknowledged Time',
        'uei': 'UEI',
        'severity': 'Severity',
        'locationName': 'Location',
        'reductionKey': 'Reduction Key',
        'node': 'Node',
        'node.label': 'Node Label',
        'service.name': 'Service Name',
        'ipAddress': 'IP Address',
        'isAcknowledged': 'Is Acknowledged',
        'situationAlarmCount': 'Situation Alarm Count',
        'isSituation': 'Is Situation',
        'isInSituation': 'Is In Situation',
        'affectedNodeCount': 'Affected Node Count',
        'category.name': 'Category',
        'lastEvent.severity.label': 'Last Event Severity',
        'troubleTicketState.label': 'Trouble Ticket State',
    }

    return generateFilteredProperties(fullProperties, mappedItems) as AlarmProperties;
}

const filterNodeProperties = (fullProperties: SearchOption[]): NodeProperties => {
    const mappedItems = {
        'category.name': 'category',
        'label': 'Label',
        'foreignSource': 'Foreign Source',
        'foreignId': 'ForeignId',
        'createTime': 'Created Time',
    }

    return generateFilteredProperties(fullProperties, mappedItems) as NodeProperties;
}

const filterIPInterfaceProperties = (fullProperties: SearchOption[]): IpInterfaceProperties => {
    const mappedItems = {
        'ipAdddress': 'ipAddress',
        'hostname': 'hostname',
        'snmpPrimary': 'snmpPrimary'
    }

    return generateFilteredProperties(fullProperties, mappedItems) as IpInterfaceProperties;
};

const filterSNMPInterfaces = (fullProperties: SearchOption[]): SNMPInterfaceProperties => {
    const mappedItems = {
        ifDescr: 'ifDescr',
        ifName: 'ifName',
        ifSpeed: 'ifSpeed',
        ifAlias: 'ifAlias',
    }

    return generateFilteredProperties(fullProperties, mappedItems) as SNMPInterfaceProperties;
}

const filterMonitoredServiceProperties = (fullProperties: SearchOption[]): MonitoredServiceProperties => {
    const mappedItems = {
        ipAddress: 'ipAddress',
        type: 'type',
        statusId: 'statusId',
    }

    return generateFilteredProperties(fullProperties, mappedItems) as MonitoredServiceProperties;
}

const filterOutagesProperties = (fullProperties: SearchOption[]): OutagesProperties => {
    const mappedItems = {
        foreignSource: 'foreignSource',
        nodeLabel: 'nodeLabel',
        ipAddress: 'ipAddress',
        monitoredServiceTypeName: 'monitoredServiceTypeName',
        ifLostService: 'ifLostService',
        ifRegainedService: 'ifRegainedService',
        perspective: 'perspective'
    }

    return generateFilteredProperties(fullProperties, mappedItems) as OutagesProperties;
}

export const queryEntity = async (label: string | undefined, filter: API.Filter, client: ClientDelegate): Promise<OnmsTableData> => {
    let data: OnmsTableData = {
        name: '',
        columns: [],
        rows: [],
        type: 'table'
    }    

    switch (label) {
        case EntityTypes.Nodes:
            data = await queryNodes(client, filter)
            break
        case EntityTypes.Alarms:
            data = await queryAlarms(client, filter)
            break
        case EntityTypes.IPInterfaces:
            data = await queryIPInterfaces(client, filter)
            break
        case EntityTypes.MonitoredServices:
            data = await queryMonitoredServices(client, filter)
            break
        case EntityTypes.SNMPInterfaces:
            data = await querySNMPInterfaces(client, filter)
            break
        case EntityTypes.Outages:
            data = await queryOutages(client, filter)
            break
    }

    return data
}

export const isLocationQuery = (query: string) => {
    return query && query.match(/locations\([^\)]*\)/i)
}

export const metricFindLocations = async (simpleRequest: SimpleOpenNMSRequest) => {
    const results = await simpleRequest.getLocations();
    return results
}

export const getSmallerAPIFilter = () => {
    const b = new API.Filter()
    b.limit = 10;
    return b;
}

export const isMetricMetadataQuery = (queryType) => {
    const metadataQueryTypes = ['attributes', 'comparators', 'operators']
    return metadataQueryTypes.includes(queryType)
}

export const isSituationAttribute = (attribute) => {
    const situations = ['isSituation', 'isInSituation', 'isAcknowledged']
    return situations.includes(attribute)
}

export const getTemplateVariable = (templateSrv, name) => {
    if (templateSrv.getVariables() && templateSrv.getVariables().length > 0) {
        return templateSrv.getVariables().filter((v) => {
            return v.name === name
        })[0]
    }
    return undefined
}

/** Parse some relevant Helm function info out of an attribute or query. */
export const parseFunctionInfo = (attribute: string) => {
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
