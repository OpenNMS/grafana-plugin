import { API } from 'opennms'
import { EntityTypes } from './constants'
import {
    AlarmProperties,
    IpInterfaceProperties,
    MonitoredServiceProperties,
    NodeProperties,
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

export const getColumns = (entityName: string) => {
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
        default:
            return []
    }
}

export const getSearchProperties = (entityName: string, client: ClientDelegate): Promise<API.SearchProperty[]> => {
    switch (entityName) {
        case EntityTypes.Alarms:
            return client.getAlarmProperties()
        case EntityTypes.Nodes:
            return client.getNodeProperties()
        case EntityTypes.IPInterfaces:
            return client.getIpInterfaceProperties()
        case EntityTypes.SNMPInterfaces:
            return client.getSnmpInterfaceProperties()
        case EntityTypes.MonitoredServices:
            return client.getMonitoredServiceProperties()
        case EntityTypes.Outages:
            return client.getOutageProperties()
        default:
            return Promise.resolve([] as API.SearchProperty[])
    }
}

export const getPropertyComparators = (entityName: string, propertyId: string, client: ClientDelegate): Promise<API.Comparator[]> => {
    switch (entityName) {
        case EntityTypes.Alarms:
            return client.getAlarmPropertyComparators(propertyId)
        case EntityTypes.Nodes:
            return client.getNodeProperties()
        case EntityTypes.IPInterfaces:
            return client.getIpInterfaceProperties()
        case EntityTypes.SNMPInterfaces:
            return client.getSnmpInterfaceProperties()
        case EntityTypes.MonitoredServices:
            return client.getMonitoredServiceProperties()
        case EntityTypes.Outages:
            return client.getOutageProperties()
        default:
            return Promise.resolve([] as API.SearchProperty[])
    }
}

export const filterProperties = (entityName: string, fullProperties: API.SearchOptions[]): Properties => {
    switch (entityName) {
        case EntityTypes.Alarms:
            return filterAlarmProperties(fullProperties);
        case EntityTypes.Nodes:
            return filterNodeProperties(fullProperties);
        case EntityTypes.IPInterfaces:
            return filterIPInterfaceProperties(fullProperties);
        case EntityTypes.SNMPInterfaces:
            return filterSNMPInterfaces(fullProperties);
        case EntityTypes.MonitoredServices:
            return filterMonitoredServiceProperties(fullProperties);
        case EntityTypes.Outages:
            return filterOutagesProperties(fullProperties);
        default:
            return {} as Properties;
    }
}

export const getEntityTypeFromFuncName = (funcName: string) => {
    // key is start of function name, this will also match e.g. 'outage()' and 'outages()'
    const entityQueries = [
        ['alarms', EntityTypes.Alarms],
        ['ipInterface', EntityTypes.IPInterfaces],
        ['monitoredService', EntityTypes.MonitoredServices],
        ['nodes', EntityTypes.Nodes],
        ['nodeFilter', EntityTypes.Nodes],
        ['outage', EntityTypes.Outages],
        ['snmpInterface', EntityTypes.SNMPInterfaces]
    ]

    if (funcName) {
        const item = entityQueries.find(d => funcName.startsWith(d[0]))
        if (item) {
            return item[1]
        }
    }

    return null
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
    switch (label) {
        case EntityTypes.Nodes:
            return queryNodes(client, filter);
        case EntityTypes.Alarms:
            return queryAlarms(client, filter);
        case EntityTypes.IPInterfaces:
            return queryIPInterfaces(client, filter);
        case EntityTypes.MonitoredServices:
            return queryMonitoredServices(client, filter);
        case EntityTypes.SNMPInterfaces:
            return querySNMPInterfaces(client, filter);
        case EntityTypes.Outages:
            return queryOutages(client, filter);
        default:
            return Promise.resolve({
                name: '',
                columns: [],
                rows: [],
                type: 'table',
            } as OnmsTableData);
    }
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
