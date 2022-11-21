import { API } from 'opennms'
import { EntityTypes } from './constants'
import { AlarmProperties, IpInterfaceProperties, MonitoredServiceProperties, NodeProperties, OutagesProperties, Properties, SearchOption, SNMPInterfaceProperties } from './types';
import { ClientDelegate } from 'lib/client_delegate';
import { queryNodes } from './queries/index';
import { queryAlarms } from './queries/queryAlarms';
import { queryIPInterfaces } from './queries/queryIPInterfaces';
import { queryMonitoredServices } from './queries/queryMonitoredServices';
import { queryOutages } from './queries/queryOutages';
import { querySNMPInterfaces } from './queries/querySNMPInterfaces';

export const filterProperties = (entityName: string, fullProperties: API.SearchOptions[]): Properties => {
    let filteredProperties: Properties = {};
    switch (entityName) {
        case EntityTypes.Alarms:
            filteredProperties = filterAlarmProperties(fullProperties);
            break;
        case EntityTypes.Nodes:
            filteredProperties = filterNodeProperties(fullProperties);
            break;
        case EntityTypes.IPInterfaces:
            filteredProperties = filterIPInterfaceProperties(fullProperties);
            break;
        case EntityTypes.SNMPInterfaces:
            filteredProperties = filterSNMPInterfaces(fullProperties);
            break;
        case EntityTypes.MonitoredServices:
            filteredProperties = filterMonitoredServiceProperties(fullProperties);
            break;
        case EntityTypes.Outages:
            filteredProperties = filterOutagesProperties(fullProperties);
            break;
    }
    return filteredProperties;
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
        'foreignId': 'ForiegnId',
        'craeteTime': 'Created Time',
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
    return generateFilteredProperties(fullProperties, mappedItems) as OutagesProperties
}

export const queryEntity = async (label: string | undefined, filter: API.Filter, client: ClientDelegate) => {
    let queryResult;
    switch (label) {
        case EntityTypes.Nodes:
            queryResult = await queryNodes(client, filter);
            break;
        case EntityTypes.Alarms:
            queryResult = await queryAlarms(client, filter);
            break;
        case EntityTypes.IPInterfaces:
            queryResult = await queryIPInterfaces(client, filter);
            break;
        case EntityTypes.MonitoredServices:
            queryResult = await queryMonitoredServices(client, filter);
            break;
        case EntityTypes.SNMPInterfaces:
            queryResult = await querySNMPInterfaces(client, filter);
            break;
        case EntityTypes.Outages:
            queryResult = await queryOutages(client, filter);
            break;
    }
    return queryResult
}

export const queryProperties = async (queryWithNodeFilterWrapper: string, client: ClientDelegate) => {
    const results: Array<{text: string,value: string}> = []
    const query = queryWithNodeFilterWrapper.replace('nodes(','').replace(')','')
    const nodeProps = await client.getNodeProperties();
    const property = nodeProps.find((d) => d.id === query)
    if (property){
        for (const [key,value] of Object.entries(property.values)){
            results.push({text:value as string,value:key as string})
        }
    }   
    return results
}


export const getSmallerAPIFilter = () => {
    const b = new API.Filter()
    b.limit = 10;
    return b;
}
