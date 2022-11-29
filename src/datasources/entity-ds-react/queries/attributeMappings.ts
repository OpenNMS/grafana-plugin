import { EntityTypes } from '../constants'

/**
 * Given an entity type and query or query attribute, return the DAO mapping for that attribute, or
 * the original query/attribute if there is no special mapping.
 */
export const getAttributeMapping = (entityType: string, attribute: string): string => {
    const attributeMap = getAttributeMap(entityType)

    return attributeMap[attribute] || attribute
}

const getAttributeMap = (entityType: string) => {
    switch (entityType) {
        case EntityTypes.Alarms:
            return alarmAttributeMapping
        case EntityTypes.Nodes:
            return nodeAttributeMapping
        case EntityTypes.IPInterfaces:
            return ipInterfaceAttributeMapping
        case EntityTypes.SNMPInterfaces:
            return snmpInterfaceAttributeMapping
        case EntityTypes.MonitoredServices:
            return monitoredServiceAttributeMapping
        case EntityTypes.Outages:
            return outageAttributeMapping
        default:
            return {}
    }
}

const nodeAttributeMapping = {
  category: 'category.name',
  categories: 'category.name',
  'categories.name': 'category.name',
  ifIndex: 'snmpInterface.ifIndex',
  ipAddr: 'ipInterface.ipAddress',
  ipAddress: 'ipInterface.ipAddress',
  ipHostname: 'ipInterface.ipHostname',
  location: 'location.locationName',
  parentId: 'parent.id',
  parentForeignSource: 'parent.foreignSource',
  // typo??
  //parentForeignId: 'parent.foreindId',
  parentForeignId: 'parent.foreignId',
}

const alarmAttributeMapping = {
  'location': 'location.locationName',
  'service': 'serviceType.name',
  'category': 'category.name',
  'ipAddr': 'ipInterface.ipAddress',
  'ipAddress': 'ipInterface.ipAddress',
  'lastEvent.severity': 'lastEvent.severity.label',
  'severity': 'severity',
  'troubleTicketState': 'troubleTicketState.label',
}

const ipInterfaceAttributeMapping = {
  ifAlias: 'snmpInterface.ifAlias',
  ifDescr: 'snmpInterface.ifDescr',
  ifIndex: 'snmpInterface.ifIndex',
  ifName: 'snmpInterface.ifName',
  physAddr: 'snmpInterface.physAddr',
}

const snmpInterfaceAttributeMapping = {}

const monitoredServiceAttributeMapping = {
  node: 'node.id',
  nodeLabel: 'node.label',
  foreignSource: 'node.foreignSource',
  foreignId: 'node.foreignId',
  ipAddress: 'ipInterface.ipAddress',
}

const outageAttributeMapping = {
  // typo??
  rode: 'nodeId',
  node: 'nodeId',
}
