import { ClientDelegate } from 'lib/client_delegate'
import { OnmsNode } from 'opennms/src/model/OnmsNode'
import { API } from 'opennms'
import { OnmsColumn, OnmsTableData } from '../types'

const columns = Object.freeze([
    { text: 'ID', resource: 'id' },
    { text: 'Label', resource: 'label', featured: true, },
    { text: 'Label Source', resource: 'labelSource' },
    { text: 'Foreign Source', resource: 'foreignSource', featured: true, },
    { text: 'Foreign ID', resource: 'foreignId', featured: true, },
    { text: 'Location', resource: 'location.locationName' },
    { text: 'Creation Time', resource: 'createTime', featured: true, },
    { text: 'Parent ID', resource: 'parent.id' },
    { text: 'Parent Foreign Source', resource: 'parent.foreignSource' },
    { text: 'Parent Foreign ID', resource: 'parent.foreignId' },
    { text: 'Type', resource: 'type' },
    { text: 'SNMP sysObjectID', resource: 'sysObjectId' },
    { text: 'SNMP sysName', resource: 'sysName' },
    { text: 'SNMP sysDescription', resource: 'sysDescription' },
    { text: 'SNMP sysLocation', resource: 'sysLocation' },
    { text: 'SNMP sysContact', resource: 'sysContact' },
    { text: 'NETBIOS/SMB Name', resource: 'netBiosName' },
    { text: 'NETBIOS/SMB Domain', resource: 'netBiosDomain' },
    { text: 'Operating System', resource: 'operatingSystem' },
    { text: 'Last Poll Time', resource: 'lastCapsdPoll' },
    /* { text: 'Primary SNMP Physical Address', resource: 'ipInterface.snmpInterface.physAddr' }, */
    { text: 'Primary SNMP ifIndex', resource: 'snmpInterface.ifIndex' },
    { text: 'Primary IP Interface', resource: 'ipInterface.ipAddress' },
    /* { text: 'Primary IP Hostname', resource: 'ipInterface.ipHostname' }, */
    { text: 'Categories', resource: 'category.name', featured: true, },
    { text: 'Data Source' }
] as OnmsColumn[])

export const getNodeColumns = () => columns

const getSnmpPrimaryInterface = (node: OnmsNode) => {
  if (node.ipInterfaces) {
    const primary = node.ipInterfaces.filter(iface => {
        return !!iface.snmpPrimary?.isPrimary()
    })?.[0]

    return primary
  }

  return undefined
}

export const queryNodes = async (client: ClientDelegate, filter: API.Filter): Promise<OnmsTableData> => {
    let nodes: OnmsNode[] = []

    try {
        nodes = await client.findNodes(filter, true)
    } catch (e) {
        console.error(e)
    }
    const rows = nodes?.map((node) => {
        const primaryIpInterface = getSnmpPrimaryInterface(node)
        const ifIndex = primaryIpInterface?.snmpInterfaceId
        const ipAddress = primaryIpInterface?.ipAddress?.correctForm() || ''

        return [
            node.id,
            node.label,
            node.labelSource,
            node.foreignSource,
            node.foreignId,
            node.location,
            node.createTime,
            node.parent ? node.parent.id : '',
            node.parent ? node.parent.foreignSource : '',
            node.parent ? node.parent.foreignId : '',
            node.type ? node.type.toDisplayString() : '',
            node.sysObjectId,
            node.sysName,
            node.sysDescription,
            node.sysLocation,
            node.sysContact,
            node.netBiosName,
            node.netBiosDomain,
            node.operatingSystem,
            node.lastCapsdPoll,
            ifIndex || '',
            ipAddress,
            node.categories ? node.categories.map(cat => cat.name) : ''
        ]
    })
  
    return {
        name: 'nodes',
        columns: columns,
        rows: rows,
        type: 'table',
    } as OnmsTableData
}
