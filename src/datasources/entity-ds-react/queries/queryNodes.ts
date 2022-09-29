import { ClientDelegate } from "lib/client_delegate";
import { OnmsNode } from "opennms/src/model/OnmsNode";
import { API } from '../../../../../../../opennmsdev/opennms-js'

export const queryNodes = async (client: ClientDelegate, filter: API.Filter) => {
    let nodes: OnmsNode[] = [];
    try {
        nodes = await client.findNodes(filter, true)
    } catch (e) {
        console.error(e);
    }
    const columns = [
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
    ];
    const rows = nodes?.map((node) => {
        return [
            node.id,
            node.label,
            node.labelSource,
            node.foreignSource,
            node.foreignId,
            node.location,
            node.createTime,
            node.parent ? node.parent.id : undefined,
            node.parent ? node.parent.foreignSource : undefined,
            node.parent ? node.parent.foreignId : undefined,
            node.type ? node.type.toDisplayString() : undefined,
            node.sysObjectId,
            node.sysName,
            node.sysDescription,
            node.sysLocation,
            node.sysContact,
            node.netBiosName,
            node.netBiosDomain,
            node.operatingSystem,
            node.lastCapsdPoll,
            node.categories ? node.categories.map(cat => cat.name) : undefined,
        ]
    })
  
    return {
        'columns': columns,
        'rows': rows,
        'type': 'table',
    }
}
