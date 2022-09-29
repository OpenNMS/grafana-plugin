import { ClientDelegate } from "lib/client_delegate";
import { OnmsIpInterface } from "opennms/src/model/OnmsIpInterface";
import { API } from 'opennms'

export const queryIPInterfaces = async (client: ClientDelegate, filter: API.Filter) => {
    let ifaces: OnmsIpInterface[] = [];
 
    try {
        ifaces = await client.findIpInterfaces(filter);
    } catch (e) {
        console.error(e);
    }

    const columns = [
        { text: 'ID', resource: 'id' },
        { text: 'IP Address', resource: 'ipAddress', featured: true, },
        { text: 'Hostname', resource: 'hostname', featured: true, },
        { text: 'Is Down?', resource: 'isDown', },
        { text: 'Is Managed?', resource: 'isManaged', },
        { text: 'Last Capsd Poll', resource: 'lastCapsdPoll', },
        { text: 'Last Ingress Flow', resource: 'lastIngressFlow', },
        { text: 'Last Egress Flow', resource: 'lastEgressFlow', },
        { text: 'Service Count', resource: 'monitoredServiceCount', },
        { text: 'SNMP Primary', resource: 'snmpPrimary', featured: true, },
        { text: 'SNMP ifAlias', resource: 'snmpInterface.ifAlias' },
        { text: 'SNMP ifDescr', resource: 'snmpInterface.ifDescr' },
        { text: 'SNMP ifIndex', resource: 'snmpInterface.ifIndex' },
        { text: 'SNMP PhysAddr', resource: 'snmpInterface.physAddr' },
    ];

    const rows = ifaces?.map((iface: OnmsIpInterface) => {
        return [
            iface.id,
            iface.ipAddress?.correctForm(),
            iface.hostname,
            iface.isDown,
            iface.isManaged?.toDisplayString(),
            iface.lastCapsdPoll,
            iface.lastIngressFlow,
            iface.lastEgressFlow,
            iface.monitoredServiceCount,
            iface.snmpPrimary?.toDisplayString(),
            iface.snmpInterface?.ifAlias,
            iface.snmpInterface?.ifDescr,
            iface.snmpInterface?.ifIndex,
            iface.snmpInterface?.physAddr?.toString(),
        ];
    });

    return {
        'columns': columns,
        'rows': rows,
        'type': 'table',
    }
}
