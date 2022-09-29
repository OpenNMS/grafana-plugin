
import { OnmsSnmpInterface } from "opennms/src/model/OnmsSnmpInterface";
import { ClientDelegate } from "lib/client_delegate";
import { API } from 'opennms'

export const querySNMPInterfaces = async (client: ClientDelegate, filter: API.Filter) => {
    let ifaces: OnmsSnmpInterface[] = [];

    try {
        ifaces = await client.findSnmpInterfaces(filter);
    } catch (e) {
        console.error(e);
    }

    const columns = [
        { text: 'ID', resource: 'id' },
        { text: 'Index', resource: 'ifIndex' },
        { text: 'Description', resource: 'ifDescr', featured: true },
        { text: 'Type', resource: 'ifType' },
        { text: 'Name', resource: 'ifName', featured: true },
        { text: 'Speed', resource: 'ifSpeed', featured: true },
        { text: 'Admin Status', resource: 'ifAdminStatus' },
        { text: 'Operational Status', resource: 'ifOperStatus' },
        { text: 'Alias', resource: 'ifAlias', featured: true },
        { text: 'Last Capsd Poll', resource: 'lastCapsdPoll' },
        { text: 'Collected?', resource: 'collect' },
        { text: 'Polled?', resource: 'poll' },
        { text: 'Last SNMP Poll', resource: 'lastSnmpPoll' },
        { text: 'Physical Address', resource: 'physAddr' },
    ];

    const rows = ifaces?.map((iface: OnmsSnmpInterface) => {
        return [
            iface.id,
            iface.ifIndex,
            iface.ifDescr,
            iface.ifType,
            iface.ifName,
            iface.ifSpeed,
            iface.ifAdminStatus?.toDisplayString(),
            iface.ifOperStatus?.toDisplayString(),
            iface.ifAlias,
            iface.lastCapsdPoll,
            iface.collect?.toDisplayString(),
            iface.poll,
            iface.lastSnmpPoll,
            iface.physAddr?.toString(),
        ];
    });
    return {
        'columns': columns,
        'rows': rows,
        'type': 'table',
    }
}
