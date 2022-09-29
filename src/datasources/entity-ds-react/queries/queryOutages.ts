
import { ClientDelegate } from "lib/client_delegate";
import { OnmsOutage } from "opennms/src/model/OnmsOutage";
import { API } from 'opennms'

export const queryOutages = async (client: ClientDelegate, filter: API.Filter) => {
    let outages: OnmsOutage[] = [];
    try {
        outages = await client.findOutages(filter);
    } catch (e) {
        console.error(e);
    }
    const columns = [
        { text: 'ID', resource: 'id' },
        { text: 'Foreign Source', resource: 'foreignSource', featured: true },
        { text: 'Foreign ID', resource: 'foreignId' },
        { text: 'Node ID', resource: 'nodeId' },
        { text: 'Node Label', resource: 'nodeLabel', featured: true },
        { text: 'IP Address', resource: 'ipAddress', featured: true },
        { text: 'Service', resource: 'monitoredService.type.name', featured: true },
        { text: 'Lost Service', resource: 'ifLostService', featured: true },
        { text: 'Regained Service', resource: 'ifRegainedService', featured: true },
        { text: 'Suppressed', resource: 'suppressTime' },
        { text: 'Suppressed By', resource: 'suppressedBy' },
        { text: 'Perspective', resource: 'perspective', featured: true },
    ];
    const rows = outages?.map((outage: OnmsOutage) => {

        return [
            outage.id,
            outage.foreignSource,
            outage.foreignId,
            outage.nodeId,
            outage.nodeLabel,
            outage.ipAddress?.correctForm(),
            outage.monitoredService?.type?.name,
            outage.ifLostService,
            outage.ifRegainedService,
            outage.suppressTime,
            outage.suppressedBy,
            outage.perspective,
        ];
    });
    return {
        'columns': columns,
        'rows': rows,
        'type': 'table',
    }
}
