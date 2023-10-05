import { OnmsColumn, OnmsTableData } from '../types'
import { ClientDelegate } from "lib/client_delegate";
import { OnmsMonitoredService } from "opennms/src/model/OnmsMonitoredService";
import { API } from 'opennms'

const columns = Object.freeze([
    { text: 'ID', resource: 'id' },
    { text: 'Last Failed Poll', resource: 'lastFail' },
    { text: 'Last Good Poll', resource: 'lastGood' },
    { text: 'IP Address', resource: 'ipAddress', featured: true },
    { text: 'Type', resource: 'type.name', featured: true },
    { text: 'Status', resource: 'status.id', featured: true },
    { text: 'Down', resource: 'down' },
    { text: 'IP Interface ID', resource: 'ipInterfaceId' },
    { text: 'Node ID', resource: 'nodeId' },
    { text: 'Node Label', resource: 'nodeLabel' }
] as OnmsColumn[]);

export const getMonitoredServicesColumns = () => columns

export const queryMonitoredServices = async (client: ClientDelegate, filter: API.Filter): Promise<OnmsTableData> => {
    let services: OnmsMonitoredService[] = [];

    try {
        services = await client.findMonitoredServices(filter);
    } catch (e) {
        console.error(e);
    }

    const rows = services?.map((service: OnmsMonitoredService) => {
        return [
            service.id,
            service.lastFail,
            service.lastGood,
            service.ipAddress || '',
            service.type?.name,
            service.status?.toDisplayString(),
            service.down,
            service.ipInterfaceId,
            service.nodeId,
            service.nodeLabel
        ];
    });

    return {
        name: 'monitoredServices',
        columns: columns,
        rows: rows,
        type: 'table',
    } as OnmsTableData
}
