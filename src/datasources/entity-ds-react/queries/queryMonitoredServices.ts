import { OnmsColumn, OnmsTableData } from '../types'
import { ClientDelegate } from "lib/client_delegate";
import { OnmsMonitoredService } from "opennms/src/model/OnmsMonitoredService";
import { API } from 'opennms'

const columns = Object.freeze([
    { text: 'ID', resource: 'id' },
    { text: 'Last Failed Poll', resource: 'lastFail' },
    { text: 'Last Good Poll', resource: 'lastGood' },
    { text: 'IP Address', resource: 'ipInterface.ipAddress', featured: true },
    { text: 'Type', resource: 'type.name', featured: true },
    { text: 'Status', resource: 'status.id', featured: true }
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
            service.ipInterface?.ipAddress?.correctForm(),
            service.type?.name,
            service.status?.toDisplayString(),
        ];
    });

    return {
        name: 'monitoredServices',
        columns: columns,
        rows: rows,
        type: 'table',
    } as OnmsTableData
}
