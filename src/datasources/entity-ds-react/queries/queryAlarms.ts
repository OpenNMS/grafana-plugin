import { isNil } from 'lodash'
import { API } from 'opennms'
import { OnmsAlarm } from "opennms/src/model/OnmsAlarm";
import { OnmsColumn, OnmsTableData } from '../types'
import { ClientDelegate } from "lib/client_delegate";

const columns = Object.freeze([
    { text: 'ID', resource: 'id' },
    { text: 'Count', resource: 'count' },
    { text: 'Acked By', resource: 'ackUser' },
    { text: 'Ack Time', resource: 'alarmAckTime', featured: true },
    { text: 'UEI', resource: 'uei', featured: true },
    { text: 'Severity', resource: 'severity', featured: true },
    { text: 'Type', resource: 'type.label' },
    { text: 'Description', resource: 'description' },
    { text: 'Location', resource: 'location', featured: true },
    { text: 'Log Message', resource: 'logMessage' },
    { text: 'Reduction Key', resource: 'reductionKey', featured: true },
    { text: 'Trouble Ticket', resource: 'troubleTicket' },
    { text: 'Trouble Ticket State', resource: 'troubleTicketState.label' },
    { text: 'Node ID', resource: 'node', featured: true },
    { text: 'Node Label', resource: 'node.label', featured: true },
    { text: 'Service', resource: 'service.name', featured: true },
    { text: 'Suppressed Time', resource: 'suppressedTime' },
    { text: 'Suppressed Until', resource: 'suppressedUntil' },
    { text: 'Suppressed By', resource: 'suppressedBy' },
    { text: 'IP Address', resource: 'ipAddress', featured: true },
    { text: 'Is Acknowledged', resource: 'isAcknowledged', featured: true },
    { text: 'First Event Time', resource: 'firstEventTime' },
    { text: 'Last Event ID', resource: 'lastEvent.id' },
    { text: 'Last Event Time', resource: 'lastEvent.time' },
    { text: 'Last Event Source', resource: 'lastEvent.source' },
    { text: 'Last Event Creation Time', resource: 'lastEvent.createTime' },
    { text: 'Last Event Severity', resource: 'lastEvent.severity' },
    { text: 'Last Event Label', resource: 'lastEvent.label' },
    { text: 'Last Event Location', resource: 'lastEvent.location' },
    { text: 'Sticky ID', resource: 'sticky.id' },
    { text: 'Sticky Note', resource: 'sticky.body' },
    { text: 'Sticky Author', resource: 'sticky.author' },
    { text: 'Sticky Update Time', resource: 'sticky.updated' },
    { text: 'Sticky Creation Time', resource: 'sticky.created' },
    { text: 'Journal ID', resource: 'journal.id' },
    { text: 'Journal Note', resource: 'journal.body' },
    { text: 'Journal Author', resource: 'journal.author' },
    { text: 'Journal Update Time', resource: 'journal.updated' },
    { text: 'Journal Creation Time', resource: 'journal.created' },
    { text: 'Is Situation', resource: 'isSituation', featured: true },
    { text: 'Is In Situation', resource: 'isInSituation', featured: true, visible: false },
    { text: 'Situation Alarm Count', resource: 'situationAlarmCount', featured: true },
    { text: 'Affected Node Count', resource: 'affectedNodeCount', featured: true },
    { text: 'Managed Object Instance', resource: 'managedObjectInstance' },
    { text: 'Managed Object Type', resource: 'managedObjectType' },
    { text: 'Categories', resource: 'category', featured: true, visible: false },
    { text: 'Data Source' }
] as OnmsColumn[]);

export const getAlarmColumns = () => columns

export const queryAlarms = async (client: ClientDelegate, filter: API.Filter): Promise<OnmsTableData> => {
    let alarms: OnmsAlarm[] = [];

    try {
        alarms = await client.findAlarms(filter)
    } catch (e) {
        console.error(e);
    }
    const rows = alarms?.map((alarm) => {
        let row = [
            alarm.id,
            alarm.count,
            alarm.ackUser,
            alarm.ackTime,
            alarm.uei,
            alarm.severity ? alarm.severity.label : undefined,
            alarm.type ? alarm.type.label : undefined,
            alarm.description,
            alarm.location,

            alarm.logMessage,
            alarm.reductionKey,
            alarm.troubleTicket,
            alarm.troubleTicketState ? alarm.troubleTicketState.label : undefined,
            alarm.nodeId,
            alarm.nodeLabel,
            alarm.service ? alarm.service.name : undefined,
            alarm.suppressedTime,
            alarm.suppressedUntil,
            alarm.suppressedBy,
            alarm.lastEvent ? alarm.lastEvent.ipAddress ? alarm.lastEvent.ipAddress.address : undefined : undefined,
            !isNil(alarm.ackUser) && !isNil(alarm.ackTime),  // isAcknowledged

            // Event
            alarm.firstEventTime,
            alarm.lastEvent ? alarm.lastEvent.id : undefined,
            alarm.lastEvent ? alarm.lastEvent.time : undefined,
            alarm.lastEvent ? alarm.lastEvent.source : undefined,
            alarm.lastEvent ? alarm.lastEvent.createTime : undefined,
            alarm.lastEvent && alarm.lastEvent.severity ? alarm.lastEvent.severity.label : undefined,
            alarm.lastEvent ? alarm.lastEvent.label : undefined,
            alarm.lastEvent ? alarm.lastEvent.location : undefined,

            // Sticky Note
            alarm.sticky ? alarm.sticky.id : undefined,
            alarm.sticky ? alarm.sticky.body : undefined,
            alarm.sticky ? alarm.sticky.author : undefined,
            alarm.sticky ? alarm.sticky.updated : undefined,
            alarm.sticky ? alarm.sticky.created : undefined,

            // Journal Note
            alarm.journal ? alarm.journal.id : undefined,
            alarm.journal ? alarm.journal.body : undefined,
            alarm.journal ? alarm.journal.author : undefined,
            alarm.journal ? alarm.journal.updated : undefined,
            alarm.journal ? alarm.journal.created : undefined,

            // Situation Data
            alarm.relatedAlarms && alarm.relatedAlarms.length > 0 ? 'Y' : 'N',
            alarm.relatedAlarms ? alarm.relatedAlarms.length.toFixed(0) : undefined,
            alarm.affectedNodeCount ? alarm.affectedNodeCount.toFixed(0) : undefined,
            alarm.managedObjectInstance ? alarm.managedObjectInstance : undefined,
            alarm.managedObjectType ? alarm.managedObjectType : undefined,

            // Data Source
            self.name
        ];

        return row;
    });

    return {
        name: 'alarms',
        columns: columns.filter(column => column.visible !== false),
        rows: rows,
        type: 'table',
    } as OnmsTableData
}
