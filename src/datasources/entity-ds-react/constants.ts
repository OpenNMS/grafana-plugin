import { OnmsEntityNestType, OnmsEntityType, SearchOption } from "./types"

export const EntityTypes = {
    Alarms: 'Alarms',
    Nodes: 'Nodes',
    IPInterfaces: 'IP Interfaces',
    SNMPInterfaces: 'SNMP Interfaces',
    MonitoredServices: 'Monitored Services',
    Outages: 'Outages',
}

export const defaultClause = {
    attribute: {} as unknown as SearchOption,
    comparator: {} as unknown as { l: string, i: number, aliases: string[] },
    comparedValue: '',
    comparedString: '',
    type: OnmsEntityType.FIRST,
    nestingType: OnmsEntityNestType.TOP
}

export const defaultOrder = { label: 'DESC' }
