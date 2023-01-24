import { getFuncNameFromEntityType } from '../datasources/entity-ds-react/EntityHelper'

export const useEntities = () => {
    const entities = [
        { label: 'Alarms', value: '0' },
        { label: 'Nodes', value: '1' },
        { label: 'IP Interfaces', value: '2' },
        { label: 'SNMP Interfaces', value: '3' },
        { label: 'Monitored Services', value: '4'},
        { label: 'Outages', value: '5' },
    ]

    return {
        entities,
        getFuncNameFromEntityType
    }
}
