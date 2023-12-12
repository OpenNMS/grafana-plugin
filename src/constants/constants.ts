export const EntityTypes = {
  Alarms: 'Alarms',
  Nodes: 'Nodes',
  IPInterfaces: 'IP Interfaces',
  SNMPInterfaces: 'SNMP Interfaces',
  MonitoredServices: 'Monitored Services',
  Outages: 'Outages',
}

/** Maps an entity query function name with the EntityType that it requests. */
export const EntityQueries = [
    [ 'alarms', EntityTypes.Alarms ],
    [ 'ipInterface', EntityTypes.IPInterfaces ],
    [ 'monitoredService', EntityTypes.MonitoredServices ],
    [ 'nodes', EntityTypes.Nodes ],
    [ 'nodeFilter', EntityTypes.Nodes ],
    [ 'outage', EntityTypes.Outages ],
    [ 'snmpInterface', EntityTypes.SNMPInterfaces ]
]

export const ALL_SELECTION_VALUE = '$__all'

export const DATASOURCE_HELP_BASE_URL = 'https://docs.opennms.com/grafana-plugin/latest'
