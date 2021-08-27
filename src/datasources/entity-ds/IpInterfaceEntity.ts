import { OnmsIpInterface } from 'opennms/src/model/OnmsIpInterface';

import Entity from './Entity';
import { AttributeMapping } from './mapping/AttributeMapping';

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
  { text: 'Data Source' }
];

const mapping = new AttributeMapping({
  ifAlias: 'snmpInterface.ifAlias',
  ifDescr: 'snmpInterface.ifDescr',
  ifIndex: 'snmpInterface.ifIndex',
  ifName: 'snmpInterface.ifName',
  physAddr: 'snmpInterface.physAddr',
});

export default class IpInterfaceEntity extends Entity {
  name = 'ipInterfaces';
  type = 'ipInterface';

  constructor(client, datasource) {
    super(client, datasource);
  }

  getAttributeMapping() {
    return mapping;
  }

  getColumns() {
    return columns;
  }

  getProperties() {
    return this.client.getIpInterfaceProperties();
  }

  getPropertyComparators(attribute) {
    return this.client.getIpInterfacePropertyComparators(attribute);
  }

  findProperty(attribute) {
    return this.client.findIpInterfaceProperty(attribute);
  }

  async query(filter) {
    const ifaces = await this.client.findIpInterfaces(filter);

    const rows = ifaces.map((iface: OnmsIpInterface) => {
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
        this.name,
      ];
    });

    const metas = ifaces.map((iface) => {
      return {
        // Store the interface for easy access by the panels
        [this.type]: iface,
        // Store the entity type
        'type': this.type,
        // Store the name of the data-source as part of the data so that
        // the panel can grab an instance of the DS to perform actions
        // on the nodes, if necessary
        'source': this.datasource.name,
      }
    });

    return [
      {
        'columns': columns,
        'meta': {
          entity_metadata: metas,
        },
        'rows': rows,
        'type': 'table',
      }
    ];
  }
}
