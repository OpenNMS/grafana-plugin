import { OnmsSnmpInterface } from 'opennms/src/model/OnmsSnmpInterface';

import Entity from './Entity';
import { AttributeMapping } from './mapping/AttributeMapping';

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
  { text: 'Data Source' }
];

const mapping = new AttributeMapping({
});

export default class SnmpInterfaceEntity extends Entity {
  name = 'snmpInterfaces';
  type = 'snmpInterface';

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
    return this.client.getSnmpInterfaceProperties();
  }

  getPropertyComparators(attribute) {
    return this.client.getSnmpInterfacePropertyComparators(attribute);
  }

  findProperty(attribute) {
    return this.client.findSnmpInterfaceProperty(attribute);
  }

  async query(filter) {
    const ifaces = await this.client.findSnmpInterfaces(filter);

    const rows = ifaces.map((iface: OnmsSnmpInterface) => {
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
