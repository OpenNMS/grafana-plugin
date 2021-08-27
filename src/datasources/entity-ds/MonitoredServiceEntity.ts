import { OnmsMonitoredService } from 'opennms/src/model/OnmsMonitoredService';

import Entity from './Entity';
import { AttributeMapping } from './mapping/AttributeMapping';

const columns = [
  { text: 'ID', resource: 'id' },
  { text: 'Last Failed Poll', resource: 'lastFail' },
  { text: 'Last Good Poll', resource: 'lastGood' },
  { text: 'IP Address', resource: 'ipInterface.ipAddress', featured: true },
  { text: 'Type', resource: 'type.name', featured: true },
  { text: 'Status', resource: 'status.id', featured: true },
  { text: 'Data Source' }
];

const mapping = new AttributeMapping({
  node: 'node.id',
  nodeLabel: 'node.label',
  foreignSource: 'node.foreignSource',
  foreignId: 'node.foreignId',
  ipAddress: 'ipInterface.ipAddress',
});

export default class MonitoredServiceEntity extends Entity {
  name = 'monitoredServices';
  type = 'monitoredService';

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
    return this.client.getMonitoredServiceProperties();
  }

  getPropertyComparators(attribute) {
    return this.client.getMonitoredServicePropertyComparators(attribute);
  }

  findProperty(attribute) {
    return this.client.findMonitoredServiceProperty(attribute);
  }

  async query(filter) {
    const services = await this.client.findMonitoredServices(filter);
    console.debug('services=', services);

    const rows = services.map((service: OnmsMonitoredService) => {
      console.debug('service=', service);

      /*
        { text: 'ID', resource: 'id' },
  { text: 'Last Failed Poll', resource: 'lastFail' },
  { text: 'Last Good Poll', resource: 'lastGood' },
  { text: 'IP Address', resource: 'ipInterface.ipAddress', featured: true },
  { text: 'Type', resource: 'type.name', featured: true },
  { text: 'Status', resource: 'status.id', featured: true },
  { text: 'Data Source' }
*/
      return [
        service.id,
        service.lastFail,
        service.lastGood,
        service.ipInterface?.ipAddress?.correctForm(),
        service.type?.name,
        service.status?.toDisplayString(),
        this.name,
      ];
    });

    const metas = services.map((iface) => {
      return {
        // Store the service for easy access by the panels
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
