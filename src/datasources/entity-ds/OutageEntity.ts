import { OnmsOutage } from 'opennms/src/model/OnmsOutage';

import Entity from './Entity';
import { AttributeMapping } from './mapping/AttributeMapping';

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
  { text: 'Data Source' }
];

const mapping = new AttributeMapping({
  rode: 'nodeId',
});

export default class OutageEntity extends Entity {
  name = 'outages';
  type = 'outage';

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
    return this.client.getOutageProperties();
  }

  getPropertyComparators(attribute) {
    return this.client.getOutagePropertyComparators(attribute);
  }

  findProperty(attribute) {
    return this.client.findOutageProperty(attribute);
  }

  async query(filter) {
    const outages = await this.client.findOutages(filter);
    console.debug('outages=', outages);

    const rows = outages.map((outage: OnmsOutage) => {
      console.debug('outage=', outage);

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
        this.name,
      ];
    });

    const metas = outages.map((iface) => {
      return {
        // Store the outage for easy access by the panels
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
