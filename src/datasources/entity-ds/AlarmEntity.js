import _ from 'lodash';

import {API} from 'opennms';

import { AttributeMapping } from './mapping/AttributeMapping';
import Entity from './Entity';

const columns = [
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
];

const mapping = new AttributeMapping({
  'location': 'location.locationName',
  'service': 'serviceType.name',
  'category': 'category.name',
  'ipAddr': 'ipInterface.ipAddress',
  'ipAddress': 'ipInterface.ipAddress',
  'lastEvent.severity': 'lastEvent.severity.label',
  'severity': 'severity',
  'troubleTicketState': 'troubleTicketState.label',
});

export default class AlarmEntity extends Entity {
  constructor(client, datasource) {
    super(client, datasource);
    this.type = 'alarm';
  }

  getAttributeMapping() {
    return mapping;
  }

  getColumns() {
    return columns;
  }

  getPanelRestrictions() {
    const self = this;
    const dashboard = self.datasource.dashboardSrv.getCurrent();
    const filterPanel = dashboard.panels.filter(panel => panel.type === 'opennms-helm-filter-panel')[0];
    const restrictions = new API.NestedRestriction();

    if (filterPanel && filterPanel.columns && filterPanel.columns.length > 0) {
      filterPanel.columns.forEach((column) => {
        const selected = column.selected;
        if (!selected) {
          return;
        }
        let key = selected.resource;
        if (selected.entityType && selected.entityType.id !== self.type) {
          key = selected.entityType.id + '.' + key;
        }
        const comparator = API.Comparators.EQ;
        const getValueRestriction = val => {
          if (!self.datasource.templateSrv.isAllValue(val) && !_.isNil(val)) {
            if ((selected.resource === 'categories' || selected.resource === 'category.name')) {
              return new API.Restriction('category.name', comparator, val);
            } else if (selected.inputType === 'text') {
              if (val.length === 0) {
                  return undefined;
              }
              if (!val.startsWith('*') && !val.endsWith('*')) {
                  return new API.Restriction(key, comparator, '*' + val + '*');
              }
            }
            return new API.Restriction(key, comparator, val);
          }
          return undefined;
        };
        if (selected.value) {
          const values = Array.isArray(selected.value) ? selected.value : [selected.value];
          let restriction;
          if (values.length === 0) {
            return;
          } else if (values.length === 1) {
            restriction = getValueRestriction(values[0]);
          } else {
            restriction = new API.NestedRestriction();
            values.forEach(val => {
              if (val) restriction.withOrRestriction(getValueRestriction(val));
            });
            if (!restriction.clauses || restriction.clauses.length === 0) {
              restriction = undefined;
            }
          }
          if (restriction) {
            restrictions.withAndRestriction(restriction);
          }
        }
      });
    }
    if (restrictions.clauses && restrictions.clauses.length > 0) {
      return restrictions;
    }
    return undefined;
  }

  async query(filter) {
    const self = this;

    const panelRestrictions = this.getPanelRestrictions();

    if (panelRestrictions) {
      filter.withAndRestriction(panelRestrictions);
    }

    const c = await this.client.getClientWithMetadata();
    const metadata = c.server.metadata;

    const alarms = await this.client.findAlarms(filter);

    // Build a sorted list of (unique) event parameter names
    let parameterNames = _.uniq(_.sortBy(_.flatten(_.map(alarms, alarm => {
      if (!alarm.lastEvent || !alarm.lastEvent.parameters) {
        return [];
      }
      return _.map(alarm.lastEvent.parameters, parameter => {
        return parameter.name;
      });
    })), name => name), true);

    // Include the event parameters as columns
    _.each(parameterNames, parameterName => {
      columns.push({
        text: 'Param_' + parameterName,
        resource: 'lastEvent.' + parameterName,
      });
    });

    const rows = _.map(alarms, alarm => {
      let row = [
        alarm.id,
        alarm.count,
        alarm.ackUser,
        alarm.ackTime,
        alarm.uei,
        alarm.severity.label,
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
        !_.isNil(alarm.ackUser) && !_.isNil(alarm.ackTime),

        // Event
        alarm.firstEventTime,
        alarm.lastEvent ? alarm.lastEvent.id : undefined,
        alarm.lastEvent ? alarm.lastEvent.time : undefined,
        alarm.lastEvent ? alarm.lastEvent.source : undefined,
        alarm.lastEvent ? alarm.lastEvent.createTime : undefined,
        alarm.lastEvent ? alarm.lastEvent.severity.label : undefined,
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

      // Index the event parameters by name
      const eventParametersByName = {};
      if (alarm.lastEvent && alarm.lastEvent.parameters) {
        _.each(alarm.lastEvent.parameters, parameter => {
          eventParametersByName[parameter.name] = parameter.value;
        });
      }

      // Append the event parameters to the row
      row = row.concat(_.map(parameterNames, parameterName => {
        if (_.has(eventParametersByName, parameterName)) {
          return eventParametersByName[parameterName];
        } else {
          return undefined;
        }
      }));

      row.meta = {
          // Store the alarm for easy access by the panels - may not be necessary
          'alarm': alarm,
          // Store the name of the data-source as part of the data so that
          // the panel can grab an instance of the DS to perform actions
          // on the alarms
          'source': this.datasource.name,
          // Store the entity type
          'type': this.type,
          // Store the ticketerConfig here
          'ticketerConfig': metadata.ticketerConfig
      };

      return row;
    });

    return [
      {
        'columns': columns.filter(column => column.visible !== false),
        'rows': rows,
        'type': 'table',
      }
    ];
  }
}
