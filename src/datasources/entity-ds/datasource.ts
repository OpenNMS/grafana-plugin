import _ from 'lodash';
import {API, Model} from 'opennms';

import {ClientDelegate} from '../../lib/client_delegate';
import {FunctionFormatter} from '../../lib/function_formatter';

import AlarmEntity from './AlarmEntity';
import IpInterfaceEntity from './IpInterfaceEntity';
import MonitoredServiceEntity from './MonitoredServiceEntity';
import NodeEntity from './NodeEntity';
import OutageEntity from './OutageEntity';
import SnmpInterfaceEntity from './SnmpInterfaceEntity';
import Entity from "./Entity";
import { SimpleOpenNMSRequest } from "lib/utils";

const isNumber = function isNumber(num) {
    return ((parseInt(num,10) + '') === (num + ''));
};

export const entityTypes = [
    {
        id: 'alarm',
        label: 'Alarms',
        queryFunction: 'alarms',
    },
    {
        id: 'node',
        label: 'Nodes',
        queryFunction: 'nodes',
    },
    {
        id: 'ipInterface',
        label: 'IP Interfaces',
        queryFunction: 'ipInterfaces',
    },
    {
        id: 'snmpInterface',
        label: 'SNMP Interfaces',
        queryFunction: 'snmpInterfaces',
    },
    {
        id: 'monitoredService',
        label: 'Monitored Services',
        queryFunction: 'monitoredServices',
    },
    {
        id: 'outage',
        label: 'Outages',
        queryFunction: 'outages',
    },
];

export const getEntity = (e, client, datasource) => {
    if (!e) {
        return null;
    }

    const entityType = e.id ? e.id : e;
    switch(entityType) {
        case 'alarm': return new AlarmEntity(client, datasource);
        case 'ipInterface': return new IpInterfaceEntity(client, datasource);
        case 'monitoredService': return new MonitoredServiceEntity(client, datasource);
        case 'node': return new NodeEntity(client, datasource);
        case 'outage': return new OutageEntity(client, datasource);
        case 'snmpInterface': return new SnmpInterfaceEntity(client, datasource);
    }

    throw new Error('Unable to get entity for ' + JSON.stringify(e));
};

export class OpenNMSEntityDatasource {
    name?: string;
    url?: string;
    type?: string;
    opennmsClient: ClientDelegate;
    user?: string;
    simpleRequest: SimpleOpenNMSRequest;

    /** @ngInject */
  constructor(instanceSettings: any, public backendSrv: any, public templateSrv: any, public contextSrv: any, public dashboardSrv: any) {
    this.type = instanceSettings.type;
    this.url = instanceSettings.url;
    this.name = instanceSettings.name;
    this.opennmsClient = new ClientDelegate(instanceSettings, backendSrv);
    this.simpleRequest = new SimpleOpenNMSRequest(backendSrv, this.url);

    // When enabled in the datasource, the grafana user should be used instead of the datasource username on
    // supported operations
    if (instanceSettings.jsonData && instanceSettings.jsonData.useGrafanaUser) {
        // If the datasource contains the field which should be used and that field is set, use it
        if (instanceSettings.jsonData.grafanaUserField && contextSrv.user[instanceSettings.jsonData.grafanaUserField]) {
            this.user = contextSrv.user[instanceSettings.jsonData.grafanaUserField];
        } else { // otherwise the login is used instead
            this.user = contextSrv.user.login;
        }
    }
  }

  query(options) {
      const target = options.targets[0]; // TODO: handle multiple target queries

      // Initialize filter
      const filter = target.filter || new API.Filter();
      const entityType = target.entityType || 'alarm';
      filter.limit = target.limit || 0; // 0 = no limit

      let entity: Entity | null = getEntity(entityType, this.opennmsClient, this);
      if (!entity) {
        console.error('Unable to determine entity from entity type', entityType);
        entity = new AlarmEntity(this.opennmsClient, this);
      }

      options.enforceTimeRange = true;
      options.entity = entity;
      const clonedFilter = this.buildQuery(filter, options);

      return entity.query(clonedFilter).then((data) => {
          return { data: data };
      });
  }

  // Clone Filter to make substitution possible
  // (otherwise substitution would happen in original query,
  // and overwriting the $<variable> or [[variable]] in restrictions which may not be the intention)
  buildQuery(filter, options) {
      const clonedFilter = API.Filter.fromJson(filter);

      // Before replacing any variables, add a global time range restriction (which is hidden to the user)
      // This behavior should probably be _in_ the entity, but... ¯\_(ツ)_/¯
      if (options && options.enforceTimeRange) {
          if (!options.entity || options.entity.type === 'alarm') {
              clonedFilter.withAndRestriction(new API.NestedRestriction()
                  .withAndRestriction(new API.Restriction("lastEventTime", API.Comparators.GE, "$range_from"))
                  .withAndRestriction(new API.Restriction("lastEventTime", API.Comparators.LE, "$range_to"))
              );
          }
      }

      // Substitute $<variable> or [[variable]] in the restriction value
      this.substitute(clonedFilter.clauses, options);
      return clonedFilter;
  }

  _getTemplateVariable(name) {
    if (this.templateSrv.variables && this.templateSrv.variables.length > 0) {
        return this.templateSrv.variables.filter((v) => {
            return v.name === name;
        })[0];
    }
    return undefined;
  }

  subtituteNodeRestriction(clause) {
    const restriction = clause.restriction;
    // Handle "node" as a special case, updating restrictions to either foreignSource+foreignId or node.id
    if (restriction.attribute === 'node') {
        if (restriction.value.indexOf(':') > 0) {
            if (restriction.comparator.id !== API.Comparators.EQ.id) {
                console.warn('Using a comparator other than EQ will probably not work as expected with a foreignSource:foreignId node criteria.');
            }
            const nodeCriteria = restriction.value.split(':');
            const replacement = new API.NestedRestriction(
                new API.Clause(new API.Restriction('node.foreignSource', restriction.comparator, nodeCriteria[0]), API.Operators.AND),
                new API.Clause(new API.Restriction('node.foreignId', restriction.comparator, nodeCriteria[1]), API.Operators.AND),
            );
            clause.restriction = replacement;
        } else if (isNumber(restriction.value)) {
            clause.restriction = new API.Restriction('node.id', restriction.comparator, restriction.value);
        } else if (restriction.value === '{}') {
            return true;
        } else {
            console.warn('found a "node" criteria but it does not appear to be a node ID nor a foreignSource:foreignId tuple.',restriction);
        }
    } 
    return false;
  }

  substitute(clauses, options) {
      const self = this;
      const remove = [] as any[];
      _.each(clauses, clause => {
        if (clause.restriction) {
            const restriction = clause.restriction;
            if (restriction instanceof API.NestedRestriction) {
                self.substitute(restriction.clauses, options);
            } else if (restriction.value) {
                const variableName = self.templateSrv.getVariableName(restriction.value);
                const templateVariable = self._getTemplateVariable(variableName);

                // Process multi-selects
                if (templateVariable && templateVariable.multi) {
                    if (templateVariable.current.value && self.templateSrv.isAllValue(templateVariable.current.value)) {
                        // if we're querying "all" we just dump the clause altogether
                        remove.push(clause);
                    } else {
                        // annoyingly, depending on how you interact with the UI, if one value is selected it will
                        // *either* be an array with 1 entry, or just the raw value >:|
                        // so we normalize it back to just the raw value here if necessary
                        if (_.isArray(templateVariable.current.value) && templateVariable.current.value.length === 1) {
                            templateVariable.current.value = templateVariable.current.value[0];
                        }

                        // now if it's *still* an array, we chop it up into nested restrictions
                        if (_.isArray(templateVariable.current.value)) {
                            const replacement = new API.NestedRestriction();
                            let values = templateVariable.current.value;
                            if (!_.isArray(values)) {
                                values = [values];
                            }
                            for (const value of values) {
                                if (restriction.comparator.id === API.Comparators.EQ.id) {
                                    replacement.withOrRestriction(new API.Restriction(restriction.attribute, restriction.comparator, value));
                                } else if (restriction.comparator.id === API.Comparators.NE.id) {
                                    replacement.withAndRestriction(new API.Restriction(restriction.attribute, restriction.comparator, value));
                                } else {
                                    throw new Error('Unable to query "' + restriction.attribute + '": multi-select values with variable substitution must be either "=" or "!="');
                                }
                            }

                            // we've turned a single restriction into a nested one, so re-process it as a
                            // collection and skip the simple replacement below
                            clause.restriction = replacement;
                            self.substitute(clause.restriction.clauses, options);
                            return;
                        }
                    }
                }

                // Range must be of type date, otherwise it is not parseable by the OpenNMS client
                if (variableName === 'range_from') {
                    restriction.value = options.range.from;
                } else if (variableName === 'range_to') {
                    restriction.value = options.range.to;
                } else {
                    restriction.value = self.templateSrv.replace(restriction.value, options.scopedVars);
                }

                const shouldRemove = self.subtituteNodeRestriction(clause);
                if (shouldRemove) {
                    remove.push(clause);
                }
            }
        }
      });
      for (const r of remove) {
        const i = clauses.indexOf(r);
        if (i >= 0) {
            clauses.splice(i, 1);
        }
      }
    }

  testDatasource() {
      return this.opennmsClient.getClientWithMetadata()
          .then(metadata => {
              if (metadata) {
                  return {
                      status: "success",
                      message: "Data source is working",
                      title: "Success"
                  };
              } else {
                return {
                  status: "danger",
                  message: "OpenNMS provided a response, but no metadata was found.",
                  title: "Unexpected Response"
                }
              }
          }).catch(e => {
              if (e.message === "Unsupported Version") {
                  return {
                      status: "danger",
                      message: "The OpenNMS version you are trying to connect to is not supported. " +
                               "OpenNMS Horizon version >= 20.1.0 or OpenNMS Meridian version >= 2017.1.0 is required.",
                      title: e.message
                  }
              } else {
                  throw e;
              }
          });
  }

  annotationQuery(/* options */) {
    return Promise.resolve([]);
  }

  _getQueryEntity(query) {
    const q = query && query.hasOwnProperty('query') ? query.query : query;
    if (q === undefined || q === null || q.trim().length === 0) {
        console.debug('_getQueryEntity: no query defined, assuming "alarm" entity type.');
        return new AlarmEntity(this.opennmsClient, this);
    }

    try {
        const functions = FunctionFormatter.findFunctions(q);
        for (const func of functions) {
            if (func.name === 'alarms') {
                return new AlarmEntity(this.opennmsClient, this);
            } else if (func.name === 'ipInterfaces') {
                return new IpInterfaceEntity(this.opennmsClient, this);
            } else if (func.name === 'monitoredServices') {
                return new MonitoredServiceEntity(this.opennmsClient, this);
            } else if (func.name === 'nodes') {
                return new NodeEntity(this.opennmsClient, this);
            } else if (func.name === 'outages') {
                return new OutageEntity(this.opennmsClient, this);
            } else if (func.name === 'snmpInterfaces') {
                return new SnmpInterfaceEntity(this.opennmsClient, this);
            }
        }
    } catch (e) {
        console.warn('_getQueryEntity: failed to get functions:', e);
    }

    console.warn('WARNING: unable to infer entity type based on query: ' + q);
    return null;
  }

  metricFindQuery(query, optionalOptions) {
    const options = optionalOptions || {};

    const locations = query ? query.match(/locations\([^\)]*\)/i) : null;
    if(locations){
        return this.metricFindLocations();
    }

    const nodeFilter = query ? query.match(/nodeFilter\(([^\)]*)\)/i) : null;
    if(nodeFilter){
        let filterQuery = nodeFilter.length > 1 ? nodeFilter[1] : null;
        return this.metricFindNodeFilterQuery(filterQuery);
    }

    const entity = options.entityType ? getEntity(options.entityType, this.opennmsClient, this) : this._getQueryEntity({ query: query });

    if (!entity) {
        throw new Error('Failed to get entity.');
    }

    let attribute = entity.getAttributeMapping().getApiAttribute(query);

    // special case queries to fill in metadata
    if (options.queryType === 'attributes') {
        if (options.strategy === 'featured') {
            return Promise.resolve(entity.getColumns().filter(col => col.featured).map(col => {
                return { id: col.resource, value: col.text }
            }));
        }
        // assume all
        return entity.getProperties();
    } else if (options.queryType === 'comparators') {
        return entity.getPropertyComparators(attribute);
    } else if (options.queryType === 'operators') {
        return this.opennmsClient.findOperators();
    }

    if (attribute === undefined || attribute === null || attribute === '') {
        console.warn('entity-ds: metricFindQuery: no attribute specified');
        return Promise.resolve([]);
    }

    let e = entity;
    const functions = FunctionFormatter.findFunctions(attribute);
    for (const func of functions) {
        attribute = func.arguments[0] || 'id';
        if (func.name === 'alarms') {
            e = new AlarmEntity(entity.client, this);
        } else if (func.name === 'ipInterfaces') {
            e = new IpInterfaceEntity(entity.client, this);
        } else if (func.name === 'monitoredServices') {
            e = new MonitoredServiceEntity(entity.client, this);
        } else if (func.name === 'nodes') {
            e = new NodeEntity(entity.client, this);
        } else if (func.name === 'outages') {
            e = new OutageEntity(entity.client, this);
        } else if (func.name === 'snmpInterfaces') {
            e = new SnmpInterfaceEntity(entity.client, this);
        }        
    }

    return this.searchForValues(e, attribute).then(values => {
        console.debug('entity-ds: searchForValues (' + attribute + '):', values);
        return values;
    });
  }

  searchForValues(entity, attribute) {
      if (attribute === 'isSituation' || attribute === 'isInSituation' || attribute === 'isAcknowledged') {
        return Promise.resolve([{ id: 'false', label: 'false', text: 'false'}, {id: 'true', label: 'true', text: 'true'}]);
      }

      return entity.findProperty(attribute)
          .then(property => {
              if (!property) {
                  return Promise.resolve([]);
              }
              // Special handling for properties
              switch(property.id) {
                  // Severity is handled separately as otherwise the severity ordinal vs the severity label would be
                  // used, but that may not be ideal for the user
                  case 'severity':
                      return Promise.resolve(_.map(Model.Severities, severity => {
                          return {
                              id: severity.id,
                              label: severity.label
                          }
                      }));
              }
              return property.findValues({limit: 0}).then(values => {
                  return values.filter(value => value !== null).map(value => {
                      return {id: value, label: value, text: value ? String(value) : value, value: value}
                  });
              });
          });
    }

    metricFindLocations() {
        return this.simpleRequest.getLocations();
    }

    metricFindNodeFilterQuery(query) {
        return this.simpleRequest.getNodesByFilter(query);
    }

    getAlarm(alarmId) {
        return this.opennmsClient.getAlarm(alarmId);
    }

    acknowledgeAlarm(alarmId) {
        return this.opennmsClient.doAck(alarmId, this.user);
    }

    unacknowledgeAlarm(alarmId) {
        return this.opennmsClient.doUnack(alarmId, this.user);
    }

    clearAlarm(alarmId) {
        return this.opennmsClient.doClear(alarmId, this.user);
    }

    escalateAlarm(alarmId) {
        return this.opennmsClient.doEscalate(alarmId, this.user);
    }

    createTicketForAlarm(alarmId) {
        return this.opennmsClient.doTicketAction(alarmId, "create");
    }

    updateTicketForAlarm(alarmId) {
        return this.opennmsClient.doTicketAction(alarmId, "update");
    }

    closeTicketForAlarm(alarmId) {
        return this.opennmsClient.doTicketAction(alarmId, "close");
    }

    saveSticky(alarmId, sticky) {
        return this.opennmsClient.saveSticky(alarmId, sticky, this.user);
    }

    deleteSticky(alarmId) {
        return this.opennmsClient.deleteSticky(alarmId);
    }

    saveJournal(alarmId, journal) {
        return this.opennmsClient.saveJournal(alarmId, journal, this.user);
    }

    deleteJournal(alarmId) {
        return this.opennmsClient.deleteJournal(alarmId);
    }

    // Situation Feedback

    getSituationFeedback(situationId) {
        return this.opennmsClient.getSituationfeedback(situationId);
    }

    submitSituationFeedback(situationId, feedback) {
        return this.opennmsClient.submitSituationFeedback(situationId, feedback);
    }
}
