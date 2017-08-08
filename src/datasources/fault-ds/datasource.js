import {ClientDelegate} from './client_delegate';
import {API} from '../../opennms';
import {FilterCloner} from "./FilterCloner"
import _ from 'lodash';

export class OpenNMSFMDatasource {

  constructor(instanceSettings, $q, backendSrv, templateSrv) {
    this.type = instanceSettings.type;
    this.url = instanceSettings.url;
    this.name = instanceSettings.name;
    this.q = $q;
    this.backendSrv = backendSrv;
    this.templateSrv = templateSrv;
    this.alarmClient = new ClientDelegate(instanceSettings, backendSrv, $q);
  }

  query(options) {
      // Initialize filter
      var filter = options.targets[0].filter || new API.Filter();
      filter.limit = 0; // no limit

      // Clone Filter to prevent some issues and also make substitution possible
      // (otherwise substitution would happen in original query, and overwriting the $<variable> which may not be the intention)
      var clonedFilter = new FilterCloner().cloneFilter(filter);
      this.substitute(clonedFilter.clauses, options);

      var self = this;
      return this.alarmClient.findAlarms(clonedFilter).then(function(alarms) {
          return {
              data: self.toTable(alarms)
          };
      });
  }

  substitute(clauses, options) {
      const self = this;
      _.each(clauses, clause => {
        if (clause.restriction) {
            if (clause.restriction instanceof API.NestedRestriction) {
                self.substitute(clause.restriction.clauses, options);
            } else if (clause.restriction.value) {
                // TODO MVR: This is a hint on how to probably best implement HELM-12
                // clause.restriction.value = clause.restriction.value.replace(/\$timeFrom/g, options.range.from.valueOf());
                // clause.restriction.value = clause.restriction.value.replace(/\$timeTo/g, options.range.to.valueOf());
                clause.restriction.value = self.templateSrv.replace(clause.restriction.value, options.scopedVars);
            }
        }
      });
  }

  testDatasource() {
    return this.backendSrv.datasourceRequest({
      url: this.url + '/rest/info',
      method: 'GET'
    }).then(response => {
      if (response.status === 200) {
        return {status: "success", message: "Data source is working", title: "Success"};
      }
    });
  }

  annotationQuery(options) {
    return this.q.when([]);
  }

  metricFindQuery(query) {
    if (!query || !query.find) {
        return this.q.when([]);
    }

    if (query.find === "attributes") {
      return this.alarmClient.getProperties();
    }
    if (query.find === "comparators") {
      return this.alarmClient.getPropertyComparators(query.attribute);
    }
    if (query.find == 'values') {
        return this.searchForValues(query);
    }
    if (query.find === 'operators') {
        return this.alarmClient.findOperators();
    }
    return this.q.when([]);
  }

  searchForValues(query) {
      return this.alarmClient.findProperty(query.attribute)
          .then(property => {
              if (!property) {
                  return this.q.when([]);
              }
              switch (property.id) {
                  case 'alarmAckUser':
                  case 'suppressedUser':
                  case 'lastEvent.eventAckUser':
                      return this.alarmClient.findUsers({query: query.query})
                          .then(function (data) {
                              return _.map(data.rows, function (user) {
                                  return {
                                      id: user['user-id'],
                                      label: user['full-name']
                                  };
                              });
                          });
                  case 'node.label':
                      return this.alarmClient.findNodes({query: query.query})
                          .then(function (data) {
                              return _.map(data.rows, function (node) {
                                  return {
                                      id: node.label,
                                      label: node.label
                                  }
                              });
                          });
                  case 'category.name':
                      return this.alarmClient.findCategories({query: query.query})
                          .then(function (data) {
                              return _.map(data.rows, function (category) {
                                  return {
                                      id: category.id,
                                      label: category.name
                                  };
                              })
                          });
                  case 'location.locationName':
                      return this.alarmClient.findLocations({query: query.query})
                          .then(function (data) {
                              return _.map(data.rows, function (location) {
                                  return {
                                      id: location['location-name'],
                                      label: location['location-name']
                                  };
                              })
                          });
                  case 'severity':
                      return this.alarmClient.findSeverities({query: query.query})
                          .then(function (data) {
                              return _.map(data, function (severity) {
                                  return {
                                      id: severity.id,
                                      label: severity.label
                                  }
                              })
                          });
                  case 'serviceType.name':
                      return this.alarmClient.findServices({query: query.query})
                          .then(function (data) {
                              return _.map(data.rows, function (service) {
                                  return {
                                      id: service,
                                      label: service
                                  }
                              })
                          });
          }
      });
  }

    // Converts the data fetched from the Alarm REST Endpoint of OpenNMS to the grafana table model
    toTable(alarms) {
        var columnNames = [
            "Log Message", "Description",
            "UEI", "Node ID", "Node Label",
            "IP Address", "Service", "Acked By", "Severity",
            "First Event Time", "Last Event Time", "Event Source",
            "Trouble Ticket", "Trouble Ticket State", "Count"];

        var columns = _.map(columnNames, column => {
            return { "text" : column }
        });

        var rows = _.map(alarms, alarm => {
            var row = [
                alarm.logMessage,
                alarm.description,
                alarm.uei,
                alarm.nodeId,
                alarm.nodeLabel,
                alarm.ipAddress,
                alarm.serviceType ? alarm.serviceType.name : undefined,
                alarm.ackUser,
                alarm.severity.label,
                alarm.firstEventTime,
                alarm.lastEventTime,
                alarm.lastEvent.source,
                alarm.troubleTicket,
                alarm.troubleTicketState,
                alarm.count
            ];
            row.meta = {
                // Store the alarm for easy access by the panels - may not be necessary
                'alarm': alarm,
                // Store the name of the data-source as part of the data so that
                // the panel can grab an instance of the DS to perform actions
                // on the alarms
                "source": this.name
            };
            return row;
        });

        return [
            {
                "columns": columns,
                "rows": rows,
                "type": "table",
            }
        ];
    }

    getAlarm(alarmId) {
        return this.alarmClient.getAlarm(alarmId);
    }

    acknowledgeAlarm(alarmId) {
      return this.alarmClient.doAck(alarmId);
    }

    unacknowledgeAlarm(alarmId) {
        this.alarmClient.doUnack(alarmId);
    }

    clearAlarm(alarmId) {
        this.alarmClient.doClear(alarmId);
    }

    escalateAlarm(alarmId) {
        this.alarmClient.doEscalate(alarmId);
    }

    createTicketForAlarm(alarmId) {
        this.alarmClient.doTicketAction(alarmId, "create");
    }

    updateTicketForAlarm(alarmId) {
        this.alarmClient.doTicketAction(alarmId, "update");
    }

    closeTicketForAlarm(alarmId) {
        this.alarmClient.doTicketAction(alarmId, "close");
    }

    saveSticky(alarmId, sticky) {
      return this.alarmClient.saveSticky(alarmId, sticky);
    }

    deleteSticky(alarmId) {
      return this.alarmClient.deleteSticky(alarmId);
    }

    saveJournal(alarmId, journal) {
      return this.alarmClient.saveJournal(alarmId, journal);
    }

    deleteJournal(alarmId) {
      return this.alarmClient.deleteJournal(alarmId);
    }
}
