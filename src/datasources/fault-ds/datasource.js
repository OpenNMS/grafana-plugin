import {ClientDelegate} from './client_delegate';
import {API} from '../../opennms';
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
      var filter = options.targets[0].filter || new API.Filter();
      filter.limit = 0; // no limit
      var self = this;
      return this.alarmClient.findAlarms(filter).then(function(alarms) {
          return {
              data: self.toTable(alarms)
          };
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
      return this.q.when(this.alarmClient.getAttributes());
    }
    if (query.find === "comparators") {
      return this.q.when(this.alarmClient.getAttributeComparators(query.attribute));
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
      let attribute = this.alarmClient.findAttribute(query.attribute) || {};
      switch (attribute.type) {
          case 'user':
              return this.alarmClient.findUsers({query: query.query})
                  .then(function(data) {
                      return _.map(data.rows, function(user) {
                          return {
                              id: user['user-id'],
                              label: user['full-name']
                          };
                      });
                  });
          case 'node':
              return this.alarmClient.findNodes({query: query.query})
                  .then(function(data) {
                      return _.map(data.rows, function(node) {
                        return {
                            id: node.id,
                            label: node.label
                        }
                      });
                  });
          case 'category':
              return this.alarmClient.findCategories({query: query.query})
                  .then(function(data) {
                      return _.map(data.rows, function(category) {
                          return {
                              id: category.id,
                              label: category.name
                          };
                      })
                  });
          case 'location':
              return this.alarmClient.findLocations({query: query.query})
                  .then(function(data) {
                      return _.map(data.rows, function(location) {
                          return {
                              id: location['location-name'],
                              label: location['location-name']
                          };
                      })
                  });
          case 'severity':
              return this.alarmClient.findSeverities({query: query.query})
                  .then(function(data) {
                      return _.map(data, function(severity) {
                          return {
                              id: severity.id,
                              label: severity.label
                          }
                      })
                  });
          case 'service':
              return this.alarmClient.findServices({query: query.query})
                  .then(function(data) {
                      return _.map(data.rows, function(service) {
                          return {
                              id: service,
                              label: service
                          }
                      })
                  })
      }
      return this.q.when([]);
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
        this.alarmClient.doUpdate(alarmId, {ack: false});
    }

    clearAlarm(alarmId) {
        this.alarmClient.doUpdate(alarmId, {clear: true});
    }

    escalateAlarm(alarmId) {
        this.alarmClient.doUpdate(alarmId, {escalate: true});
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
