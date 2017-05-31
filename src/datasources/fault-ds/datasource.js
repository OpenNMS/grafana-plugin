import {AlarmClientMock} from './alarm_client';
import {AlarmQuery} from './alarm_query';
import _ from 'lodash';

export class OpenNMSFMDatasource {

    // TODO MVR some pieces are copied over from the "grafana-opennms-datasource", e.g. fetching nodes, and the modal selection dialog
    // We should think about making this a "general, common or shared" project, which we can re-use here

  constructor(instanceSettings, $q, backendSrv, templateSrv) {
    this.name = instanceSettings.name;
    this.q = $q;
    this.backendSrv = backendSrv;
    this.templateSrv = templateSrv;
    this.alarmClient = new AlarmClientMock(instanceSettings, backendSrv, $q);
  }

  query(options) {
      var query = new AlarmQuery(options.targets[0].restrictions).getRestrictionsAsQuery();
      // TODO MVR what about limiting the request? The rest endpoint enforces a default limit of 10 if not sent
      query.limit = 100000;
      query.match = 'any';
      query.comparator = 'ilike';

      var self = this;
    return this.alarmClient.findAlarms(query).then(function(data) {
        return {
            data: self.toTable(data)
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
    return this.q.when([]);
  }

  // TODO MVR revisit this and figure out if value/text can be set on the segment to have an id and a label set?!
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
    toTable(data) {
        var columnNames = [
            "ID", "Description", "UEI",
            "Node ID", "Node Label",
            "Severity", "IP Address", "Service"];

        var columns = _.map(columnNames, column => {
            return { "text" : column }
        });

        var rows = _.map(data.alarm, alarm => {
            var row = [
                alarm.id,
                alarm.description,
                alarm.uei,
                alarm.nodeId,
                alarm.nodeLabel,
                alarm.severity,
                alarm.ipAddress,
                alarm.serviceType ? alarm.serviceType.name : undefined
            ];
            row.meta = {
                // Store the alarm for easy access by the panels - may not be necessary
                'alarm': alarm
            };
            return row;
        });

        return [
            {
                "columns": columns,
                "rows": rows,
                "type": "table",
                // Store the name of the data-source as part of the data so that
                // the panel can grab an instance of the DS to perform actions
                // on the alarms
                "source": this.name
            }
        ];
    }
}
