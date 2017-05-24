import {AlarmClientMock} from './alarmClient';
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
    var self = this;
    return this.backendSrv.datasourceRequest({
      url: '/public/plugins/opennms-helm-app/datasources/fault-ds/alarms.json',
      method: 'GET'
    }).then(response => {
      if (response.status === 200) {
        return {data: self.toTable(response.data)};
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

  searchForValues(query) {
      let attribute = this.alarmClient.findAttribute(query.attribute) || {};
      switch (attribute.type) {
          case 'user':
              return this.alarmClient.findUsers({query: query.query});
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
              return this.alarmClient.findCategories({query: query.query});
          case 'location':
              return this.alarmClient.findLocations({query: query.query});
          case 'severity':
              return this.alarmClient.findSeverities({query: query.query});
      }
      return this.q.when([]);
  }

    toTable(data) {
        // let columns = _.map(attributes, function(attribute) {
        //     return {
        //         'text' : attribute.label,
        //         'type' : attribute.type,
        //     }
        // });
        var columns = [
            {
                "text": "ID",
            },
            {
                "text": "Description",
            },
            {
                "text": "UEI",
            },
            {
                "text": "Node ID",
            },
            {
                "text": "Acked By",
            }
        ];

        let rows = [];
        for (var i = 0; i < data.alarm.length; i++) {
            var alarm = data.alarm[i];
            var row = [
                alarm.id,
                alarm.description,
                alarm.uei,
                alarm.nodeId,
                alarm.ackUser
            ];
            row.meta = {
                // Store the alarm for easy access by the panels - may not be necessary
                'alarm': alarm
            };
            rows.push(row);
        }

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
