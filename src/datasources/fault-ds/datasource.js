export class OpenNMSFMDatasource {

  constructor(instanceSettings, $q, backendSrv, templateSrv) {
    this.type = instanceSettings.type;
    this.url = instanceSettings.url;
    this.name = instanceSettings.name;
    this.q = $q;
    this.backendSrv = backendSrv;
    this.templateSrv = templateSrv;
  }

  query(options) {
    if (!options.targets || options.targets.length < 1) {
      return this.q.when({data: []});
    }

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

  toTable(data) {
    var columns = [
      {
        "text": "Node Label",
      },
      {
        "text": "Log Message",
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
      },
      {
        "text": "Severity",
      },
      {
        "text": "First Event Time",
      },
      {
        "text": "Last Event Time",
      },
      {
        "text": "Event Source",
      },
      {
        "text": "Count",
      }
    ];

    var rows = [];
    for (var i = 0; i < data.alarm.length; i++) {
      var alarm = data.alarm[i];
      var row = [
        alarm.nodeLabel,
        alarm.logMessage,
        alarm.description,
        alarm.uei,
        alarm.nodeId,
        alarm.ackUser,
        alarm.severity,
        alarm.firstEventTime,
        alarm.lastEventTime,
        alarm.lastEvent.source,
        alarm.count
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
    return this.q.when({});
  }

  metricFindQuery(query) {
    return this.q.when({});
  }
}
