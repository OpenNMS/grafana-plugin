export class OpenNMSFMDatasource {

    // TODO MVR some pieces are copied over from the "grafana-opennms-datasource", e.g. fetching nodes, and the modal selection dialog
    // We should think about making this a "general, common or shared" project, which we can re-use here

  constructor(instanceSettings, $q, backendSrv, templateSrv) {
    this.type = instanceSettings.type;
    this.url = instanceSettings.url;
    this.name = instanceSettings.name;
    this.q = $q;
    this.backendSrv = backendSrv;
    this.templateSrv = templateSrv;
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

  getFields() {
    var fields = [
        { name: "id", label: "ID", type: "number" },
        { name: "uei", label: "UEI", type: "string" },
        { name: "location", label: "Location", type: "location"},
        { name: "nodeId", label: "Node ID", type: "node"},
        { name: "ipAddr", label: "Ip Address", type: "ipaddress" },
        { name: "service", label: "Service", type: "service"},
        { name: "reductionKey", label: "Reduction Key", type: "string" },
        { name: "ifIndex", label: "ifIndex", type: "number"},
        { name: "counter", label: "Counter", type: "number"},
        { name: "severity", label: "Severity", type: "severity" },
        { name: "firstEventTime", label: "First Event Time", type: "date"},
        { name: "lastEventTime", label: "Last Event Time", type: "date"},
        { name: "firstAutomationTime", label: "First Automation Time", type: "date"},
        { name: "description", label: "Description", type: "string"},
        { name: "logMsg", label: "Log Message", type: "string"},
        { name: "operInstruct", label: "Oper Instruct", type: "string"},
        { name: "ticketId", label: "Ticket Id", type: "string"},
        { name: "ticketState", label: "Ticket State", type: "string"},
        { name: "mouseOverText", label: "Mouse Over Text", type: "string"},
        { name: "suppressedUntil", label: "Suppressed Until", type: "date"},
        { name: "suppressedUser", label: "Suppressed User", type: "user"},
        { name: "alarmAcktime", label: "Acknowledged At", type: "date"},
        { name: "alarmAckUser", label: "Acknowledged User", type: "user"},
        { name: "clearKey", label: "Clear Key", type: "string"},
        // TODO MVR add more ...
        // TODO MVR add category
    ];
    return fields;
  }

  getFieldComparators(field) {
      if (!field || !field.type) {
          return [];
      }
      let type = field.type;
      let theType = type || '';
      let numberComparators = ["==", "!=" , ">=", ">", "<=", "<"];
      let generalComparators = ["like", "not like", "in", "not in"];
      let ipComparator = ["iplike"];

      if (theType == "number" || theType == "severity" || theType == "node") {
          return [].concat(numberComparators).concat(generalComparators);
      }
      if (theType == "ipaddress") {
          return [].concat(ipComparator).concat(generalComparators);
      }
      if (theType == "string") {
          return [].concat(generalComparators).concat(["==", "!="]);
      }
      return generalComparators;
  }

  toTable(data) {
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

    var rows = [];
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

    if (query.find === "fields") {
      return this.q.when(this.getFields());
    }
    if (query.find === "comparators") {
      return this.q.when(this.getFieldComparators(query.field));
    }
    return this.q.when([]);
  }
}
