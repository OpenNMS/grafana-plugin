'use strict';

System.register([], function (_export, _context) {
  "use strict";

  var _createClass, OpenNMSFMDatasource;

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  return {
    setters: [],
    execute: function () {
      _createClass = function () {
        function defineProperties(target, props) {
          for (var i = 0; i < props.length; i++) {
            var descriptor = props[i];
            descriptor.enumerable = descriptor.enumerable || false;
            descriptor.configurable = true;
            if ("value" in descriptor) descriptor.writable = true;
            Object.defineProperty(target, descriptor.key, descriptor);
          }
        }

        return function (Constructor, protoProps, staticProps) {
          if (protoProps) defineProperties(Constructor.prototype, protoProps);
          if (staticProps) defineProperties(Constructor, staticProps);
          return Constructor;
        };
      }();

      _export('OpenNMSFMDatasource', OpenNMSFMDatasource = function () {
        function OpenNMSFMDatasource(instanceSettings, $q, backendSrv, templateSrv) {
          _classCallCheck(this, OpenNMSFMDatasource);

          this.type = instanceSettings.type;
          this.url = instanceSettings.url;
          this.name = instanceSettings.name;
          this.q = $q;
          this.backendSrv = backendSrv;
          this.templateSrv = templateSrv;
        }

        _createClass(OpenNMSFMDatasource, [{
          key: 'query',
          value: function query(options) {
            var self = this;
            return this.backendSrv.datasourceRequest({
              url: '/public/plugins/opennms-helm-app/datasources/mock-fault-ds/alarms.json',
              method: 'GET'
            }).then(function (response) {
              if (response.status === 200) {
                return { data: self.toTable(options, response.data) };
              }
            });
          }
        }, {
          key: 'toTable',
          value: function toTable(options, data) {
            var columns = [{
              "text": "Node Label"
            }, {
              "text": "Log Message"
            }, {
              "text": "Description"
            }, {
              "text": "UEI"
            }, {
              "text": "Node ID"
            }, {
              "text": "Acked By"
            }, {
              "text": "Severity"
            }, {
              "text": "First Event Time"
            }, {
              "text": "Last Event Time"
            }, {
              "text": "Event Source"
            }, {
              "text": "Count"
            }];

            var rows = [];
            for (var i = 0; i < data.alarm.length; i++) {
              var alarm = data.alarm[i];
              var row = [alarm.nodeLabel, alarm.logMessage, alarm.description, alarm.uei, alarm.nodeId, alarm.ackUser, alarm.severity, alarm.firstEventTime, alarm.lastEventTime, alarm.lastEvent.source, alarm.count];
              row.meta = {
                // Store the alarm for easy access by the panels
                'alarm': alarm,
                // Store the name of the data-source as part of the data so that
                // the panel can grab an instance of the DS to perform actions
                // on the alarms
                'source': this.name
              };
              rows.push(row);
            }

            return Array(options.targets.length).fill({
              "columns": columns,
              "rows": rows,
              "type": "table"
            });
          }
        }, {
          key: 'testDatasource',
          value: function testDatasource() {
            return this.q.when({ status: "success", message: "Data source is working", title: "Success" });
          }
        }, {
          key: 'annotationQuery',
          value: function annotationQuery(options) {
            return this.q.when({});
          }
        }, {
          key: 'metricFindQuery',
          value: function metricFindQuery(query) {
            return this.q.when({});
          }
        }, {
          key: 'acknowledgeAlarm',
          value: function acknowledgeAlarm(alarmId) {
            console.log("Ack", alarmId);
          }
        }, {
          key: 'unacknowledgeAlarm',
          value: function unacknowledgeAlarm(alarmId) {
            console.log("Unack", alarmId);
          }
        }, {
          key: 'clearAlarm',
          value: function clearAlarm(alarmId) {
            console.log("Clear", alarmId);
          }
        }, {
          key: 'escalateAlarm',
          value: function escalateAlarm(alarmId) {
            console.log("Escalate", alarmId);
          }
        }, {
          key: 'createTicketForAlarm',
          value: function createTicketForAlarm(alarmId) {
            console.log("Create ticket", alarmId);
          }
        }, {
          key: 'updateTicketForAlarm',
          value: function updateTicketForAlarm(alarmId) {
            console.log("Update ticket", alarmId);
          }
        }, {
          key: 'closeTicketForAlarm',
          value: function closeTicketForAlarm(alarmId) {
            console.log("Close ticket", alarmId);
          }
        }]);

        return OpenNMSFMDatasource;
      }());

      _export('OpenNMSFMDatasource', OpenNMSFMDatasource);
    }
  };
});
//# sourceMappingURL=datasource.js.map
