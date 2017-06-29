import {Datasource} from "../datasources/mock-fault-ds/module";
import Q from "q";
import _ from 'lodash';

describe('OpenNMS_Mock_FaultManagement_Datasource', function() {
  let ctx = {};

  beforeEach(function() {
    ctx.$q = Q;
    ctx.backendSrv = {};
    ctx.templateSrv = {};
    ctx.ds = new Datasource({'name': 'mock'}, ctx.$q, ctx.backendSrv, ctx.templateSrv);

  });

  it('should return a table when queried', function (done) {
    ctx.backendSrv.datasourceRequest = function (request) {
      return ctx.$q.when({
        _request: request,
        status: 200,
        data: alarmDataResponse
      });
    };

    ctx.ds.query({targets:['foo']}).then(function (result) {
      expect(result.data).to.have.length(1);
      expect(result.data[0].columns).to.have.length.above(1);
      expect(result.data[0].rows).to.have.length(1);
      expect(result.data[0].type).to.eql('table');
      done();
    });
  });

  it('should include meta-data in the rows', function (done) {
    ctx.backendSrv.datasourceRequest = function (request) {
      return ctx.$q.when({
        _request: request,
        status: 200,
        data: alarmDataResponse
      });
    };

    ctx.ds.query({targets:['foo']}).then(function (result) {
      expect(result.data).to.have.length(1);
      expect(result.data[0].rows).to.have.length(1);
      _.each(result.data[0].rows, row => {
        // Datsource name
        expect(row.meta.source).to.eql('mock');
        // Alarm object
        expect(row.meta.alarm.id).to.be.above(0);
      });
      done();
    });
  });

  it('should support alarm actions', function() {
    ctx.ds.acknowledgeAlarm(1);
  });

  let alarmDataResponse = {
    "count": 1,
    "totalCount": 1,
    "offset": null,
    "alarm": [{
      "id": 13190,
      "description": "<p>High threshold exceeded for SNMP datasource\n            hrStorageUsed / hrStorageSize * 100.0 on interface 172.20.1.18, parms: label=\"/boot\" ds=\"hrStorageUsed / hrStorageSize * 100.0\" description=\"Trigger an alert when the percentage of disk space used reaches or goes above 90% for two consecutive measurement intervals (only for disks of type hrStorageFixedDisk, such as a locally attached or USB-attached hard disk)\" value=\"93.86\" instance=\"59\" instanceLabel=\"boot\" resourceId=\"node[17].hrStorageIndex[boot]\" threshold=\"90.0\" trigger=\"2\" rearm=\"75.0\"</p>\n            <p>By default, OpenNMS watches some key parameters\n            on devices in your network and will alert you with\n            an event if certain conditions arise. For example, if\n            the CPU utilization on your Cisco router maintains an\n            inordinately high percentage of utilization for an extended\n            period, an event will be generated. These thresholds are\n            determined and configured based on vendor recommendations,\n            tempered with real-world experience in working\n            deployments.</p> <p>This specific event\n            indicates that a high threshold was exceeded.</p>",
      "ifIndex": null,
      "uei": "uei.opennms.org/threshold/highThresholdExceeded",
      "nodeId": 17,
      "nodeLabel": "drive01.internal.opennms.com",
      "reductionKey": "uei.opennms.org/threshold/highThresholdExceeded::17:172.20.1.18:hrStorageUsed / hrStorageSize * 100.0:90.0:2:75.0:/boot",
      "x733AlarmType": null,
      "x733ProbableCause": 0,
      "serviceType": {
        "id": 5,
        "name": "SNMP"
      },
      "ackId": 13190,
      "firstEventTime": 1495016358338,
      "suppressedUntil": 1495016358338,
      "suppressedTime": 1495016358338,
      "lastEvent": {
        "id": 34615,
        "ifIndex": null,
        "nodeId": 17,
        "nodeLabel": "drive01.internal.opennms.com",
        "serviceType": {
          "id": 5,
          "name": "SNMP"
        },
        "ipAddress": "172.20.1.18",
        "time": 1495016358338,
        "uei": "uei.opennms.org/threshold/highThresholdExceeded",
        "host": "jw-dev-1",
        "source": "OpenNMS.Threshd.hrStorageUsed / hrStorageSize * 100.0",
        "createTime": 1495016358346,
        "description": "<p>High threshold exceeded for SNMP datasource\n            hrStorageUsed / hrStorageSize * 100.0 on interface 172.20.1.18, parms: label=\"/boot\" ds=\"hrStorageUsed / hrStorageSize * 100.0\" description=\"Trigger an alert when the percentage of disk space used reaches or goes above 90% for two consecutive measurement intervals (only for disks of type hrStorageFixedDisk, such as a locally attached or USB-attached hard disk)\" value=\"93.86\" instance=\"59\" instanceLabel=\"boot\" resourceId=\"node[17].hrStorageIndex[boot]\" threshold=\"90.0\" trigger=\"2\" rearm=\"75.0\"</p>\n            <p>By default, OpenNMS watches some key parameters\n            on devices in your network and will alert you with\n            an event if certain conditions arise. For example, if\n            the CPU utilization on your Cisco router maintains an\n            inordinately high percentage of utilization for an extended\n            period, an event will be generated. These thresholds are\n            determined and configured based on vendor recommendations,\n            tempered with real-world experience in working\n            deployments.</p> <p>This specific event\n            indicates that a high threshold was exceeded.</p>",
        "logMessage": "\n            High threshold exceeded for SNMP datasource hrStorageUsed / hrStorageSize * 100.0 on interface\n            172.20.1.18, parms: label=\"/boot\" ds=\"hrStorageUsed / hrStorageSize * 100.0\" description=\"Trigger an alert when the percentage of disk space used reaches or goes above 90% for two consecutive measurement intervals (only for disks of type hrStorageFixedDisk, such as a locally attached or USB-attached hard disk)\" value=\"93.86\" instance=\"59\" instanceLabel=\"boot\" resourceId=\"node[17].hrStorageIndex[boot]\" threshold=\"90.0\" trigger=\"2\" rearm=\"75.0\"\n        ",
        "log": "Y",
        "display": "Y",
        "severity": "WARNING",
        "parameters": [{
          "name": "label",
          "value": "/boot",
          "type": "string"
        }, {
          "name": "ds",
          "value": "hrStorageUsed / hrStorageSize * 100.0",
          "type": "string"
        }, {
          "name": "description",
          "value": "Trigger an alert when the percentage of disk space used reaches or goes above 90% for two consecutive measurement intervals (only for disks of type hrStorageFixedDisk, such as a locally attached or USB-attached hard disk)",
          "type": "string"
        }, {
          "name": "value",
          "value": "93.86",
          "type": "string"
        }, {
          "name": "instance",
          "value": "59",
          "type": "string"
        }, {
          "name": "instanceLabel",
          "value": "boot",
          "type": "string"
        }, {
          "name": "resourceId",
          "value": "node[17].hrStorageIndex[boot]",
          "type": "string"
        }, {
          "name": "threshold",
          "value": "90.0",
          "type": "string"
        }, {
          "name": "trigger",
          "value": "2",
          "type": "string"
        }, {
          "name": "rearm",
          "value": "75.0",
          "type": "string"
        }]
      },
      "lastEventTime": 1495016358338,
      "managedObjectInstance": null,
      "managedObjectType": null,
      "ossPrimaryKey": null,
      "qosAlarmState": null,
      "type": 1,
      "ipAddress": "172.20.1.18",
      "count": 1,
      "severity": "WARNING",
      "logMessage": "\n            High threshold exceeded for SNMP datasource hrStorageUsed / hrStorageSize * 100.0 on interface\n            172.20.1.18, parms: label=\"/boot\" ds=\"hrStorageUsed / hrStorageSize * 100.0\" description=\"Trigger an alert when the percentage of disk space used reaches or goes above 90% for two consecutive measurement intervals (only for disks of type hrStorageFixedDisk, such as a locally attached or USB-attached hard disk)\" value=\"93.86\" instance=\"59\" instanceLabel=\"boot\" resourceId=\"node[17].hrStorageIndex[boot]\" threshold=\"90.0\" trigger=\"2\" rearm=\"75.0\"\n        ",
      "parameters": [{
        "name": "label",
        "value": "/boot",
        "type": "string"
      }, {
        "name": "ds",
        "value": "hrStorageUsed / hrStorageSize * 100.0",
        "type": "string"
      }, {
        "name": "description",
        "value": "Trigger an alert when the percentage of disk space used reaches or goes above 90% for two consecutive measurement intervals (only for disks of type hrStorageFixedDisk, such as a locally attached or USB-attached hard disk)",
        "type": "string"
      }, {
        "name": "value",
        "value": "93.86",
        "type": "string"
      }, {
        "name": "instance",
        "value": "59",
        "type": "string"
      }, {
        "name": "instanceLabel",
        "value": "boot",
        "type": "string"
      }, {
        "name": "resourceId",
        "value": "node[17].hrStorageIndex[boot]",
        "type": "string"
      }, {
        "name": "threshold",
        "value": "90.0",
        "type": "string"
      }, {
        "name": "trigger",
        "value": "2",
        "type": "string"
      }, {
        "name": "rearm",
        "value": "75.0",
        "type": "string"
      }]
    }]
  };
});
