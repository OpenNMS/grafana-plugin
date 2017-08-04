'use strict';

System.register(['./client_delegate', '../../opennms', 'lodash'], function (_export, _context) {
    "use strict";

    var ClientDelegate, API, _, _createClass, OpenNMSFMDatasource;

    function _classCallCheck(instance, Constructor) {
        if (!(instance instanceof Constructor)) {
            throw new TypeError("Cannot call a class as a function");
        }
    }

    return {
        setters: [function (_client_delegate) {
            ClientDelegate = _client_delegate.ClientDelegate;
        }, function (_opennms) {
            API = _opennms.API;
        }, function (_lodash) {
            _ = _lodash.default;
        }],
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
                    this.alarmClient = new ClientDelegate(instanceSettings, backendSrv, $q);
                }

                _createClass(OpenNMSFMDatasource, [{
                    key: 'query',
                    value: function query(options) {
                        var filter = options.targets[0].filter || new API.Filter();
                        filter.limit = 0; // no limit
                        var self = this;
                        return this.alarmClient.findAlarms(filter).then(function (alarms) {
                            return {
                                data: self.toTable(alarms)
                            };
                        });
                    }
                }, {
                    key: 'testDatasource',
                    value: function testDatasource() {
                        return this.backendSrv.datasourceRequest({
                            url: this.url + '/rest/info',
                            method: 'GET'
                        }).then(function (response) {
                            if (response.status === 200) {
                                return { status: "success", message: "Data source is working", title: "Success" };
                            }
                        });
                    }
                }, {
                    key: 'annotationQuery',
                    value: function annotationQuery(options) {
                        return this.q.when([]);
                    }
                }, {
                    key: 'metricFindQuery',
                    value: function metricFindQuery(query) {
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
                }, {
                    key: 'searchForValues',
                    value: function searchForValues(query) {
                        var attribute = this.alarmClient.findAttribute(query.attribute) || {};
                        switch (attribute.type) {
                            case 'user':
                                return this.alarmClient.findUsers({ query: query.query }).then(function (data) {
                                    return _.map(data.rows, function (user) {
                                        return {
                                            id: user['user-id'],
                                            label: user['full-name']
                                        };
                                    });
                                });
                            case 'node':
                                return this.alarmClient.findNodes({ query: query.query }).then(function (data) {
                                    return _.map(data.rows, function (node) {
                                        return {
                                            id: node.id,
                                            label: node.label
                                        };
                                    });
                                });
                            case 'category':
                                return this.alarmClient.findCategories({ query: query.query }).then(function (data) {
                                    return _.map(data.rows, function (category) {
                                        return {
                                            id: category.id,
                                            label: category.name
                                        };
                                    });
                                });
                            case 'location':
                                return this.alarmClient.findLocations({ query: query.query }).then(function (data) {
                                    return _.map(data.rows, function (location) {
                                        return {
                                            id: location['location-name'],
                                            label: location['location-name']
                                        };
                                    });
                                });
                            case 'severity':
                                return this.alarmClient.findSeverities({ query: query.query }).then(function (data) {
                                    return _.map(data, function (severity) {
                                        return {
                                            id: severity.id,
                                            label: severity.label
                                        };
                                    });
                                });
                            case 'service':
                                return this.alarmClient.findServices({ query: query.query }).then(function (data) {
                                    return _.map(data.rows, function (service) {
                                        return {
                                            id: service,
                                            label: service
                                        };
                                    });
                                });
                        }
                        return this.q.when([]);
                    }
                }, {
                    key: 'toTable',
                    value: function toTable(alarms) {
                        var _this = this;

                        var columnNames = ["Log Message", "Description", "UEI", "Node ID", "Node Label", "IP Address", "Service", "Acked By", "Severity", "First Event Time", "Last Event Time", "Event Source", "Trouble Ticket", "Trouble Ticket State", "Count"];

                        var columns = _.map(columnNames, function (column) {
                            return { "text": column };
                        });

                        var rows = _.map(alarms, function (alarm) {
                            var row = [alarm.logMessage, alarm.description, alarm.uei, alarm.nodeId, alarm.nodeLabel, alarm.ipAddress, alarm.serviceType ? alarm.serviceType.name : undefined, alarm.ackUser, alarm.severity.label, alarm.firstEventTime, alarm.lastEventTime, alarm.lastEvent.source, alarm.troubleTicket, alarm.troubleTicketState, alarm.count];
                            row.meta = {
                                // Store the alarm for easy access by the panels - may not be necessary
                                'alarm': alarm,
                                // Store the name of the data-source as part of the data so that
                                // the panel can grab an instance of the DS to perform actions
                                // on the alarms
                                "source": _this.name
                            };
                            return row;
                        });

                        return [{
                            "columns": columns,
                            "rows": rows,
                            "type": "table"
                        }];
                    }
                }, {
                    key: 'acknowledgeAlarm',
                    value: function acknowledgeAlarm(alarmId) {
                        this.alarmClient.doUpdate(alarmId, { ack: true });
                    }
                }, {
                    key: 'unacknowledgeAlarm',
                    value: function unacknowledgeAlarm(alarmId) {
                        this.alarmClient.doUpdate(alarmId, { ack: false });
                    }
                }, {
                    key: 'clearAlarm',
                    value: function clearAlarm(alarmId) {
                        this.alarmClient.doUpdate(alarmId, { clear: true });
                    }
                }, {
                    key: 'escalateAlarm',
                    value: function escalateAlarm(alarmId) {
                        this.alarmClient.doUpdate(alarmId, { escalate: true });
                    }
                }, {
                    key: 'createTicketForAlarm',
                    value: function createTicketForAlarm(alarmId) {
                        this.alarmClient.doTicketAction(alarmId, "create");
                    }
                }, {
                    key: 'updateTicketForAlarm',
                    value: function updateTicketForAlarm(alarmId) {
                        this.alarmClient.doTicketAction(alarmId, "update");
                    }
                }, {
                    key: 'closeTicketForAlarm',
                    value: function closeTicketForAlarm(alarmId) {
                        this.alarmClient.doTicketAction(alarmId, "close");
                    }
                }]);

                return OpenNMSFMDatasource;
            }());

            _export('OpenNMSFMDatasource', OpenNMSFMDatasource);
        }
    };
});
//# sourceMappingURL=datasource.js.map
