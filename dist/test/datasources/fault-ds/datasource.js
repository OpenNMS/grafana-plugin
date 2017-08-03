'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.OpenNMSFMDatasource = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _client_delegate = require('./client_delegate');

var _opennms = require('../../opennms');

var _FilterCloner = require('./FilterCloner');

var _lodash = require('lodash');

var _lodash2 = _interopRequireDefault(_lodash);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var OpenNMSFMDatasource = exports.OpenNMSFMDatasource = function () {
    function OpenNMSFMDatasource(instanceSettings, $q, backendSrv, templateSrv) {
        _classCallCheck(this, OpenNMSFMDatasource);

        this.type = instanceSettings.type;
        this.url = instanceSettings.url;
        this.name = instanceSettings.name;
        this.q = $q;
        this.backendSrv = backendSrv;
        this.templateSrv = templateSrv;
        this.alarmClient = new _client_delegate.ClientDelegate(instanceSettings, backendSrv, $q);
    }

    _createClass(OpenNMSFMDatasource, [{
        key: 'query',
        value: function query(options) {
            // Initialize filter
            var filter = options.targets[0].filter || new _opennms.API.Filter();
            filter.limit = 0; // no limit

            // Clone Filter to prevent some issues and also make substitution possible
            // (otherwise substitution would happen in original query, and overwriting the $<variable> which may not be the intention)
            var clonedFilter = new _FilterCloner.FilterCloner().cloneFilter(filter);
            this.substitute(clonedFilter.clauses, options);

            var self = this;
            return this.alarmClient.findAlarms(clonedFilter).then(function (alarms) {
                return {
                    data: self.toTable(alarms)
                };
            });
        }
    }, {
        key: 'substitute',
        value: function substitute(clauses, options) {
            var self = this;
            _lodash2.default.each(clauses, function (clause) {
                if (clause.restriction) {
                    if (clause.restriction instanceof _opennms.API.NestedRestriction) {
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
                        return _lodash2.default.map(data.rows, function (user) {
                            return {
                                id: user['user-id'],
                                label: user['full-name']
                            };
                        });
                    });
                case 'node':
                    return this.alarmClient.findNodes({ query: query.query }).then(function (data) {
                        return _lodash2.default.map(data.rows, function (node) {
                            return {
                                id: node.id,
                                label: node.label
                            };
                        });
                    });
                case 'category':
                    return this.alarmClient.findCategories({ query: query.query }).then(function (data) {
                        return _lodash2.default.map(data.rows, function (category) {
                            return {
                                id: category.id,
                                label: category.name
                            };
                        });
                    });
                case 'location':
                    return this.alarmClient.findLocations({ query: query.query }).then(function (data) {
                        return _lodash2.default.map(data.rows, function (location) {
                            return {
                                id: location['location-name'],
                                label: location['location-name']
                            };
                        });
                    });
                case 'severity':
                    return this.alarmClient.findSeverities({ query: query.query }).then(function (data) {
                        return _lodash2.default.map(data, function (severity) {
                            return {
                                id: severity.id,
                                label: severity.label
                            };
                        });
                    });
                case 'service':
                    return this.alarmClient.findServices({ query: query.query }).then(function (data) {
                        return _lodash2.default.map(data.rows, function (service) {
                            return {
                                id: service,
                                label: service
                            };
                        });
                    });
            }
            return this.q.when([]);
        }

        // Converts the data fetched from the Alarm REST Endpoint of OpenNMS to the grafana table model

    }, {
        key: 'toTable',
        value: function toTable(alarms) {
            var _this = this;

            var columnNames = ["Log Message", "Description", "UEI", "Node ID", "Node Label", "IP Address", "Service", "Acked By", "Severity", "First Event Time", "Last Event Time", "Event Source", "Trouble Ticket", "Trouble Ticket State", "Count"];

            var columns = _lodash2.default.map(columnNames, function (column) {
                return { "text": column };
            });

            var rows = _lodash2.default.map(alarms, function (alarm) {
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
}();
//# sourceMappingURL=datasource.js.map
