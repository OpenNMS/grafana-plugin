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
            var _this = this;

            // Initialize filter
            var filter = options.targets[0].filter || new _opennms.API.Filter();
            filter.limit = 0; // no limit

            var clonedFilter = this.buildQuery(filter, options);

            var self = this;
            return this.alarmClient.findAlarms(clonedFilter).then(function (alarms) {
                return _this.alarmClient.getClientWithMetadata().then(function (client) {
                    return {
                        data: self.toTable(alarms, client.server.metadata)
                    };
                });
            });
        }

        // Clone Filter to prevent some issues and also make substitution possible
        // (otherwise substitution would happen in original query, and overwriting the $<variable> which may not be the intention)

    }, {
        key: 'buildQuery',
        value: function buildQuery(filter, options) {
            var clonedFilter = new _FilterCloner.FilterCloner().cloneFilter(filter);
            this.substitute(clonedFilter.clauses, options);
            return clonedFilter;
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
                        // Range must be of type date, otherwise it is not parseable by the OpenNMS client
                        if (clause.restriction.value === '$range_from' || clause.restriction.value === "[[range_from]]") {
                            clause.restriction.value = options.range.from;
                        } else if (clause.restriction.value === '$range_to' || clause.restriction.value === "[[range_to]]") {
                            clause.restriction.value = options.range.to;
                        } else {
                            clause.restriction.value = self.templateSrv.replace(clause.restriction.value, options.scopedVars);
                        }
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
    }, {
        key: 'searchForValues',
        value: function searchForValues(query) {
            var _this2 = this;

            return this.alarmClient.findProperty(query.attribute).then(function (property) {
                if (!property) {
                    return _this2.q.when([]);
                }
                switch (property.id) {
                    case 'alarmAckUser':
                    case 'suppressedUser':
                    case 'lastEvent.eventAckUser':
                        return _this2.alarmClient.findUsers({ query: query.query }).then(function (data) {
                            return _lodash2.default.map(data.rows, function (user) {
                                return {
                                    id: user['user-id'],
                                    label: user['full-name']
                                };
                            });
                        });
                    case 'node.label':
                        return _this2.alarmClient.findNodes({ query: query.query }).then(function (data) {
                            return _lodash2.default.map(data.rows, function (node) {
                                return {
                                    id: node.label,
                                    label: node.label
                                };
                            });
                        });
                    case 'category.name':
                        return _this2.alarmClient.findCategories({ query: query.query }).then(function (data) {
                            return _lodash2.default.map(data.rows, function (category) {
                                return {
                                    id: category.id,
                                    label: category.name
                                };
                            });
                        });
                    case 'location.locationName':
                        return _this2.alarmClient.findLocations({ query: query.query }).then(function (data) {
                            return _lodash2.default.map(data.rows, function (location) {
                                return {
                                    id: location['location-name'],
                                    label: location['location-name']
                                };
                            });
                        });
                    case 'severity':
                        return _this2.alarmClient.findSeverities({ query: query.query }).then(function (data) {
                            return _lodash2.default.map(data, function (severity) {
                                return {
                                    id: severity.id,
                                    label: severity.label
                                };
                            });
                        });
                    case 'serviceType.name':
                        return _this2.alarmClient.findServices({ query: query.query }).then(function (data) {
                            return _lodash2.default.map(data.rows, function (service) {
                                return {
                                    id: service,
                                    label: service
                                };
                            });
                        });
                }
            });
        }

        // Converts the data fetched from the Alarm REST Endpoint of OpenNMS to the grafana table model

    }, {
        key: 'toTable',
        value: function toTable(alarms, metadata) {
            var _this3 = this;

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
                    "source": _this3.name,
                    // Store the ticketerConfig here
                    "ticketerConfig": metadata.ticketerConfig
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
        key: 'getAlarm',
        value: function getAlarm(alarmId) {
            return this.alarmClient.getAlarm(alarmId);
        }
    }, {
        key: 'acknowledgeAlarm',
        value: function acknowledgeAlarm(alarmId) {
            return this.alarmClient.doAck(alarmId);
        }
    }, {
        key: 'unacknowledgeAlarm',
        value: function unacknowledgeAlarm(alarmId) {
            return this.alarmClient.doUnack(alarmId);
        }
    }, {
        key: 'clearAlarm',
        value: function clearAlarm(alarmId) {
            return this.alarmClient.doClear(alarmId);
        }
    }, {
        key: 'escalateAlarm',
        value: function escalateAlarm(alarmId) {
            return this.alarmClient.doEscalate(alarmId);
        }
    }, {
        key: 'createTicketForAlarm',
        value: function createTicketForAlarm(alarmId) {
            return this.alarmClient.doTicketAction(alarmId, "create");
        }
    }, {
        key: 'updateTicketForAlarm',
        value: function updateTicketForAlarm(alarmId) {
            return this.alarmClient.doTicketAction(alarmId, "update");
        }
    }, {
        key: 'closeTicketForAlarm',
        value: function closeTicketForAlarm(alarmId) {
            return this.alarmClient.doTicketAction(alarmId, "close");
        }
    }, {
        key: 'saveSticky',
        value: function saveSticky(alarmId, sticky) {
            return this.alarmClient.saveSticky(alarmId, sticky);
        }
    }, {
        key: 'deleteSticky',
        value: function deleteSticky(alarmId) {
            return this.alarmClient.deleteSticky(alarmId);
        }
    }, {
        key: 'saveJournal',
        value: function saveJournal(alarmId, journal) {
            return this.alarmClient.saveJournal(alarmId, journal);
        }
    }, {
        key: 'deleteJournal',
        value: function deleteJournal(alarmId) {
            return this.alarmClient.deleteJournal(alarmId);
        }
    }]);

    return OpenNMSFMDatasource;
}();
//# sourceMappingURL=datasource.js.map
