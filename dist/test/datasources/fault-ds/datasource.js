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

var FeaturedAttributes = ["alarmAckTime", "category", "ipAddress", "location", "node.label", "reductionKey", "service", "severity", "uei"];

var OpenNMSFMDatasource = exports.OpenNMSFMDatasource = function () {
    function OpenNMSFMDatasource(instanceSettings, $q, backendSrv, templateSrv, contextSrv) {
        _classCallCheck(this, OpenNMSFMDatasource);

        this.type = instanceSettings.type;
        this.url = instanceSettings.url;
        this.name = instanceSettings.name;
        this.q = $q;
        this.backendSrv = backendSrv;
        this.templateSrv = templateSrv;
        this.alarmClient = new _client_delegate.ClientDelegate(instanceSettings, backendSrv, $q);

        // When enabled in the datasource, the grafana user should be used instead of the datasource username on
        // supported operations
        if (instanceSettings.jsonData && instanceSettings.jsonData.useGrafanaUser) {
            // If the datasource contains the field which should be used and that field is set, use it
            if (instanceSettings.jsonData.grafanaUserField && contextSrv.user[instanceSettings.jsonData.grafanaUserField]) {
                this.user = contextSrv.user[instanceSettings.jsonData.grafanaUserField];
            } else {
                // otherwise the login is used instead
                this.user = contextSrv.user.login;
            }
        }
    }

    _createClass(OpenNMSFMDatasource, [{
        key: 'query',
        value: function query(options) {
            var _this = this;

            // Initialize filter
            var filter = options.targets[0].filter || new _opennms.API.Filter();
            filter.limit = 0; // no limit

            options.enforceTimeRange = true;
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

        // Clone Filter to make substitution possible
        // (otherwise substitution would happen in original query,
        // and overwriting the $<variable> or [[variable]] in restrictions which may not be the intention)

    }, {
        key: 'buildQuery',
        value: function buildQuery(filter, options) {
            var clonedFilter = new _FilterCloner.FilterCloner().cloneFilter(filter);

            // Before replacing any variables, add a global time range restriction (which is hidden to the user)
            if (options && options.enforceTimeRange) {
                clonedFilter.withAndRestriction(new _opennms.API.NestedRestriction().withAndRestriction(new _opennms.API.Restriction("lastEventTime", _opennms.API.Comparators.GE, "$range_from")).withAndRestriction(new _opennms.API.Restriction("lastEventTime", _opennms.API.Comparators.LE, "$range_to")));
            }

            // Substitute $<variable> or [[variable]] in the restriction value
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
            return this.alarmClient.getClientWithMetadata().then(function (metadata) {
                if (metadata) {
                    return {
                        status: "success",
                        message: "Data source is working",
                        title: "Success"
                    };
                }
            }).catch(function (e) {
                if (e.message === "Unsupported Version") {
                    return {
                        status: "danger",
                        message: "The OpenNMS version you are trying to connect to is not supported. " + "OpenNMS Horizon version >= 21.0.0 or OpenNMS Meridian version >= 2017.1.0 is required.",
                        title: e.message
                    };
                } else {
                    throw e;
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
                if (query.strategy === 'featured') {
                    var featuredAttributes = _lodash2.default.map(_lodash2.default.sortBy(FeaturedAttributes), function (attribute) {
                        return { id: attribute };
                    });
                    return this.q.when(featuredAttributes);
                }
                // assume all
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

            var columnNames = ["ID", "Count", "Acked By", "Ack Time", "UEI", "Severity", "Type", "Description", "Log Message", "Reduction Key", "Trouble Ticket", "Trouble Ticket State", "Node ID", "Node Label", "Service", "Suppressed Time", "Suppressed Until", "Suppressed By", "IP Address", "First Event Time", "Last Event ID", "Last Event Time", "Last Event Source", "Last Event Creation Time", "Last Event Severity", "Sticky ID", "Sticky Note", "Sticky Author", "Sticky Update Time", "Sticky Creation Time", "Journal ID", "Journal Note", "Journal Author", "Journal Update Time", "Journal Creation Time"];

            var columns = _lodash2.default.map(columnNames, function (column) {
                return { "text": column };
            });

            var rows = _lodash2.default.map(alarms, function (alarm) {
                var row = [alarm.id, alarm.count, alarm.ackUser, alarm.ackTime, alarm.uei, alarm.severity.label, alarm.type ? alarm.type.label : undefined, alarm.description, alarm.logMessage, alarm.reductionKey, alarm.troubleTicket, alarm.troubleTicketState, alarm.nodeId, alarm.nodeLabel, alarm.service ? alarm.service.name : undefined, alarm.suppressedTime, alarm.suppressedUntil, alarm.suppressedBy, alarm.ipAddress,

                // Event
                alarm.firstEventTime, alarm.lastEvent ? alarm.lastEvent.id : undefined, alarm.lastEvent ? alarm.lastEvent.time : undefined, alarm.lastEvent ? alarm.lastEvent.source : undefined, alarm.lastEvent ? alarm.lastEvent.createTime : undefined, alarm.lastEvent ? alarm.lastEvent.severity.label : undefined,

                // Sticky Note
                alarm.sticky ? alarm.sticky.id : undefined, alarm.sticky ? alarm.sticky.body : undefined, alarm.sticky ? alarm.sticky.author : undefined, alarm.sticky ? alarm.sticky.updated : undefined, alarm.sticky ? alarm.sticky.created : undefined,

                // Journal Note
                alarm.journal ? alarm.journal.id : undefined, alarm.journal ? alarm.journal.body : undefined, alarm.journal ? alarm.journal.author : undefined, alarm.journal ? alarm.journal.updated : undefined, alarm.journal ? alarm.journal.created : undefined];

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
            return this.alarmClient.doAck(alarmId, this.user);
        }
    }, {
        key: 'unacknowledgeAlarm',
        value: function unacknowledgeAlarm(alarmId) {
            return this.alarmClient.doUnack(alarmId, this.user);
        }
    }, {
        key: 'clearAlarm',
        value: function clearAlarm(alarmId) {
            return this.alarmClient.doClear(alarmId, this.user);
        }
    }, {
        key: 'escalateAlarm',
        value: function escalateAlarm(alarmId) {
            return this.alarmClient.doEscalate(alarmId, this.user);
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
            return this.alarmClient.saveSticky(alarmId, sticky, this.user);
        }
    }, {
        key: 'deleteSticky',
        value: function deleteSticky(alarmId) {
            return this.alarmClient.deleteSticky(alarmId);
        }
    }, {
        key: 'saveJournal',
        value: function saveJournal(alarmId, journal) {
            return this.alarmClient.saveJournal(alarmId, journal, this.user);
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
