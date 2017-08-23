'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.ClientDelegate = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _opennms = require('../../opennms');

var _lodash = require('lodash');

var _lodash2 = _interopRequireDefault(_lodash);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var ClientDelegate = exports.ClientDelegate = function () {
    function ClientDelegate(settings, backendSrv, $q) {
        _classCallCheck(this, ClientDelegate);

        this.type = settings.type;
        this.url = settings.url;
        this.name = settings.name;
        this.backendSrv = backendSrv;
        this.searchLimit = 1000;
        this.$q = $q;

        var server = new _opennms.API.OnmsServer(this.name, this.url);
        var http = new _opennms.Rest.GrafanaHTTP(this.backendSrv, server);
        this.client = new _opennms.Client(http);
        this.client.server = server;
        this.clientWithMetadata = undefined;
    }

    _createClass(ClientDelegate, [{
        key: 'getClientWithMetadata',
        value: function getClientWithMetadata() {
            if (!this.clientWithMetadata) {
                var self = this;
                this.clientWithMetadata = _opennms.Client.getMetadata(this.client.server, this.client.http).then(function (metadata) {
                    // Ensure the OpenNMS we are talking to is compatible
                    if (metadata.apiVersion() !== 2) {
                        throw new Error("Unsupported Version");
                    }
                    self.client.server.metadata = metadata;
                    return self.client;
                }).catch(function (e) {
                    // in case of error, reset the client, otherwise
                    // the datasource may never recover
                    self.clientWithMetadata = void 0;
                    throw e;
                });
            }
            return this.clientWithMetadata;
        }
    }, {
        key: 'getAlarmDao',
        value: function getAlarmDao() {
            return this.getClientWithMetadata().then(function (client) {
                return client.alarms();
            });
        }
    }, {
        key: 'findAlarms',
        value: function findAlarms(filter) {
            return this.getAlarmDao().then(function (alarmDao) {
                return alarmDao.find(filter);
            });
        }
    }, {
        key: 'getAlarm',
        value: function getAlarm(alarmId) {
            return this.getAlarmDao().then(function (alarmDao) {
                return alarmDao.get(alarmId);
            });
        }
    }, {
        key: 'doEscalate',
        value: function doEscalate(alarmId, user) {
            return this.getAlarmDao().then(function (alarmDao) {
                return alarmDao.escalate(alarmId, user);
            });
        }
    }, {
        key: 'doClear',
        value: function doClear(alarmId, user) {
            return this.getAlarmDao().then(function (alarmDao) {
                return alarmDao.clear(alarmId, user);
            });
        }
    }, {
        key: 'doUnack',
        value: function doUnack(alarmId, user) {
            return this.getAlarmDao().then(function (alarmDao) {
                return alarmDao.unacknowledge(alarmId, user);
            });
        }
    }, {
        key: 'doAck',
        value: function doAck(alarmId, user) {
            return this.getAlarmDao().then(function (alarmDao) {
                return alarmDao.acknowledge(alarmId, user);
            });
        }
    }, {
        key: 'doTicketAction',
        value: function doTicketAction(alarmId, action) {
            var supportedActions = ["create", "update", "close"];
            if (supportedActions.indexOf(action) < 0) {
                throw { message: "Action '" + action + "' not supported." };
            }
            var self = this;
            return this.backendSrv.datasourceRequest({
                url: self.url + '/api/v2/alarms/' + alarmId + "/ticket/" + action,
                method: 'POST'
            });
        }
    }, {
        key: 'saveSticky',
        value: function saveSticky(alarmId, sticky, user) {
            return this.getAlarmDao().then(function (alarmDao) {
                return alarmDao.saveStickyMemo(alarmId, sticky, user);
            });
        }
    }, {
        key: 'deleteSticky',
        value: function deleteSticky(alarmId) {
            return this.getAlarmDao().then(function (alarmDao) {
                return alarmDao.deleteStickyMemo(alarmId);
            });
        }
    }, {
        key: 'saveJournal',
        value: function saveJournal(alarmId, journal, user) {
            return this.getAlarmDao().then(function (alarmDao) {
                return alarmDao.saveJournalMemo(alarmId, journal, user);
            });
        }
    }, {
        key: 'deleteJournal',
        value: function deleteJournal(alarmId) {
            return this.getAlarmDao().then(function (alarmDao) {
                return alarmDao.deleteJournalMemo(alarmId);
            });
        }
    }, {
        key: 'findNodes',
        value: function findNodes(options) {
            var self = this;
            return this.backendSrv.datasourceRequest({
                url: self.url + '/rest/nodes',
                method: 'GET',
                params: {
                    limit: options.limit || self.searchLimit
                }
            }).then(function (results) {
                return {
                    'count': results.data.count,
                    'totalCount': results.data.totalCount,
                    'rows': results.data.node
                };
            });
        }
    }, {
        key: 'findUsers',
        value: function findUsers(options) {
            var self = this;
            return this.backendSrv.datasourceRequest({
                url: self.url + '/rest/users',
                method: 'GET',
                params: {
                    limit: options.limit || self.searchLimit }
            }).then(function (results) {
                return {
                    'count': results.data.count,
                    'totalCount': results.data.totalCount,
                    'rows': results.data.user
                };
            });
        }
    }, {
        key: 'findLocations',
        value: function findLocations(query) {
            var self = this;
            return this.backendSrv.datasourceRequest({
                url: self.url + '/api/v2/monitoringLocations',
                method: 'GET',
                params: {
                    limit: query.limit || self.searchLimit
                }
            }).then(function (results) {
                return {
                    'count': results.data.count,
                    'totalCount': results.data.totalCount,
                    'rows': results.data.location
                };
            });
        }
    }, {
        key: 'findCategories',
        value: function findCategories(options) {
            var self = this;
            return this.backendSrv.datasourceRequest({
                url: self.url + '/rest/categories',
                method: 'GET',
                params: {
                    limit: options.limit || self.searchLimit }
            }).then(function (results) {
                return {
                    'count': results.data.count,
                    'totalCount': results.data.totalCount,
                    'rows': results.data.category
                };
            });
        }
    }, {
        key: 'findSeverities',
        value: function findSeverities(options) {
            var severities = _lodash2.default.map(_opennms.Model.Severities, function (severity) {
                return {
                    id: severity.id,
                    label: severity.label
                };
            });
            return this.$q.when(severities);
        }
    }, {
        key: 'findServices',
        value: function findServices(options) {
            var self = this;
            return this.backendSrv.datasourceRequest({
                url: self.url + '/rest/foreignSourcesConfig/services/default',
                method: 'GET',
                params: {
                    limit: options.limit || self.searchLimit
                }
            }).then(function (results) {
                return {
                    'count': results.data.count,
                    'totalCount': results.data.totalCount,
                    'rows': results.data.element
                };
            });
        }
    }, {
        key: 'findOperators',
        value: function findOperators() {
            var operators = _lodash2.default.map(_opennms.API.Operators, function (operator) {
                return {
                    id: operator.id,
                    label: operator.label
                };
            });
            return this.$q.when(operators);
        }
    }, {
        key: 'getProperties',
        value: function getProperties() {
            return this.getAlarmDao().then(function (alarmDao) {
                return alarmDao.searchProperties();
            });
        }

        // TODO MVR it would be nice to query the rest endpoint directly for the property, rather than queriing for all of the elements

    }, {
        key: 'findProperty',
        value: function findProperty(propertyId) {
            return this.getProperties().then(function (properties) {
                return _lodash2.default.find(properties, function (property) {
                    return property.id === propertyId;
                });
            });
        }
    }, {
        key: 'getPropertyComparators',
        value: function getPropertyComparators(propertyId) {
            return this.findProperty(propertyId).then(function (property) {
                if (property) {
                    var comparators = property.type.getComparators();
                    if (comparators && comparators.length > 0) {
                        return comparators;
                    }
                }
                console.log("No comparators found for property with id '" + propertyId + "'. Falling back to EQ.");
                // This may be the case when the user entered a property, which does not exist
                // therefore fallback to EQ
                return [_opennms.API.Comparators.EQ];
            });
        }
    }]);

    return ClientDelegate;
}();
//# sourceMappingURL=client_delegate.js.map
