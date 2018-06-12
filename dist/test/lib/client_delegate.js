'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.ClientDelegate = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _opennms = require('../opennms');

var _lodash = require('lodash');

var _lodash2 = _interopRequireDefault(_lodash);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var Q = void 0;

var ClientDelegate = exports.ClientDelegate = function () {
    function ClientDelegate(settings, backendSrv, $q) {
        _classCallCheck(this, ClientDelegate);

        this.type = settings.type;
        this.url = settings.url;
        this.name = settings.name;
        this.backendSrv = backendSrv;
        this.searchLimit = 1000;
        this.$q = $q;
        if (!Q) {
            Q = $q;
        }

        if (settings.jsonData && settings.jsonData.timeout) {
            this.timeout = parseInt(settings.jsonData.timeout, 10) * 1000;
        }

        var authConfig = undefined;
        if (settings.basicAuth) {
            // If basic auth is configured, pass the username and password to the client
            // This allows the datasource to work in direct mode
            // We need the raw username and password, so we decode the token
            var token = settings.basicAuth.split(' ')[1];
            var decodedToken = atob(token);
            var username = decodedToken.split(':')[0];
            var password = decodedToken.substring(username.length + 1, decodedToken.length);
            authConfig = new _opennms.API.OnmsAuthConfig(username, password);
        }

        var server = new _opennms.API.OnmsServer(this.name, this.url, authConfig);
        var http = new _opennms.Rest.GrafanaHTTP(this.backendSrv, server, this.timeout);
        this.client = new _opennms.Client(http);
        this.client.server = server;
        this.clientWithMetadata = undefined;
    }

    _createClass(ClientDelegate, [{
        key: 'decorateError',
        value: function decorateError(err) {
            var ret = err;
            if (err.err) {
                ret = err.err;
            }
            if (err.data && err.data.err) {
                ret = err.data.err;
            }
            var statusText = 'Request failed.';

            // cancelled property causes the UI to never complete on failure
            if (err.cancelled) {
                statusText = 'Request timed out.';
                delete err.cancelled;
            }
            if (err.data && err.data.cancelled) {
                statusText = 'Request timed out.';
                delete err.data.cancelled;
            }

            if (!ret.message) {
                ret.message = ret.statusText || statusText;
            }
            if (!ret.status) {
                ret.status = 'error';
            }
            return Q.reject(ret);
        }
    }, {
        key: 'getClientWithMetadata',
        value: function getClientWithMetadata() {
            if (!this.clientWithMetadata) {
                var self = this;
                var client = _opennms.Client.getMetadata(self.client.server, self.client.http, self.timeout).then(function (metadata) {
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

                // Grafana functions that invoke the datasource expect the
                // promise to be one that is returned by $q.
                var deferred = this.$q.defer();
                client.then(function (success) {
                    return deferred.resolve(success);
                }).catch(function (error) {
                    return deferred.reject(error);
                });
                this.clientWithMetadata = deferred.promise;
            }
            return this.clientWithMetadata;
        }

        // Fault related functions

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
            }).catch(this.decorateError);
        }
    }, {
        key: 'getAlarm',
        value: function getAlarm(alarmId) {
            return this.getAlarmDao().then(function (alarmDao) {
                return alarmDao.get(alarmId);
            }).catch(this.decorateError);
        }
    }, {
        key: 'doEscalate',
        value: function doEscalate(alarmId, user) {
            return this.getAlarmDao().then(function (alarmDao) {
                return alarmDao.escalate(alarmId, user);
            }).catch(this.decorateError);
        }
    }, {
        key: 'doClear',
        value: function doClear(alarmId, user) {
            return this.getAlarmDao().then(function (alarmDao) {
                return alarmDao.clear(alarmId, user);
            }).catch(this.decorateError);
        }
    }, {
        key: 'doUnack',
        value: function doUnack(alarmId, user) {
            return this.getAlarmDao().then(function (alarmDao) {
                return alarmDao.unacknowledge(alarmId, user);
            }).catch(this.decorateError);
        }
    }, {
        key: 'doAck',
        value: function doAck(alarmId, user) {
            return this.getAlarmDao().then(function (alarmDao) {
                return alarmDao.acknowledge(alarmId, user);
            }).catch(this.decorateError);
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
            }).catch(this.decorateError);
        }
    }, {
        key: 'saveSticky',
        value: function saveSticky(alarmId, sticky, user) {
            return this.getAlarmDao().then(function (alarmDao) {
                return alarmDao.saveStickyMemo(alarmId, sticky, user);
            }).catch(this.decorateError);
        }
    }, {
        key: 'deleteSticky',
        value: function deleteSticky(alarmId) {
            return this.getAlarmDao().then(function (alarmDao) {
                return alarmDao.deleteStickyMemo(alarmId);
            }).catch(this.decorateError);
        }
    }, {
        key: 'saveJournal',
        value: function saveJournal(alarmId, journal, user) {
            return this.getAlarmDao().then(function (alarmDao) {
                return alarmDao.saveJournalMemo(alarmId, journal, user);
            }).catch(this.decorateError);
        }
    }, {
        key: 'deleteJournal',
        value: function deleteJournal(alarmId) {
            return this.getAlarmDao().then(function (alarmDao) {
                return alarmDao.deleteJournalMemo(alarmId);
            }).catch(this.decorateError);
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
            }).catch(this.decorateError);
        }
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
            }).catch(this.decorateError);
        }

        // Flow related functions

    }, {
        key: 'getFlowDao',
        value: function getFlowDao() {
            return this.getClientWithMetadata().then(function (c) {
                return c.flows();
            }).catch(this.decorateError);
        }
    }, {
        key: 'getSeriesForTopNApplications',
        value: function getSeriesForTopNApplications(N, start, end, step, includeOther, nodeCriteria, interfaceId) {
            return this.getFlowDao().then(function (flowDao) {
                return flowDao.getSeriesForTopNApplications(N, start, end, step, includeOther, nodeCriteria, interfaceId);
            }).catch(this.decorateError);
        }
    }, {
        key: 'getSeriesForTopNConversations',
        value: function getSeriesForTopNConversations(N, start, end, step, nodeCriteria, interfaceId) {
            return this.getFlowDao().then(function (flowDao) {
                return flowDao.getSeriesForTopNConversations(N, start, end, step, nodeCriteria, interfaceId);
            }).catch(this.decorateError);
        }
    }, {
        key: 'getSummaryForTopNApplications',
        value: function getSummaryForTopNApplications(N, start, end, includeOther, nodeCriteria, interfaceId) {
            return this.getFlowDao().then(function (flowDao) {
                return flowDao.getSummaryForTopNApplications(N, start, end, includeOther, nodeCriteria, interfaceId);
            }).catch(this.decorateError);
        }
    }, {
        key: 'getSummaryForTopNConversations',
        value: function getSummaryForTopNConversations(N, start, end, nodeCriteria, interfaceId) {
            return this.getFlowDao().then(function (flowDao) {
                return flowDao.getSummaryForTopNConversations(N, start, end, nodeCriteria, interfaceId);
            }).catch(this.decorateError);
        }
    }, {
        key: 'getExporters',
        value: function getExporters() {
            var searchLimit = this.searchLimit;
            return this.getFlowDao().then(function (flowDao) {
                return flowDao.getExporters(searchLimit);
            }).catch(this.decorateError);
        }
    }, {
        key: 'getExporter',
        value: function getExporter(nodeCriteria) {
            var searchLimit = this.searchLimit;
            return this.getFlowDao().then(function (flowDao) {
                return flowDao.getExporter(nodeCriteria, searchLimit);
            }).catch(this.decorateError);
        }
    }]);

    return ClientDelegate;
}();
//# sourceMappingURL=client_delegate.js.map
