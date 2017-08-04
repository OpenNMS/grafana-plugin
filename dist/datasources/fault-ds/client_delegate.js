'use strict';

System.register(['../../opennms', 'lodash'], function (_export, _context) {
    "use strict";

    var API, Client, Rest, DAO, Model, _, _createClass, ClientDelegate;

    function _classCallCheck(instance, Constructor) {
        if (!(instance instanceof Constructor)) {
            throw new TypeError("Cannot call a class as a function");
        }
    }

    return {
        setters: [function (_opennms) {
            API = _opennms.API;
            Client = _opennms.Client;
            Rest = _opennms.Rest;
            DAO = _opennms.DAO;
            Model = _opennms.Model;
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

            _export('ClientDelegate', ClientDelegate = function () {
                function ClientDelegate(settings, backendSrv, $q) {
                    _classCallCheck(this, ClientDelegate);

                    this.type = settings.type;
                    this.url = settings.url;
                    this.name = settings.name;
                    this.backendSrv = backendSrv;
                    this.searchLimit = 1000;
                    this.$q = $q;

                    var server = new API.OnmsServer(this.name, this.url);
                    var http = new Rest.GrafanaHTTP(this.backendSrv, server);
                    this.client = new Client(http);
                    this.client.server = server;
                    this.clientWithMetadata = undefined;
                }

                _createClass(ClientDelegate, [{
                    key: 'getClientWithMetadata',
                    value: function getClientWithMetadata() {
                        if (!this.clientWithMetadata) {
                            var self = this;
                            this.clientWithMetadata = Client.getMetadata(this.client.server, this.client.http).then(function (metadata) {
                                self.client.server.metadata = metadata;
                                return self.client;
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
                    key: 'doUpdate',
                    value: function doUpdate(alarmId, options) {
                        var self = this;
                        return this.backendSrv.datasourceRequest({
                            url: self.url + '/api/v2/alarms/' + alarmId,
                            method: 'PUT',
                            headers: {
                                'Content-Type': 'application/x-www-form-urlencoded'
                            },
                            params: options || {},
                            data: '' // empty data or Content-Type header is reset
                        });
                    }
                }, {
                    key: 'doAck',
                    value: function doAck(alarmId) {
                        return this.getAlarmDao().then(function (alarmDao) {
                            return alarmDao.acknowledge(alarmId);
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
                    value: function saveSticky(alarmId, sticky) {
                        return this.getAlarmDao().then(function (alarmDao) {
                            return alarmDao.saveStickyMemo(alarmId, sticky);
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
                    value: function saveJournal(alarmId, journal) {
                        return this.getAlarmDao().then(function (alarmDao) {
                            return alarmDao.saveJournalMemo(alarmId, journal);
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
                        var severities = _.map(Model.Severities, function (severity) {
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
                        var operators = _.map(API.Operators, function (operator) {
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
                }, {
                    key: 'findProperty',
                    value: function findProperty(propertyId) {
                        return this.getProperties().then(function (properties) {
                            return _.find(properties, function (property) {
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
                            return [API.Comparators.EQ];
                        });
                    }
                }]);

                return ClientDelegate;
            }());

            _export('ClientDelegate', ClientDelegate);
        }
    };
});
//# sourceMappingURL=client_delegate.js.map
