'use strict';

System.register(['../../opennms', 'lodash'], function (_export, _context) {
    "use strict";

    var API, Client, Rest, DAO, _, _createClass, ClientDelegate;

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

                    var authConfig = undefined;
                    if (settings.basicAuth) {
                        // If basic auth is configured, pass the username and password to the client
                        // This allows the datasource to work in direct mode
                        // We need the raw username and password, so we decode the token
                        var token = settings.basicAuth.split(' ')[1];
                        var decodedToken = atob(token);
                        var username = decodedToken.split(':')[0];
                        var password = decodedToken.substring(username.length + 1, decodedToken.length);
                        authConfig = new API.OnmsAuthConfig(username, password);
                    }

                    var server = new API.OnmsServer(this.name, this.url, authConfig);
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
                            var client = Client.getMetadata(this.client.server, this.client.http).then(function (metadata) {
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
