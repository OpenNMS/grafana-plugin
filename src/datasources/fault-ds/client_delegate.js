import {API, Client, Rest, DAO, Model} from '../../opennms'
import _ from 'lodash';

export class ClientDelegate {

    constructor(settings, backendSrv, $q) {
        this.type = settings.type;
        this.url = settings.url;
        this.name = settings.name;
        this.backendSrv = backendSrv;
        this.searchLimit = 1000;
        this.$q = $q;

        let server = new API.OnmsServer(this.name, this.url);
        let http = new Rest.GrafanaHTTP(this.backendSrv, server);
        this.client = new Client(http);
        this.client.server = server;
        this.clientWithMetadata = undefined;
     }

    getClientWithMetadata() {
        if (!this.clientWithMetadata) {
              let self = this;
              this.clientWithMetadata = Client.getMetadata(this.client.server, this.client.http)
                .then(function(metadata) {
                    self.client.server.metadata = metadata;
                    return self.client;
                });
        }
        return this.clientWithMetadata;
      }

    getAlarmDao() {
        return this.getClientWithMetadata().then(function(client) {
            return client.alarms();
        });
    }

    findAlarms(filter) {
        return this.getAlarmDao()
            .then(function(alarmDao) {
                return alarmDao.find(filter);
            });
    }

    getAlarm(alarmId) {
      return this.getAlarmDao()
        .then(function(alarmDao) {
            return alarmDao.get(alarmId);
        });
    }

    doEscalate(alarmId) {
        return this.getAlarmDao()
            .then(alarmDao => {
                return alarmDao.escalate(alarmId)
            });
    }

    doClear(alarmId) {
        return this.getAlarmDao()
            .then(alarmDao => {
                return alarmDao.clear(alarmId);
            });
    }

    doUnack(alarmId) {
        return this.getAlarmDao()
            .then(alarmDao => {
                return alarmDao.unacknowledge(alarmId);
            });
    }

    doAck(alarmId) {
        return this.getAlarmDao()
            .then(function(alarmDao) {
                return alarmDao.acknowledge(alarmId);
            });
    }

    doTicketAction(alarmId, action) {
        var supportedActions = ["create", "update", "close"];
        if (supportedActions.indexOf(action) < 0) {
            throw {message: "Action '" + action + "' not supported."};
        }
        var self = this;
        return this.backendSrv.datasourceRequest({
            url: self.url + '/api/v2/alarms/' + alarmId + "/ticket/" + action,
            method: 'POST',
        });
    }

    saveSticky(alarmId, sticky) {
      return this.getAlarmDao()
        .then(function(alarmDao) {
          return alarmDao.saveStickyMemo(alarmId, sticky);
        });
    }

    deleteSticky(alarmId) {
      return this.getAlarmDao()
        .then(function(alarmDao) {
          return alarmDao.deleteStickyMemo(alarmId);
        });
    }

    saveJournal(alarmId, journal) {
      return this.getAlarmDao()
        .then(function(alarmDao) {
          return alarmDao.saveJournalMemo(alarmId, journal);
        });
    }

    deleteJournal(alarmId) {
      return this.getAlarmDao()
        .then(function(alarmDao) {
          return alarmDao.deleteJournalMemo(alarmId);
        });
    }

    findNodes(options) {
        var self = this;
        return this.backendSrv.datasourceRequest({
                url: self.url + '/rest/nodes',
                method: 'GET',
                params: {
                    limit: options.limit || self.searchLimit,
                }
            }).then(function (results) {
                    return {
                        'count': results.data.count,
                        'totalCount': results.data.totalCount,
                        'rows': results.data.node
                    };
                });
    }

    findUsers(options) {
        var self = this;
        return this.backendSrv.datasourceRequest({
            url: self.url + '/rest/users',
            method: 'GET',
            params: {
                limit: options.limit || self.searchLimit, // TODO MVR this is not implemented on the user rest service
            }
        }).then(function (results) {
            return {
                'count': results.data.count,
                'totalCount': results.data.totalCount,
                'rows': results.data.user
            };
        });
    }

    findLocations(query) {
        var self = this;
        return this.backendSrv.datasourceRequest({
            url: self.url + '/api/v2/monitoringLocations',
            method: 'GET',
            params: {
                limit: query.limit || self.searchLimit,
            }
        }).then(function (results) {
            return {
                'count': results.data.count,
                'totalCount': results.data.totalCount,
                'rows': results.data.location
            };
        });
    }

    findCategories(options) {
        var self = this;
        return this.backendSrv.datasourceRequest({
            url: self.url + '/rest/categories',
            method: 'GET',
            params: {
                limit: options.limit || self.searchLimit, // TODO MVR this is not implemented on the rest service
            }
        }).then(function (results) {
            return {
                'count': results.data.count,
                'totalCount': results.data.totalCount,
                'rows': results.data.category
            };
        });
    }

    findSeverities(options) {
        var severities = _.map(Model.Severities, function(severity) {
            return {
                id: severity.id,
                label: severity.label
            };
        });
        return this.$q.when(severities);
    }

    findServices(options) {
        var self = this;
        return this.backendSrv.datasourceRequest({
            url: self.url + '/rest/foreignSourcesConfig/services/default',
            method: 'GET',
            params : {
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

    findOperators() {
        var operators = _.map(API.Operators, function(operator) {
            return {
                id: operator.id,
                label: operator.label
            }
        });
        return this.$q.when(operators);
    }

    getProperties() {
        return this.getAlarmDao()
            .then(alarmDao => {
                return alarmDao.searchProperties();
            });
    }

    // TODO MVR it would be nice to query the rest endpoint directly for the property, rather than queriing for all of the elements
    findProperty(propertyId) {
        return this.getProperties()
            .then(properties => {
                return _.find(properties, function(property) {
                    return property.id === propertyId;
                });
            })
    }

    getPropertyComparators(propertyId) {
        return this.findProperty(propertyId)
            .then(property => {
                if (property) {
                    const comparators = property.type.getComparators();
                    if (comparators && comparators.length > 0) {
                        return comparators;
                    }
                }
                console.log("No comparators found for property with id '" + propertyId + "'. Falling back to EQ.");
                // This may be the case when the user entered a property, which does not exist
                // therefore fallback to EQ
                return [ API.Comparators.EQ ];
            });
    }
}