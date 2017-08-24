import {API, Client, Rest, DAO} from '../../opennms'
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
                    // Ensure the OpenNMS we are talking to is compatible
                    if (metadata.apiVersion() !== 2) {
                        throw new Error("Unsupported Version");
                    }
                    self.client.server.metadata = metadata;
                    return self.client;
                }).catch(function(e) {
                    // in case of error, reset the client, otherwise
                    // the datasource may never recover
                    self.clientWithMetadata = void 0;
                    throw e;
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

    doEscalate(alarmId, user) {
        return this.getAlarmDao()
            .then(alarmDao => {
                return alarmDao.escalate(alarmId, user)
            });
    }

    doClear(alarmId, user) {
        return this.getAlarmDao()
            .then(alarmDao => {
                return alarmDao.clear(alarmId, user);
            });
    }

    doUnack(alarmId, user) {
        return this.getAlarmDao()
            .then(alarmDao => {
                return alarmDao.unacknowledge(alarmId, user);
            });
    }

    doAck(alarmId, user) {
        return this.getAlarmDao()
            .then(function(alarmDao) {
                return alarmDao.acknowledge(alarmId, user);
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

    saveSticky(alarmId, sticky, user) {
      return this.getAlarmDao()
        .then(function(alarmDao) {
          return alarmDao.saveStickyMemo(alarmId, sticky, user);
        });
    }

    deleteSticky(alarmId) {
      return this.getAlarmDao()
        .then(function(alarmDao) {
          return alarmDao.deleteStickyMemo(alarmId);
        });
    }

    saveJournal(alarmId, journal, user) {
      return this.getAlarmDao()
        .then(function(alarmDao) {
          return alarmDao.saveJournalMemo(alarmId, journal, user);
        });
    }

    deleteJournal(alarmId) {
      return this.getAlarmDao()
        .then(function(alarmDao) {
          return alarmDao.deleteJournalMemo(alarmId);
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