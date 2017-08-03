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

    doUpdate(alarmId, options) {
        var self = this;
        return this.backendSrv.datasourceRequest({
            url: self.url + '/api/v2/alarms/' + alarmId,
            method: 'PUT',
            headers: {
                'Content-Type': 'application/x-www-form-urlencoded',
            },
            params: options || {},
            data: '' // empty data or Content-Type header is reset
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

    findNodes(options) {
        var self = this;
        return this.backendSrv.datasourceRequest({
                url: self.url + '/rest/nodes',
                method: 'GET',
                params: {
                    limit: options.limit || self.searchLimit,
                    match: 'any',
                    comparator: 'ilike',
                    orderBy: options.orderBy || 'id',
                    order: options.order || 'asc',
                    label: '%' + options.query + '%',
                    sysName: '%' + options.query + '%',
                    'ipInterface.ipAddress': '%' + options.query + '%',
                    'ipInterface.ipHostName': '%' + options.query + '%',
                    'foreignId': options.query + '%' // doesn't support leading '%'
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

    getAttributes() {
        let attributes = [
            { name: "uei", type: "string" },
            { name: "location", type: "location"},
            { name: "ipAddress", type: "ipaddress" },
            { name: "service", type: "service"},
            { name: "severity", type: "severity" },
            { name: "alarmAckTime", type: "date"}
            // TODO MVR add more ...
        ];
        return attributes;
    }

    findAttribute(attributeName) {
        return _.find(this.getAttributes(), function(attribute) {
            return attribute.name === attributeName;
        });
    }

    // TODO MVR we should get rid of this as well
    getAttributeComparators(attributeName) {
        var comparatorMapping = {
            'uei': ['=', '!='],
            'location': ['=', '!='],
            'severity': ['=', '>=', '<=', '>', '<', '!='],
            'service': ['!=', '='],
            'ipAddress': ['=', '!='],
            'alarmAckTime': ['=', '!=']
        };
        var comparators = comparatorMapping[attributeName];
        if (!comparators) {
            console.log("No comparators for attribute with name '" + attributeName + "' found.");
            return ['='];
        }
        return comparators;
    }
}