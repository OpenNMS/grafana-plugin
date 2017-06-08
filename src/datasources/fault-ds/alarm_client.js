import {OnmsSeverity} from './severities'

export class AlarmClientMock {

    constructor(settings, backendSrv, $q) {
        this.type = settings.type;
        this.url = settings.url;
        this.name = settings.name;
        this.backendSrv = backendSrv;
        this.searchLimit = 1000;
        this.$q = $q;
    }

    findAlarms(options) {
        var self = this;
        var params = {
            limit: options.limit || this.searchLimit,
            _s: options.search || void 0
        };
        return this.backendSrv.datasourceRequest({
            url: self.url + '/api/v2/alarms',
            method: 'GET',
            params : params
        }).then(response => {
            if (response.status === 200) {
                return response.data;
            }
            return [];
        })
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
        return this.$q.when(new OnmsSeverity().getSeverities());
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