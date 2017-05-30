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
        var theQuery = options.query;
        if (theQuery.length == 0) {
            theQuery = undefined;
        }
        return this.backendSrv.datasourceRequest({
            url: self.url + '/rest/alarms',
            method: 'GET',
            params : {
                query: theQuery,
                limit: options.limit
            }
        }).then(response => {
            if (response.status === 200) {
                return response.data;
            }
        })
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
        return this.$q.when([
            {id: 1, label: 'Indeterminate'},
            {id: 2, label: 'Cleared'},
            {id: 3, label: 'Normal'},
            {id: 4, label: 'Warning'},
            {id: 5, label: 'Minor'},
            {id: 6, label: 'Major'},
            {id: 7, label: 'Critical'},
        ]);
    }

    getAttributes() {
        let attributes = [
            { name: "alarmid", label: "ID", type: "number" },
            { name: "uei", label: "UEI", type: "string" },
            { name: "location", label: "Location", type: "location"},
            { name: "nodeId", label: "Node ID", type: "node"},
            { name: "ipAddress", label: "Ip Address", type: "ipaddress" },
            { name: "serviceType", label: "Service", type: "service"},
            { name: "reductionKey", label: "Reduction Key", type: "string" },
            { name: "ifIndex", label: "ifIndex", type: "number"},
            { name: "count", label: "Counter", type: "number"},
            { name: "severity", label: "Severity", type: "severity" },
            { name: "firstEventTime", label: "First Event Time", type: "date"},
            { name: "lastEventTime", label: "Last Event Time", type: "date"},
            { name: "description", label: "Description", type: "string"},
            { name: "logMessage", label: "Log Message", type: "string"},
            { name: "suppressedUntil", label: "Suppressed Until", type: "date"},
            { name: "category", label: "Category", type: "category"}
            // TODO MVR add more ...
            // TODO MVR add category
        ];
        return attributes;
    }

    findAttribute(attributeName) {
        return _.find(this.getAttributes(), function(attribute) {
            return attribute.name === attributeName;
        });
    }

    getAttributeComparators(attributeName) {
        let field = this.findAttribute(attributeName);
        if (!field || !field.type) {
            console.log("No comparators for attribute with name '" + attributeName + "' found.");
            return [];
        }
        let type = field.type;
        let theType = type || '';
        let numberComparators = ["=", "!=" , ">=", ">", "<=", "<"];
        let generalComparators = ["like", "not like", "in", "not in"];
        let ipComparator = ["iplike"];

        if (theType == "number" || theType == "severity" || theType == "node") {
            return [].concat(numberComparators).concat(generalComparators);
        }
        if (theType == "ipaddress") {
            return [].concat(ipComparator).concat(generalComparators);
        }
        if (theType == "string") {
            return [].concat(generalComparators).concat(["=", "!="]);
        }
        return generalComparators;
    }
}