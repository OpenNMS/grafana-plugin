export class AlarmClientMock {

    constructor(settings, backendSrv, $q) {
        this.type = settings.type;
        this.url = settings.url;
        this.name = settings.name;
        this.backendSrv = backendSrv;
        this.searchLimit = 1000;
        this.$q = $q;
    }

    findAlarms(query) {
        var self = this;
        return this.backendSrv.datasourceRequest({
            url: self.url + '/rest/alarms',
            method: 'GET',
            params : {} // TODO MVR convert query to valid query
        }).then(response => {
            if (response.status === 200) {
                return response.data;
            }
        })
    }

    findNodes(query) {
        var self = this;
        return this.backendSrv.datasourceRequest({
                url: self.url + '/rest/nodes',
                method: 'GET',
                params: {
                    limit: query.limit || self.searchLimit,
                    match: 'any',
                    comparator: 'ilike',
                    orderBy: query.orderBy || 'id',
                    order: query.order || 'asc',
                    label: '%' + query.query + '%',
                    sysName: '%' + query.query + '%',
                    'ipInterface.ipAddress': '%' + query.query + '%',
                    'ipInterface.ipHostName': '%' + query.query + '%',
                    'foreignId': query.query + '%' // doesn't support leading '%'
                }
            }).then(function (results) {
                    return {
                        'count': results.data.count,
                        'totalCount': results.data.totalCount,
                        'rows': results.data.node
                    };
                });
    }

    findUsers(query) {
        var self = this;
        return this.backendSrv.datasourceRequest({
            url: self.url + '/rest/users',
            method: 'GET',
            params: {
                limit: query.limit || self.searchLimit, // TODO MVR this is not implemented on the user rest service
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

    findCategories(query) {
        var self = this;
        return this.backendSrv.datasourceRequest({
            url: self.url + '/rest/categories',
            method: 'GET',
            params: {
                limit: query.limit || self.searchLimit, // TODO MVR this is not implemented on the rest service
            }
        }).then(function (results) {
            return {
                'count': results.data.count,
                'totalCount': results.data.totalCount,
                'rows': results.data.category
            };
        });
    }

    findSeverities(query) {
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
            { name: "id", label: "ID", type: "number" },
            { name: "uei", label: "UEI", type: "string" },
            { name: "location", label: "Location", type: "location"},
            { name: "nodeId", label: "Node ID", type: "node"},
            { name: "ipAddr", label: "Ip Address", type: "ipaddress" },
            { name: "service", label: "Service", type: "service"},
            { name: "reductionKey", label: "Reduction Key", type: "string" },
            { name: "ifIndex", label: "ifIndex", type: "number"},
            { name: "counter", label: "Counter", type: "number"},
            { name: "severity", label: "Severity", type: "severity" },
            { name: "firstEventTime", label: "First Event Time", type: "date"},
            { name: "lastEventTime", label: "Last Event Time", type: "date"},
            { name: "firstAutomationTime", label: "First Automation Time", type: "date"},
            { name: "description", label: "Description", type: "string"},
            { name: "logMsg", label: "Log Message", type: "string"},
            { name: "operInstruct", label: "Oper Instruct", type: "string"},
            { name: "ticketId", label: "Ticket Id", type: "string"},
            { name: "ticketState", label: "Ticket State", type: "string"},
            { name: "mouseOverText", label: "Mouse Over Text", type: "string"},
            { name: "suppressedUntil", label: "Suppressed Until", type: "date"},
            { name: "suppressedUser", label: "Suppressed User", type: "user"},
            { name: "alarmAcktime", label: "Acknowledged At", type: "date"},
            { name: "alarmAckUser", label: "Acknowledged User", type: "user"},
            { name: "clearKey", label: "Clear Key", type: "string"},
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
        let numberComparators = ["==", "!=" , ">=", ">", "<=", "<"];
        let generalComparators = ["like", "not like", "in", "not in"];
        let ipComparator = ["iplike"];

        if (theType == "number" || theType == "severity" || theType == "node") {
            return [].concat(numberComparators).concat(generalComparators);
        }
        if (theType == "ipaddress") {
            return [].concat(ipComparator).concat(generalComparators);
        }
        if (theType == "string") {
            return [].concat(generalComparators).concat(["==", "!="]);
        }
        return generalComparators;
    }
}