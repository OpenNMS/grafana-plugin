'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.FilterInitializer = exports.ClientDelegate = undefined;

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
                var theFilter = new FilterInitializer().createFilter(filter);
                return alarmDao.find(theFilter);
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
        key: 'findNodes',
        value: function findNodes(options) {
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
        key: 'getAttributes',
        value: function getAttributes() {
            var attributes = [{ name: "uei", type: "string" }, { name: "location", type: "location" }, { name: "ipAddress", type: "ipaddress" }, { name: "service", type: "service" }, { name: "severity", type: "severity" }, { name: "alarmAckTime", type: "date" }
            // TODO MVR add more ...
            ];
            return attributes;
        }
    }, {
        key: 'findAttribute',
        value: function findAttribute(attributeName) {
            return _lodash2.default.find(this.getAttributes(), function (attribute) {
                return attribute.name === attributeName;
            });
        }

        // TODO MVR we should get rid of this as well

    }, {
        key: 'getAttributeComparators',
        value: function getAttributeComparators(attributeName) {
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
    }]);

    return ClientDelegate;
}();

/**
 * The filter may be reloaded from a persisted state.
 * The internal opennms.js API requires a concrete implementation of Comparators or Operators in order to work.
 * As the object was persisted, the references DO NOT MATCH. In order to make them match, we just rebuild the filter.
 */


var FilterInitializer = exports.FilterInitializer = function () {
    function FilterInitializer() {
        _classCallCheck(this, FilterInitializer);
    }

    _createClass(FilterInitializer, [{
        key: 'createFilter',
        value: function createFilter(filter) {
            var newFilter = new _opennms.API.Filter();
            newFilter.limit = filter.limit;
            newFilter.clauses = this.createNestedRestriction(filter).clauses;
            return newFilter;
        }
    }, {
        key: 'createClause',
        value: function createClause(clause) {
            var operator = _lodash2.default.find(_opennms.API.Operators, function (operator) {
                return operator.label === clause.operator.label;
            });

            // Nested restriction
            if (clause.restriction.clauses) {
                var nestedRestriction = this.createNestedRestriction(clause.restriction);
                return new _opennms.API.Clause(nestedRestriction, operator);
            } else {
                // Normal restriction
                var restriction = this.createRestriction(clause.restriction);
                return new _opennms.API.Clause(restriction, operator);
            }
        }
    }, {
        key: 'createNestedRestriction',
        value: function createNestedRestriction(nestedRestriction) {
            var self = this;
            var newNestedRestriction = new _opennms.API.NestedRestriction();
            _lodash2.default.each(nestedRestriction.clauses, function (clause) {
                newNestedRestriction.withClause(self.createClause(clause));
            });
            return newNestedRestriction;
        }
    }, {
        key: 'createRestriction',
        value: function createRestriction(restriction) {
            var comparator = _lodash2.default.find(_opennms.API.Comparators, function (comparator) {
                return comparator.label === restriction.comparator.label;
            });
            return new _opennms.API.Restriction(restriction.attribute, comparator, restriction.value);
        }
    }]);

    return FilterInitializer;
}();
//# sourceMappingURL=client_delegate.js.map
