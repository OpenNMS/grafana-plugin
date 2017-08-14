'use strict';

var _angular = require('angular');

var _angular2 = _interopRequireDefault(_angular);

var _lodash = require('lodash');

var _lodash2 = _interopRequireDefault(_lodash);

var _ComparatorMapping = require('./mapping/ComparatorMapping');

var _UI = require('./UI');

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

_angular2.default.module('grafana.directives').directive('onmsQuery', function () {
    return {
        templateUrl: 'public/plugins/opennms-helm-app/datasources/fault-ds/partials/query.html',
        controller: 'QueryController',
        restrict: 'EA',
        controllerAs: 'ctrl',
        scope: {
            query: "=", // The ui query object
            datasource: "=", // The datasource
            queryCtrl: "=" }
    };
}).controller('QueryController', function ($scope, uiSegmentSrv, $q) {
    var datasource = $scope.datasource;
    var QueryCtrl = $scope.queryCtrl;
    $scope.query.updateControls();

    $scope.findOperators = function (attribute) {
        return datasource.metricFindQuery({ 'find': 'comparators', 'attribute': attribute }).then(function (comparators) {
            // the API.Comparator.id or API.Comparator.label fields cannot be used.
            comparators = _lodash2.default.filter(comparators, function (comparator) {
                return comparator.aliases && comparator.aliases.length > 0;
            });
            return _lodash2.default.map(comparators, function (comparator) {
                var uiComparator = new _ComparatorMapping.ComparatorMapping().getUiComparator(comparator);
                return uiSegmentSrv.newOperator(uiComparator);
            });
        }).catch(QueryCtrl.handleQueryError.bind(QueryCtrl));
    };

    $scope.getSuggestions = function (clause, segment, index) {
        var segments = clause.restriction.segments;

        // attribute input
        if (segment.type == 'key' || segment.type == 'plus-button') {
            return datasource.metricFindQuery({ find: "attributes" }).then(function (properties) {
                var segments = _lodash2.default.map(properties, function (property) {
                    var segment = uiSegmentSrv.newKey(property.id);
                    return segment;
                });
                return segments;
            }).catch(QueryCtrl.handleQueryError.bind(QueryCtrl));
        }

        // comparator input
        if (segment.type == 'operator') {
            var attributeSegment = segments[index - 1];
            return $scope.findOperators(attributeSegment.value);
        }

        // value input
        if (segment.type == 'value') {
            var _attributeSegment = segments[index - 2];
            var theQuery = {
                'find': 'values',
                'attribute': _attributeSegment.value,
                'query': segment.value === _UI.UI.Restriction.VALUE_PLACEHOLDER ? '' : segment.value
            };

            return datasource.metricFindQuery(theQuery).then(function (values) {
                return _lodash2.default.map(values, function (searchResult) {
                    var segment = uiSegmentSrv.newKeyValue(searchResult.label);
                    return segment;
                });
            }).catch(QueryCtrl.handleQueryError.bind(QueryCtrl));
        }

        // condition input
        if (segment.type === 'condition') {
            return this.datasource.metricFindQuery({ find: 'operators' }).then(function (operators) {
                return _lodash2.default.map(operators, function (operator) {
                    return uiSegmentSrv.newCondition(operator.label);
                });
            }).catch(QueryCtrl.handleQueryError.bind(QueryCtrl));
        }
        return $q.when([]);
    };

    $scope.segmentUpdated = function (clause, segment, segmentIndex) {
        $scope.query.segmentUpdated(clause, segment, segmentIndex);
        $scope.query.updateControls();
        QueryCtrl.updateTargetFilter();

        // After the update it must be verified that the comparator is still in the list of comparators.
        // If not, the first comparator in the list is fallen back to.
        // The check is only necessary if a restriction is already fully defined.
        if (segmentIndex == 0 && clause.restriction.asRestrictionDTO()) {
            var attribute = clause.restriction.getAttribute();
            $scope.findOperators(attribute).then(function (segments) {
                var comparators = _lodash2.default.map(segments, function (segment) {
                    return segment.value;
                });
                var comparator = comparators.find(function (comparator) {
                    return comparator === clause.restriction.getComparator();
                });
                // In case no comparator was found, fall back to the 1st one in the list
                if (comparators.length >= 1 && (!comparator || comparator === void 0)) {
                    console.log("Comparator " + clause.restriction.getComparator() + " is selected but not supported. Falling back to " + comparators[0]);
                    clause.restriction.setComparator(comparators[0]);
                }
            }).then(function () {
                QueryCtrl.updateTargetFilter(); // Finally update the target filter
            });
        }
    };

    $scope.performClick = function (clause, control) {
        if (control.action) {
            control.action($scope.query, clause);
            QueryCtrl.updateTargetFilter();
            $scope.query.findParent().updateControls();
        }
    };
});
//# sourceMappingURL=query-directive.js.map
