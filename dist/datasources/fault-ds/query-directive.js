'use strict';

System.register(['angular', 'lodash', './mapping/ComparatorMapping', './UI'], function (_export, _context) {
    "use strict";

    var angular, _, ComparatorMapping, UI;

    return {
        setters: [function (_angular) {
            angular = _angular.default;
        }, function (_lodash) {
            _ = _lodash.default;
        }, function (_mappingComparatorMapping) {
            ComparatorMapping = _mappingComparatorMapping.ComparatorMapping;
        }, function (_UI) {
            UI = _UI.UI;
        }],
        execute: function () {

            angular.module('grafana.directives').directive('onmsQuery', function () {
                return {
                    templateUrl: 'public/plugins/opennms-helm/datasources/fault-ds/partials/query.html',
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
                        comparators = _.filter(comparators, function (comparator) {
                            return comparator.aliases && comparator.aliases.length > 0;
                        });
                        return _.map(comparators, function (comparator) {
                            var uiComparator = new ComparatorMapping().getUiComparator(comparator);
                            return uiSegmentSrv.newOperator(uiComparator);
                        });
                    }).catch(QueryCtrl.handleQueryError.bind(QueryCtrl));
                };

                $scope.getSuggestions = function (clause, segment, index) {
                    var segments = clause.restriction.segments;

                    // attribute input
                    if (segment.type == 'key' || segment.type == 'plus-button') {
                        return datasource.metricFindQuery({ find: "attributes", strategy: QueryCtrl.featuredAttributes === true ? 'featured' : 'all' }).then(function (properties) {
                            var segments = _.map(properties, function (property) {
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
                            'query': segment.value === UI.Restriction.VALUE_PLACEHOLDER ? '' : segment.value
                        };

                        return datasource.metricFindQuery(theQuery).then(function (values) {
                            return _.map(values, function (searchResult) {
                                var segment = uiSegmentSrv.newKeyValue(searchResult.label);
                                return segment;
                            });
                        }).catch(QueryCtrl.handleQueryError.bind(QueryCtrl));
                    }

                    // condition input
                    if (segment.type === 'condition') {
                        return this.datasource.metricFindQuery({ find: 'operators' }).then(function (operators) {
                            return _.map(operators, function (operator) {
                                return uiSegmentSrv.newCondition(operator.label);
                            });
                        }).catch(QueryCtrl.handleQueryError.bind(QueryCtrl));
                    }
                    return $q.when([]);
                };

                $scope.segmentUpdated = function (clause, segment, segmentIndex) {
                    // Make the value not a fake input anymore
                    if (segment.type === 'value') {
                        segment.fake = false;
                    }

                    // It must be verified that the comparator is still in the list of comparators.
                    // If not, the first comparator in the list is fallen back to.
                    // The check is only necessary if a restriction is already fully defined.
                    if (segmentIndex == 0 && clause.restriction.asRestrictionDTO()) {
                        var attribute = clause.restriction.getAttribute();
                        $scope.findOperators(attribute).then(function (segments) {
                            var comparators = _.map(segments, function (segment) {
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
                            $scope.query.updateControls();
                            QueryCtrl.updateTargetFilter();
                        });
                    } else {
                        // Default behaviour
                        $scope.query.updateControls();
                        QueryCtrl.updateTargetFilter();
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
        }
    };
});
//# sourceMappingURL=query-directive.js.map
