import angular from 'angular';
import _ from 'lodash';
import {ComparatorMapping} from "./mapping/ComparatorMapping";
import {UI} from "./UI";

angular.module('grafana.directives')
    .directive('onmsQuery', function() {
        return {
            templateUrl: 'public/plugins/opennms-helm-app/datasources/fault-ds/partials/query.html',
            controller: 'QueryController',
            restrict: 'EA',
            controllerAs: 'ctrl',
            scope: {
                query: "=", // The ui query object
                datasource: "=", // The datasource
                queryCtrl: "=", // The QueryCtrl object
            }
        };
    })
    .controller('QueryController', function ($scope, uiSegmentSrv, $q) {
        const datasource = $scope.datasource;
        const QueryCtrl = $scope.queryCtrl;
        $scope.query.updateControls();

        $scope.getSuggestions = function(clause, segment, index) {
            var segments = clause.restriction.segments;

            // attribute input
            if (segment.type == 'key' || segment.type == 'plus-button') {
                return datasource.metricFindQuery({find: "attributes"})
                    .then(function(properties) {
                        let segments = _.map(properties, function(property) {
                            var segment = uiSegmentSrv.newKey(property.id);
                            return segment;
                        });
                        return segments;
                    })
                    .catch(QueryCtrl.handleQueryError.bind(QueryCtrl));
            }

            // comparator input
            if (segment.type == 'operator') {
                let attributeSegment = segments[index-1];
                return datasource.metricFindQuery({'find': 'comparators', 'attribute': attributeSegment.value})
                    .then(function(comparators) {
                        // the API.Comparator.id or API.Comparator.label fields cannot be used.
                        comparators = _.filter(comparators, function(comparator) {
                            return comparator.aliases && comparator.aliases.length > 0;
                        });
                        return _.map(comparators, function(comparator) {
                            const uiComparator = new ComparatorMapping().getUiComparator(comparator);
                            return uiSegmentSrv.newOperator(uiComparator);
                        });
                    }).catch(QueryCtrl.handleQueryError.bind(QueryCtrl));
            }

            // value input
            if (segment.type == 'value') {
                let attributeSegment = segments[index-2];
                let theQuery = {
                    'find': 'values',
                    'attribute': attributeSegment.value,
                    'query': segment.value === UI.Restriction.VALUE_PLACEHOLDER ? '' : segment.value
                };

                return datasource.metricFindQuery(theQuery)
                    .then(function(values) {
                        return _.map(values, function(searchResult) {
                            var segment = uiSegmentSrv.newKeyValue(searchResult.label);
                            return segment;
                        })
                    })
                    .catch(QueryCtrl.handleQueryError.bind(QueryCtrl));
            }

            // condition input
            if (segment.type === 'condition') {
                return this.datasource.metricFindQuery({find: 'operators'}).then(function(operators) {
                    return _.map(operators, function(operator) {
                        return uiSegmentSrv.newCondition(operator.label);
                    });
                }).catch(QueryCtrl.handleQueryError.bind(QueryCtrl));
            }
            return $q.when([]);
        };

        $scope.segmentUpdated = function(clause, segment, segmentIndex) {
            $scope.query.segmentUpdated(clause, segment, segmentIndex);
            $scope.query.updateControls();
            QueryCtrl.updateTargetFilter();
        };

        $scope.performClick = function(clause, control) {
            if (control.action) {
                control.action($scope.query, clause);
                QueryCtrl.updateTargetFilter();
                $scope.query.findParent().updateControls();
            }
        }
    });
