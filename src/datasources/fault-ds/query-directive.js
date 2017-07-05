import angular from 'angular';
import _ from 'lodash';

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
                    .then(function(attributes) {
                        let segments = _.map(attributes, function(attribute) {
                            var segment = uiSegmentSrv.newKey(attribute.name);
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
                        return _.map(comparators, function(comparator) {
                            return uiSegmentSrv.newOperator(comparator);
                        });
                    })
            }

            // value input
            if (segment.type == 'value') {
                let attributeSegment = segments[index-2];
                let theQuery = {
                    'find': 'values',
                    'attribute': attributeSegment.value,
                    'query': segment.value === 'select attribute value' ? '' : segment.value
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
                $scope.query.findParent().updateControls();
            }
        }
    });
