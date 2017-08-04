'use strict';

System.register(['./constants', './interpolate', 'lodash'], function (_export, _context) {
  "use strict";

  var QueryType, interpolate, _, _createClass, OpenNMSDatasource;

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  return {
    setters: [function (_constants) {
      QueryType = _constants.QueryType;
    }, function (_interpolate) {
      interpolate = _interpolate.interpolate;
    }, function (_lodash) {
      _ = _lodash.default;
    }],
    execute: function () {
      _createClass = function () {
        function defineProperties(target, props) {
          for (var i = 0; i < props.length; i++) {
            var descriptor = props[i];
            descriptor.enumerable = descriptor.enumerable || false;
            descriptor.configurable = true;
            if ("value" in descriptor) descriptor.writable = true;
            Object.defineProperty(target, descriptor.key, descriptor);
          }
        }

        return function (Constructor, protoProps, staticProps) {
          if (protoProps) defineProperties(Constructor.prototype, protoProps);
          if (staticProps) defineProperties(Constructor, staticProps);
          return Constructor;
        };
      }();

      _export('OpenNMSDatasource', OpenNMSDatasource = function () {
        function OpenNMSDatasource(instanceSettings, $q, backendSrv, templateSrv) {
          _classCallCheck(this, OpenNMSDatasource);

          this.type = instanceSettings.type;
          this.url = instanceSettings.url;
          this.name = instanceSettings.name;
          this.$q = $q;
          this.backendSrv = backendSrv;
          this.templateSrv = templateSrv;

          this.searchLimit = 25;
          this.target = {};
        }

        _createClass(OpenNMSDatasource, [{
          key: 'query',
          value: function query(options) {
            // Generate the query
            var query = this.buildQuery(options);

            // Issue the request
            var request;
            if (query.source.length > 0) {
              request = this.backendSrv.datasourceRequest({
                url: this.url + '/rest/measurements',
                data: query,
                method: 'POST',
                headers: { 'Content-Type': 'application/json' }
              });
            } else {
              // There are no sources listed, use an empty set of measurements
              request = this.$q.defer();
              request.resolve({ measurements: [] });
            }

            // Convert the results to the expected format
            return this.$q.when(request).then(function (response) {
              if (response.status !== 200) {
                throw { message: 'Query failed.' };
              }
              return OpenNMSDatasource.processMeasurementsResponse(response);
            });
          }
        }, {
          key: 'testDatasource',
          value: function testDatasource() {
            return this.backendSrv.datasourceRequest({
              url: this.url + '/rest/info',
              method: 'GET'
            }).then(function (response) {
              if (response.status === 200) {
                return { status: "success", message: "Data source is working!", title: "Success" };
              }
            });
          }
        }, {
          key: 'metricFindQuery',
          value: function metricFindQuery(query) {
            if (query === null || query === undefined || query === "") {
              return this.$q.resolve([]);
            }

            var interpolatedQuery = _.first(this.interpolateValue(query));
            var nodeFilterRegex = /nodeFilter\((.*)\)/;
            var nodeResourcesRegex = /nodeResources\((.*)\)/;

            if (interpolatedQuery !== undefined) {
              var nodeFilterQuery = interpolatedQuery.match(nodeFilterRegex);
              if (nodeFilterQuery) {
                return this.metricFindNodeFilterQuery(nodeFilterQuery[1]);
              }

              var nodeCriteria = interpolatedQuery.match(nodeResourcesRegex);
              if (nodeCriteria) {
                return this.metricFindNodeResourceQuery(nodeCriteria[1]);
              }
            }

            return this.$q.resolve([]);
          }
        }, {
          key: 'metricFindNodeFilterQuery',
          value: function metricFindNodeFilterQuery(query) {
            return this.backendSrv.datasourceRequest({
              url: this.url + '/rest/nodes',
              method: 'GET',
              params: {
                filterRule: query,
                limit: 0
              }
            }).then(function (response) {
              if (response.data.count > response.data.totalCount) {
                console.warn("Filter matches " + response.data.totalCount + " records, but only " + response.data.count + " will be used.");
              }
              var results = [];
              _.each(response.data.node, function (node) {
                var nodeCriteria = node.id.toString();
                if (node.foreignId !== null && node.foreignSource !== null) {
                  nodeCriteria = node.foreignSource + ":" + node.foreignId;
                }
                results.push({ text: node.label, value: nodeCriteria, expandable: true });
              });
              return results;
            });
          }
        }, {
          key: 'metricFindNodeResourceQuery',
          value: function metricFindNodeResourceQuery(query) {
            return this.backendSrv.datasourceRequest({
              url: this.url + '/rest/resources/' + encodeURIComponent(OpenNMSDatasource.getNodeResource(query)),
              method: 'GET',
              params: {
                depth: 1
              }
            }).then(function (response) {
              var results = [];
              _.each(response.data.children.resource, function (resource) {
                var resourceWithoutNodePrefix = resource.id.match(/node(Source)?\[.*?\]\.(.*)/);
                if (resourceWithoutNodePrefix) {
                  results.push({ text: resourceWithoutNodePrefix[2], expandable: true });
                }
              });
              return results;
            });
          }
        }, {
          key: 'buildQuery',
          value: function buildQuery(options) {
            var self = this,
                start = options.range.from.valueOf(),
                end = options.range.to.valueOf(),
                step = Math.floor((end - start) / options.maxDataPoints);

            var query = {
              "start": start,
              "end": end,
              "step": step,
              "maxrows": options.maxDataPoints,
              "source": [],
              "expression": []
            };

            _.each(options.targets, function (target) {
              var transient = "false";
              if (target.hide) {
                transient = true;
              }

              if (target.type === QueryType.Attribute) {
                if (!(target.nodeId && target.resourceId && target.attribute)) {
                  return;
                }

                var label = target.label;
                if (label === undefined || label === '') {
                  label = target.attribute;
                }

                // Build the source
                var source = {
                  "aggregation": target.aggregation,
                  "attribute": target.attribute,
                  "label": label,
                  "resourceId": target.resourceId,
                  "nodeId": target.nodeId, // temporary attribute used for interpolation
                  "transient": transient
                };

                if (target.subattribute !== undefined && target.subattribute !== '') {
                  source.datasource = target.subattribute;
                }

                // Perform variable substitution - may generate additional queries
                query.source = query.source.concat(self.interpolateSourceVariables(source, function (interpolatedSource) {
                  // Calculate the effective resource id after the interpolation
                  interpolatedSource.resourceId = OpenNMSDatasource.getRemoteResourceId(interpolatedSource.nodeId, interpolatedSource.resourceId);
                  delete interpolatedSource.nodeId;
                }));
              } else if (target.type === QueryType.Expression) {
                if (!(target.label && target.expression)) {
                  return;
                }

                // Build the expression
                var expression = {
                  "label": target.label,
                  "value": target.expression,
                  "transient": transient
                };

                // Perform variable substitution - may generate additional expressions
                query.expression = query.expression.concat(self.interpolateExpressionVariables(expression));
              } else if (target.type === QueryType.Filter) {
                if (!target.filter) {
                  return;
                }

                // Interpolate the filter parameters
                var interpolatedFilterParms = self.interpolateVariables(target.filterParameters, _.keys(target.filterParameters));

                var filters = _.map(interpolatedFilterParms, function (filterParms) {
                  // Build the filter definition
                  var parameters = [];
                  _.each(filterParms, function (value, key) {
                    // Skip parameters with undefined or empty values
                    if (value === undefined || value === '' || value === null) {
                      return;
                    }

                    parameters.push({
                      'key': key,
                      'value': value
                    });
                  });

                  return {
                    "name": target.filter.name,
                    "parameter": parameters
                  };
                });

                // Only add the filter attribute to the query when one or more filters are specified since
                // OpenNMS versions before 17.0.0 do not support it
                if (!query.filter) {
                  query.filter = filters;
                } else {
                  query.filter = query.filter.concat(filters);
                }
              }
            });

            return query;
          }
        }, {
          key: 'interpolateSourceVariables',
          value: function interpolateSourceVariables(source, callback) {
            return this.interpolateVariables(source, ['nodeId', 'resourceId', 'attribute', 'datasource', 'label'], callback);
          }
        }, {
          key: 'interpolateExpressionVariables',
          value: function interpolateExpressionVariables(expression) {
            return this.interpolateVariables(expression, ['value', 'label']);
          }
        }, {
          key: 'interpolateValue',
          value: function interpolateValue(value) {
            return _.map(this.interpolateVariables({ 'value': value }, ['value']), function (entry) {
              return entry.value;
            });
          }
        }, {
          key: 'interpolateVariables',
          value: function interpolateVariables(object, attributes, callback) {
            // Reformat the variables to work with our interpolate function
            var variables = [];
            _.each(this.templateSrv.variables, function (templateVariable) {
              var variable = {
                name: templateVariable.name,
                value: []
              };

              // Single-valued?
              if (_.isString(templateVariable.current.value)) {
                variable.value.push(templateVariable.current.value);
              } else {
                _.each(templateVariable.current.value, function (value) {
                  if (value === "$__all") {
                    _.each(templateVariable.options, function (option) {
                      // "All" is part of the options, so make sure to skip that one
                      if (option.value !== "$__all") {
                        variable.value.push(option.value);
                      }
                    });
                  } else {
                    variable.value.push(value);
                  }
                });
              }

              variables.push(variable);
            });
            return interpolate(object, attributes, variables, callback);
          }
        }, {
          key: 'searchForNodes',
          value: function searchForNodes(query) {
            return this.backendSrv.datasourceRequest({
              url: this.url + '/rest/nodes',
              method: 'GET',
              params: {
                limit: this.searchLimit,
                match: 'any',
                comparator: 'ilike',
                orderBy: 'id',
                order: 'asc',
                label: '%' + query + '%',
                sysName: '%' + query + '%',
                'ipInterface.ipAddress': '%' + query + '%',
                'ipInterface.ipHostName': '%' + query + '%',
                'foreignId': query + '%' // doesn't support leading '%'
              }
            });
          }
        }, {
          key: 'getResourcesWithAttributesForNode',
          value: function getResourcesWithAttributesForNode(nodeId) {
            var interpolatedNodeId = _.first(this.interpolateValue(nodeId));

            return this.backendSrv.datasourceRequest({
              url: this.url + '/rest/resources/fornode/' + encodeURIComponent(interpolatedNodeId),
              method: 'GET',
              params: {
                depth: -1
              }
            }).then(function (results) {
              return OpenNMSDatasource.flattenResourcesWithAttributes([results.data], []);
            });
          }
        }, {
          key: 'getAvailableFilters',
          value: function getAvailableFilters() {
            return this.backendSrv.datasourceRequest({
              url: this.url + '/rest/measurements/filters',
              method: 'GET'
            });
          }
        }, {
          key: 'suggestAttributes',
          value: function suggestAttributes(nodeId, resourceId, query) {
            var interpolatedNodeId = _.first(this.interpolateValue(nodeId)),
                interpolatedResourceId = _.first(this.interpolateValue(resourceId));
            var remoteResourceId = OpenNMSDatasource.getRemoteResourceId(interpolatedNodeId, interpolatedResourceId);

            return this.backendSrv.datasourceRequest({
              url: this.url + '/rest/resources/' + encodeURIComponent(remoteResourceId),
              method: 'GET',
              params: {
                depth: -1
              }
            }).then(function (results) {
              query = query.toLowerCase();
              var attributes = [];
              _.each(results.data.rrdGraphAttributes, function (value, key) {
                if (key.toLowerCase().indexOf(query) >= 0) {
                  attributes.push(key);
                }
              });
              attributes.sort();

              return attributes;
            });
          }
        }], [{
          key: 'processMeasurementsResponse',
          value: function processMeasurementsResponse(response) {
            var labels = response.data.labels;
            var columns = response.data.columns;
            var timestamps = response.data.timestamps;
            var series = [];
            var i, j, nRows, nCols, datapoints;

            if (timestamps !== undefined) {
              nRows = timestamps.length;
              nCols = columns.length;

              for (i = 0; i < nCols; i++) {
                datapoints = [];
                for (j = 0; j < nRows; j++) {
                  // Skip rows that are out-of-ranges - this can happen with RRD data in narrow time spans
                  if (timestamps[j] < response.data.start || timestamps[j] > response.data.end) {
                    continue;
                  }

                  datapoints.push([columns[i].values[j], timestamps[j]]);
                }

                series.push({
                  target: labels[i],
                  datapoints: datapoints
                });
              }
            }

            return { data: series };
          }
        }, {
          key: 'flattenResourcesWithAttributes',
          value: function flattenResourcesWithAttributes(resources, resourcesWithAttributes) {
            _.each(resources, function (resource) {
              if (resource.rrdGraphAttributes !== undefined && Object.keys(resource.rrdGraphAttributes).length > 0) {
                resourcesWithAttributes.push(resource);
              }
              if (resource.children !== undefined && resource.children.resource.length > 0) {
                OpenNMSDatasource.flattenResourcesWithAttributes(resource.children.resource, resourcesWithAttributes);
              }
            });
            return resourcesWithAttributes;
          }
        }, {
          key: 'getNodeResource',
          value: function getNodeResource(nodeId) {
            var prefix = "";
            if (nodeId.indexOf(":") > 0) {
              prefix = "nodeSource[";
            } else {
              prefix = "node[";
            }
            return prefix + nodeId + "]";
          }
        }, {
          key: 'getRemoteResourceId',
          value: function getRemoteResourceId(nodeId, resourceId) {
            return OpenNMSDatasource.getNodeResource(nodeId) + "." + resourceId;
          }
        }]);

        return OpenNMSDatasource;
      }());

      _export('OpenNMSDatasource', OpenNMSDatasource);
    }
  };
});
//# sourceMappingURL=datasource.js.map
