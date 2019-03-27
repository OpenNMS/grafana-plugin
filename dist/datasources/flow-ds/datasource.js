'use strict';

System.register(['lodash', '../../lib/client_delegate'], function (_export, _context) {
  "use strict";

  var _, ClientDelegate, _createClass, FlowDatasource;

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  return {
    setters: [function (_lodash) {
      _ = _lodash.default;
    }, function (_libClient_delegate) {
      ClientDelegate = _libClient_delegate.ClientDelegate;
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

      _export('FlowDatasource', FlowDatasource = function () {
        function FlowDatasource(instanceSettings, $q, backendSrv, templateSrv) {
          _classCallCheck(this, FlowDatasource);

          this.type = instanceSettings.type;
          this.url = instanceSettings.url;
          this.name = instanceSettings.name;
          this.q = $q;
          this.backendSrv = backendSrv;
          this.templateSrv = templateSrv;
          this.client = new ClientDelegate(instanceSettings, backendSrv, $q);
        }

        _createClass(FlowDatasource, [{
          key: 'query',
          value: function query(options) {
            var start = options.range.from.valueOf();
            var end = options.range.to.valueOf();
            var step = Math.floor((end - start) / options.maxDataPoints);

            if (options.targets.length > 1) {
              throw new Error("Multiple targets are not currently supported when using the OpenNMS Flow Datasource.");
            }

            // Grab the first target
            var target = options.targets[0];
            if (target.metric === undefined || target.metric === null) {
              // Nothing to query - this can happen when we initially create the panel
              // and have not yet selected a metric
              return this.q.when({ 'data': [] });
            }

            // Combine
            var N = this.getFunctionParameterOrDefault(target, 'topN', 0, 10);
            var includeOther = FlowDatasource.isFunctionPresent(target, 'includeOther');
            // Filter
            var exporterNode = this.getFunctionParameterOrDefault(target, 'withExporterNode', 0);
            var ifIndex = this.getFunctionParameterOrDefault(target, 'withIfIndex', 0);
            // Transform
            var asTableSummary = FlowDatasource.isFunctionPresent(target, 'asTableSummary');

            if (target.metric === 'conversations') {
              if (!asTableSummary) {
                return this.client.getSeriesForTopNConversations(N, start, end, step, exporterNode, ifIndex).then(function (series) {
                  return {
                    data: FlowDatasource.toSeries(target, series)
                  };
                });
              } else {
                return this.client.getSummaryForTopNConversations(N, start, end, exporterNode, ifIndex).then(function (table) {
                  return {
                    data: FlowDatasource.toTable(table)
                  };
                });
              }
            } else if (target.metric === 'applications') {
              if (!asTableSummary) {
                return this.client.getSeriesForTopNApplications(N, start, end, step, includeOther, exporterNode, ifIndex).then(function (series) {
                  return {
                    data: FlowDatasource.toSeries(target, series)
                  };
                });
              } else {
                return this.client.getSummaryForTopNApplications(N, start, end, includeOther, exporterNode, ifIndex).then(function (table) {
                  return {
                    data: FlowDatasource.toTable(table)
                  };
                });
              }
            } else {
              throw 'Unsupported target metric: ' + target.metric;
            }
          }
        }, {
          key: 'testDatasource',
          value: function testDatasource() {
            return this.client.getClientWithMetadata().then(function (metadata) {
              if (metadata) {
                return {
                  status: "success",
                  message: "Data source is working",
                  title: "Success"
                };
              } else {
                return {
                  status: "danger",
                  message: "OpenNMS provided a response, but no metadata was found.",
                  title: "Unexpected Response"
                };
              }
            }).catch(function (e) {
              if (e.message === "Unsupported Version") {
                return {
                  status: "danger",
                  message: "The OpenNMS version you are trying to connect to is not supported. " + "OpenNMS Horizon version >= 22.0.0 or OpenNMS Meridian version >= 2018.1.0 is required.",
                  title: e.message
                };
              } else {
                throw e;
              }
            });
          }
        }, {
          key: 'annotationQuery',
          value: function annotationQuery() /* options */{
            return this.q.when([]);
          }
        }, {
          key: 'metricFindQuery',
          value: function metricFindQuery(query) {
            if (query === null || query === undefined || query === "") {
              return this.$q.resolve([]);
            }
            query = this.templateSrv.replace(query);

            var exporterNodesRegex = /exporterNodesWithFlows\((.*)\)/;
            var interfacesOnExporterNodeRegex = /interfacesOnExporterNodeWithFlows\((.*)\)/;

            var exporterNodesQuery = query.match(exporterNodesRegex);
            if (exporterNodesQuery) {
              return this.metricFindExporterNodes(exporterNodesQuery[1]);
            }

            var interfacesOnExporterNodeQuery = query.match(interfacesOnExporterNodeRegex);
            if (interfacesOnExporterNodeQuery) {
              return this.metricFindInterfacesOnExporterNode(interfacesOnExporterNodeQuery[1]);
            }

            return this.$q.resolve([]);
          }
        }, {
          key: 'metricFindExporterNodes',
          value: function metricFindExporterNodes() /* query */{
            return this.client.getExporters().then(function (exporters) {
              var results = [];
              _.each(exporters, function (exporter) {
                results.push({ text: exporter.label, value: exporter.id, expandable: true });
              });
              return results;
            });
          }
        }, {
          key: 'metricFindInterfacesOnExporterNode',
          value: function metricFindInterfacesOnExporterNode(query) {
            return this.client.getExporter(query).then(function (exporter) {
              var results = [];
              _.each(exporter.interfaces, function (iff) {
                results.push({ text: iff.name + "(" + iff.index + ")", value: iff.index, expandable: true });
              });
              return results;
            });
          }
        }, {
          key: 'getFunctionParameterOrDefault',
          value: function getFunctionParameterOrDefault(target, name, idx, def) {
            var func = FlowDatasource.getFirstFunction(target, name);
            if (func === null) {
              // No match, use the default value
              return def;
            }
            // Return the parameter value, and perform any required template variable substitutions
            return this.templateSrv.replace(func.parameters[idx]);
          }
        }], [{
          key: 'toTable',
          value: function toTable(table) {
            var columns = table && table.headers ? _.map(table.headers, function (column) {
              return { "text": column };
            }) : [];

            return [{
              "columns": columns,
              "rows": table.rows,
              "type": "table"
            }];
          }
        }, {
          key: 'toSeries',
          value: function toSeries(target, flowSeries) {
            var toBits = FlowDatasource.isFunctionPresent(target, 'toBits');
            var perSecond = FlowDatasource.isFunctionPresent(target, 'perSecond');
            var negativeEgress = FlowDatasource.isFunctionPresent(target, 'negativeEgress');
            var negativeIngress = FlowDatasource.isFunctionPresent(target, 'negativeIngress');
            var combineIngressEgress = FlowDatasource.isFunctionPresent(target, 'combineIngressEgress');
            var onlyIngress = FlowDatasource.isFunctionPresent(target, 'onlyIngress');
            var onlyEgress = FlowDatasource.isFunctionPresent(target, 'onlyEgress');

            var start = flowSeries.start.valueOf();
            var end = flowSeries.end.valueOf();
            var columns = flowSeries.columns;
            var values = flowSeries.values;
            var timestamps = flowSeries.timestamps;
            var series = [];
            var i = void 0,
                j = void 0,
                nRows = void 0,
                nCols = void 0,
                datapoints = void 0;

            var step = timestamps[1] - timestamps[0];

            if (timestamps !== undefined) {
              nRows = timestamps.length;
              nCols = columns.length;

              for (i = 0; i < nCols; i++) {
                // Optionally skip egress or ingress columns
                if (onlyIngress && !columns[i].ingress) {
                  continue;
                }
                if (onlyEgress && columns[i].ingress) {
                  continue;
                }

                var multiplier = negativeIngress ? -1 : 1;
                var suffix = " (In)";
                if (!columns[i].ingress) {
                  multiplier = negativeEgress ? -1 : 1;
                  suffix = " (Out)";
                }
                if (combineIngressEgress) {
                  // Remove any suffix, so that ingress and egress both have the same label
                  suffix = "";
                }
                if (perSecond) {
                  multiplier /= step / 1000;
                }
                if (toBits) {
                  // Convert from bytes to bits
                  multiplier *= 8;
                }

                datapoints = [];
                for (j = 0; j < nRows; j++) {
                  // Skip rows that are out-of-range
                  if (timestamps[j] < start || timestamps[j] > end) {
                    continue;
                  }

                  if (values[i][j] === 'NaN') {
                    values[i][j] = null;
                  }

                  datapoints.push([values[i][j] * multiplier, timestamps[j]]);
                }

                series.push({
                  target: columns[i].label + suffix,
                  datapoints: datapoints
                });
              }
            }

            if (combineIngressEgress) {
              series = FlowDatasource.sumMatchingTargets(series);
            }

            return series;
          }
        }, {
          key: 'sumMatchingTargets',
          value: function sumMatchingTargets(series) {
            var targetsByName = _.groupBy(series, function (s) {
              return s.target;
            });
            var newSeries = [];
            _.each(targetsByName, function (t) {
              var target = t[0].target;
              var K = t.length;
              var N = t[0].datapoints.length;
              var summedDatapoints = new Array(N);
              for (var k = 0; k < K; k++) {
                var targetDatapoints = t[k].datapoints;
                for (var n = 0; n < N; n++) {
                  if (summedDatapoints[n] == null) {
                    summedDatapoints[n] = [0, targetDatapoints[n][1]];
                  }
                  var valueToAdd = targetDatapoints[n][0] == null ? 0 : targetDatapoints[n][0];
                  summedDatapoints[n][0] = summedDatapoints[n][0] + valueToAdd;
                }
              }
              newSeries.push({
                target: target,
                datapoints: summedDatapoints
              });
            });
            return newSeries;
          }
        }, {
          key: 'getFirstFunction',
          value: function getFirstFunction(target, name) {
            var matchingFunctions = _.filter(target.functions, function (f) {
              return f.name === name;
            });
            return matchingFunctions.length > 0 ? matchingFunctions[0] : null;
          }
        }, {
          key: 'isFunctionPresent',
          value: function isFunctionPresent(target, name) {
            return FlowDatasource.getFirstFunction(target, name) !== null;
          }
        }]);

        return FlowDatasource;
      }());

      _export('FlowDatasource', FlowDatasource);
    }
  };
});
//# sourceMappingURL=datasource.js.map
