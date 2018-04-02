import _ from 'lodash';
import {ClientDelegate} from '../../lib/client_delegate';

export class FlowDatasource {

  constructor(instanceSettings, $q, backendSrv, templateSrv) {
    this.type = instanceSettings.type;
    this.url = instanceSettings.url;
    this.name = instanceSettings.name;
    this.q = $q;
    this.backendSrv = backendSrv;
    this.templateSrv = templateSrv;
    this.client = new ClientDelegate(instanceSettings, backendSrv, $q);
  }

  query(options) {
    let start = options.range.from.valueOf();
    let end = options.range.to.valueOf();
    let step = Math.floor((end - start) / options.maxDataPoints);

    if (options.targets.length > 1) {
      throw new Error("Multiple targets are not currently supported when using the OpenNMS Flow Datasource.");
    }

    // Grab the first target
    let target = options.targets[0];
    if (target.metric === undefined || target.metric === null) {
      // Nothing to query - this can happen when we initially create the panel
      // and have not yet selected a metric
      return this.q.when({'data': []});
    }

    // Combine
    let N = this.getFunctionParameterOrDefault(target, 'topN', 10);
    let includeOther = FlowDatasource.isFunctionPresent(target, 'includeOther');
    // Filter
    let exporterNode = this.getFunctionParameterOrDefault(target, 'withExporterNode', 0);
    let ifIndex = this.getFunctionParameterOrDefault(target, 'withIfIndex', 0);
    // Transform
    let asTableSummary = FlowDatasource.isFunctionPresent(target, 'asTableSummary');

    if (target.metric === 'conversations') {
      if (!asTableSummary) {
        return this.client.getSeriesForTopNConversations(N, start, end, step, exporterNode, ifIndex).then(series => {
          return {
            data: FlowDatasource.toSeries(target, series)
          };
        });
      } else {
        return this.client.getSummaryForTopNConversations(N, start, end, exporterNode, ifIndex).then(table => {
          return {
            data: FlowDatasource.toTable(table)
          };
        });
      }
    } else if (target.metric === 'applications') {
      if (!asTableSummary) {
        return this.client.getSeriesForTopNApplications(N, start, end, step, includeOther, exporterNode, ifIndex).then(series => {
          return {
            data: FlowDatasource.toSeries(target, series)
          };
        });
      } else {
        return this.client.getSummaryForTopNApplications(N, start, end, includeOther, exporterNode, ifIndex).then(table => {
          return {
            data: FlowDatasource.toTable(table)
          };
        });
      }
    } else {
      throw 'Unsupported target metric: ' + target.metric;
    }
  }

  testDatasource() {
    return this.client.getClientWithMetadata()
      .then(metadata => {
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
          }
        }
      }).catch(e => {
        if (e.message === "Unsupported Version") {
          return {
            status: "danger",
            message: "The OpenNMS version you are trying to connect to is not supported. " +
            "OpenNMS Horizon version >= 22.0.0 or OpenNMS Meridian version >= 2018.1.0 is required.",
            title: e.message
          }
        } else {
          throw e;
        }
      });
  }

  annotationQuery(options) {
    return this.q.when([]);
  }

  // Used by template queries
  metricFindQuery(query) {
    if (query === null || query === undefined || query === "") {
      return this.$q.resolve([]);
    }
    query = this.templateSrv.replace(query);

    let exporterNodesRegex = /exporterNodesWithFlows\((.*)\)/;
    let interfacesOnExporterNodeRegex = /interfacesOnExporterNodeWithFlows\((.*)\)/;

    let exporterNodesQuery = query.match(exporterNodesRegex);
    if (exporterNodesQuery) {
      return this.metricFindExporterNodes(exporterNodesQuery[1]);
    }

    let interfacesOnExporterNodeQuery = query.match(interfacesOnExporterNodeRegex);
    if (interfacesOnExporterNodeQuery) {
      return this.metricFindInterfacesOnExporterNode(interfacesOnExporterNodeQuery[1]);
    }

    return this.$q.resolve([]);
  }

  metricFindExporterNodes(query) {
    return this.client.getExporters().then(exporters => {
      let results = [];
      _.each(exporters, function (exporter) {
        results.push({text: exporter.label, value: exporter.id, expandable: true});
      });
      return results;
    });
  }

  metricFindInterfacesOnExporterNode(query) {
    return this.client.getExporter(query).then(exporter => {
      let results = [];
      _.each(exporter.interfaces, function (iff) {
        results.push({text: iff.name + "(" + iff.index + ")", value: iff.index, expandable: true});
      });
      return results;
    });
  }

  static toTable(table) {
    let columns = _.map(table.headers, column => {
      return {"text": column}
    });

    return [
      {
        "columns": columns,
        "rows": table.rows,
        "type": "table",
      }
    ];
  }

  static toSeries(target, flowSeries) {
    let perSecond = FlowDatasource.isFunctionPresent(target, 'perSecond');
    let negativeEgress = FlowDatasource.isFunctionPresent(target, 'negativeEgress');
    let negativeIngress = FlowDatasource.isFunctionPresent(target, 'negativeIngress');
    let combineIngressEgress = FlowDatasource.isFunctionPresent(target, 'combineIngressEgress');

    let start = flowSeries.start.valueOf();
    let end = flowSeries.end.valueOf();
    let columns = flowSeries.columns;
    let values = flowSeries.values;
    let timestamps = flowSeries.timestamps;
    let series = [];
    let i, j, nRows, nCols, datapoints;

    let step = timestamps[1] - timestamps[0];

    if (timestamps !== undefined) {
      nRows = timestamps.length;
      nCols = columns.length;

      for (i = 0; i < nCols; i++) {
        let multiplier = negativeIngress ? -1 : 1;
        let suffix = " (In)";
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

  static sumMatchingTargets(series) {
    let targetsByName = _.groupBy(series, (s) => s.target);
    let newSeries = [];
    _.each(targetsByName, (t) => {
      let target = t[0].target;
      let K = t.length;
      let N = t[0].datapoints.length;
      let summedDatapoints = new Array(N);
      for (let k = 0; k < K; k++) {
        let targetDatapoints = t[k].datapoints;
        for (let n = 0; n < N; n++) {
          if (summedDatapoints[n] == null) {
            summedDatapoints[n] = [0, targetDatapoints[n][1]];
          }
          let valueToAdd = targetDatapoints[n][0] == null ? 0 : targetDatapoints[n][0];
          summedDatapoints[n][0] = summedDatapoints[n][0] + valueToAdd;
        }
      }
      newSeries.push({
        target: target,
        datapoints: summedDatapoints
      })
    });
    return newSeries;
  }

  static getFirstFunction(target, name) {
    let matchingFunctions = _.filter(target.functions, function (f) {
      return f.name === name;
    });
    return matchingFunctions.length > 0 ? matchingFunctions[0] : null;
  }

  static isFunctionPresent(target, name) {
    return FlowDatasource.getFirstFunction(target, name) !== null;
  }

  getFunctionParameterOrDefault(target, name, idx, def) {
    let func = FlowDatasource.getFirstFunction(target, name);
    if (func === null) {
      // No match, use the default value
      return def;
    }
    // Return the parameter value, and perform any required template variable substitutions
    return this.templateSrv.replace(func.parameters[idx]);
  }
}
