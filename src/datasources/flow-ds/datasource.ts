import _ from 'lodash';

import {
  DataQuery,
  DataQueryRequest,
  DataQueryResponse,
  rangeUtil,
  TableData
} from '@grafana/data';

import { ClientDelegate } from 'lib/client_delegate';
import { dscpLabel, dscpSelectOptions } from 'lib/tos_helper';
import { processSelectionVariables } from 'lib/utils';
import { OnmsFlowTable } from 'opennms/src/model/OnmsFlowTable';
import { OnmsFlowSeries } from 'opennms/src/model/OnmsFlowSeries';
import {TimeSeries} from "@grafana/data/types/data";

interface FlowDataQuery extends DataQuery {
  metric: string
}

export class FlowDatasource {
  type?: string;
  url?: string;
  name?: string;
  client: ClientDelegate;

  /** @ngInject */
  constructor(instanceSettings: any, public backendSrv: any, public templateSrv: any) {
    this.type = instanceSettings.type;
    this.url = instanceSettings.url;
    this.name = instanceSettings.name;
    this.client = new ClientDelegate(instanceSettings, backendSrv);
  }

  query(options: DataQueryRequest<FlowDataQuery>): Promise<DataQueryResponse> {

    // filter queries that have their metric set
    // -> when we initially create a query no metric is selected
    const queriesWithMetrics = options.targets.filter(query => query.metric)

    const allAreSummaries = queriesWithMetrics.every(target => FlowDatasource.isFunctionPresent(target, 'asTableSummary'))
    const allAreSeries = queriesWithMetrics.every(target => !FlowDatasource.isFunctionPresent(target, 'asTableSummary'))

    if (!allAreSummaries && !allAreSeries) {
      throw new Error("The 'asTableSummary' transformation must be included in all queries of a panel or in none of them.");
    }

    let start = options.range.from.valueOf();
    let end = options.range.to.valueOf();

    if (allAreSummaries) {

      const summaryPromises = queriesWithMetrics.map(query => this.querySummary(options, query))

      return Promise.all(summaryPromises).then(summaries => {
        return {
          data: summaries
        }
      })

    } else {

      const intervals = queriesWithMetrics
          .map(target => this.getFunctionParameterOrDefault(target, 'withGroupByInterval', 0))
          .filter(i => i) // exclude `undefined`

      const differentIntervals = new Set(intervals)
      if (differentIntervals.size > 1) {
        throw new Error("All queries must use the same 'withGroupByInterval`.");
      }
      let step;
      if (differentIntervals.size === 1) {
        step = rangeUtil.intervalToMs(differentIntervals.values().next().value)
      } else {
        // @ts-ignore
        step = Math.floor((end - start) / options.maxDataPoints);
      }

      const series = queriesWithMetrics.map(target => this.querySeries(options, target, step))

      return Promise.all(series).then(series => {
        return {
          // querySeries returns TimeSeries[]
          // -> flatMap it
          data: series.flatMap(s => s)
        }
      })
    }

  }

  querySummary(options: DataQueryRequest, target: FlowDataQuery): Promise<TableData> {

    let start = options.range.from.valueOf();
    let end = options.range.to.valueOf();

    // Combine
    let N = this.getFunctionParameterOrDefault(target, 'topN', 0, 10);
    let includeOther = FlowDatasource.isFunctionPresent(target, 'includeOther');
    // Filter
    let exporterNode = this.getFunctionParameterOrDefault(target, 'withExporterNode', 0);
    let ifIndex = this.getFunctionParameterOrDefault(target, 'withIfIndex', 0);
    let dscp = processSelectionVariables(this.getFunctionParametersOrDefault(target, 'withDscp', 0, null));
    let applications = this.getFunctionParametersOrDefault(target, 'withApplication', 0, null);
    let conversations = this.getFunctionParametersOrDefault(target, 'withConversation', 0, null);
    let hosts = this.getFunctionParametersOrDefault(target, 'withHost', 0, null);

    switch (target.metric) {
      case 'conversations':
        if (conversations && conversations.length > 0) {
          return this.client.getSummaryForConversations(conversations, start, end, includeOther, exporterNode, ifIndex, dscp).then(table => {
            return this.toTable(target, table)
          });
        } else {
          return this.client.getSummaryForTopNConversations(N, start, end, includeOther, exporterNode, ifIndex, dscp).then(table => {
            return this.toTable(target, table)
          });
        }
      case 'applications':
        if (applications && applications.length > 0) {
          return this.client.getSummaryForApplications(applications, start, end, includeOther, exporterNode, ifIndex, dscp).then(table => {
            return this.toTable(target, table)
          });
        } else {
          return this.client.getSummaryForTopNApplications(N, start, end, includeOther, exporterNode, ifIndex, dscp).then(table => {
            return this.toTable(target, table)
          });
        }
      case 'hosts':
        if (hosts && hosts.length > 0) {
          return this.client.getSummaryForHosts(hosts, start, end, includeOther, exporterNode, ifIndex, dscp).then(table => {
            return this.toTable(target, table)
          });
        } else {
          return this.client.getSummaryForTopNHosts(N, start, end, includeOther, exporterNode, ifIndex, dscp).then(table => {
            return this.toTable(target, table)
          });
        }
      case 'dscps':
        return this.client.getSummaryForDscps(start, end, exporterNode, ifIndex, dscp).then(table => {
          return this.toTable(target, table, dscpLabel)
        });
      default:
        throw 'Unsupported target metric: ' + target.metric;
    }
  }

  querySeries(options: DataQueryRequest, target: FlowDataQuery, step: number): Promise<TimeSeries[]> {

    let start = options.range.from.valueOf();
    let end = options.range.to.valueOf();

    // Combine
    let N = this.getFunctionParameterOrDefault(target, 'topN', 0, 10);
    let includeOther = FlowDatasource.isFunctionPresent(target, 'includeOther');
    // Filter
    let exporterNode = this.getFunctionParameterOrDefault(target, 'withExporterNode', 0);
    let ifIndex = this.getFunctionParameterOrDefault(target, 'withIfIndex', 0);
    let dscp = processSelectionVariables(this.getFunctionParametersOrDefault(target, 'withDscp', 0, null));
    let applications = this.getFunctionParametersOrDefault(target, 'withApplication', 0, null);
    let conversations = this.getFunctionParametersOrDefault(target, 'withConversation', 0, null);
    let hosts = this.getFunctionParametersOrDefault(target, 'withHost', 0, null);

    switch (target.metric) {
      case 'conversations':
        if (conversations && conversations.length > 0) {
          return this.client.getSeriesForConversations(conversations, start, end, step, includeOther, exporterNode, ifIndex, dscp).then(series => {
            return this.toSeries(target, series)
          });
        } else {
          return this.client.getSeriesForTopNConversations(N, start, end, step, includeOther, exporterNode, ifIndex, dscp).then(series => {
            return this.toSeries(target, series)
          });
        }
      case 'applications':
        if (applications && applications.length > 0) {
          return this.client.getSeriesForApplications(applications, start, end, step, includeOther, exporterNode, ifIndex, dscp).then(series => {
            return this.toSeries(target, series)
          });
        } else {
          return this.client.getSeriesForTopNApplications(N, start, end, step, includeOther, exporterNode, ifIndex, dscp).then(series => {
            return this.toSeries(target, series)
          });
        }
      case 'hosts':
        if (hosts && hosts.length > 0) {
          return this.client.getSeriesForHosts(hosts, start, end, step, includeOther, exporterNode, ifIndex, dscp).then(series => {
            return this.toSeries(target, series)
          });
        } else {
          return this.client.getSeriesForTopNHosts(N, start, end, step, includeOther, exporterNode, ifIndex, dscp).then(series => {
            return this.toSeries(target, series)
          });
        }
      case 'dscps':
        return this.client.getSeriesForDscps(start, end, step, exporterNode, ifIndex, dscp).then(series => {
          return this.toSeries(target, series, dscpLabel)
        });
      default:
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

  annotationQuery(/* options */) {
    return Promise.resolve([]);
  }

  // Used by template queries
  metricFindQuery(query) {
    if (query === null || query === undefined || query === "") {
      return Promise.resolve([]);
    }
    query = this.templateSrv.replace(query);

    let exporterNodesRegex = /exporterNodesWithFlows\((.*)\)/;
    let interfacesOnExporterNodeRegex = /interfacesOnExporterNodeWithFlows\(\s*([^,]+).*\)/; // just pick the first arg and ignore anything else
    let dscpOnExporterNodeAndInterfaceRegex = /dscpOnExporterNodeAndInterface\(\s*([^,]+),\s*([^,]+),\s*([^,]+),\s*([^\s]+\s*)\)/;

    let exporterNodesQuery = query.match(exporterNodesRegex);
    if (exporterNodesQuery) {
      return this.metricFindExporterNodes();
    }

    let interfacesOnExporterNodeQuery = query.match(interfacesOnExporterNodeRegex);
    if (interfacesOnExporterNodeQuery) {
      return this.metricFindInterfacesOnExporterNode(interfacesOnExporterNodeQuery[1]);
    }

    let dscpOnExporterNodeAndInterfaceQuery = query.match(dscpOnExporterNodeAndInterfaceRegex);
    if (dscpOnExporterNodeAndInterfaceQuery) {
      return this.metricFindDscpOnExporterNodeAndInterface(
          dscpOnExporterNodeAndInterfaceQuery[1], // node
          dscpOnExporterNodeAndInterfaceQuery[2], // interface
          dscpOnExporterNodeAndInterfaceQuery[3], // start millis
          dscpOnExporterNodeAndInterfaceQuery[4], // end millis
      );
    }

    return Promise.resolve([]);
  }

  metricFindExporterNodes(query?: any) {
    return this.client.getExporters().then((exporters) => {
      let results = [] as any[];
      _.each(exporters, function (exporter) {
        results.push({text: exporter.label, value: exporter.id, expandable: true});
      });
      return results;
    });
  }

  metricFindInterfacesOnExporterNode(query) {
    return this.client.getExporter(query).then((exporter) => {
      let results = [] as any[];
      _.each(exporter.interfaces, function (iff) {
        results.push({text: iff.name + "(" + iff.index + ")", value: iff.index, expandable: true});
      });
      return results;
    });
  }

  metricFindDscpOnExporterNodeAndInterface(node, iface, start, end) {
    return this.client.getDscpValues(node,  iface, start, end).then(
        values => dscpSelectOptions(values)
    );
  }

  ensureLabelTransformer(labelTransformer?: (s: string) => string): (string: string) => string {
    return labelTransformer ? labelTransformer : s => s
  }

  prefixSuffixLabelTransformer(target): (string) => string {
    let prefix = this.getFunctionParameterOrDefault(target, 'withPrefix', 0, '');
    let suffix = this.getFunctionParameterOrDefault(target, 'withSuffix', 0, '');
    return s => prefix + s + suffix
  }

  toTable(
      query: FlowDataQuery,
      table: OnmsFlowTable,
      labelTransformer?: (string) => string,
  ): TableData {

    // get optionality out of the way -> all fields are required
    if (!table.headers || !table.rows) {
      throw new Error('table response did not contain all necessary information')
    }

    let ensuredLabelTranformer = this.ensureLabelTransformer(labelTransformer)
    let prefixSuffixLabelTransformer = this.prefixSuffixLabelTransformer(query)

    let toBits = FlowDatasource.isFunctionPresent(query, 'toBits');

    if (toBits) {
      let inIndex = table.headers.indexOf('Bytes In');
      let outIndex = table.headers.indexOf('Bytes Out');
      table.rows = _.map(table.rows, (row) => {
        row[inIndex] *= 8;
        row[outIndex] *= 8;
        return row;
      });
      table.headers[inIndex] = 'Bits In';
      table.headers[outIndex] = 'Bits Out';
    }

    table.rows.forEach(r => r[0] = _.flow(ensuredLabelTranformer, prefixSuffixLabelTransformer)(r[0]));

    let ecnIndex = table.headers.lastIndexOf('ECN');
    if (ecnIndex > 0) {
      table.rows = _.map(table.rows, (row) => {
        let label;
        switch(row[ecnIndex]) {
          // all flows used ecn capable transports / no congestions were reported
          case 0: label = 'ect / no ce'; break;
          // at least some flows used non-ecn-capable transports / no congestions were reported
          case 1: label = 'non-ect / no ce'; break;
          // all flows used ecn capable transports / congestions were reported
          case 2: label = 'ect / ce'; break;
          // at least some flows used non-ecn-capable transports / congestions were reported
          case 3: label = 'non-ect / ce'; break;
        }
        if (label) {
          row[ecnIndex] = label;
        }
        return row;
      });
    }

    let columns = table && table.headers ? _.map(table.headers, column => {
      return {"text": column}
    }) : [];

    return {
        refId: query.refId,
        "columns": columns,
        "rows": table.rows,
        "type": "table",
      }
  }

  toSeries(
      query: FlowDataQuery,
      flowSeries: OnmsFlowSeries,
      labelTransformer?: (string) => string,
  ): TimeSeries[] {

    // get optionality out of the way -> all fields are required
    if (!flowSeries.start || !flowSeries.end || !flowSeries.columns || !flowSeries.timestamps || !flowSeries.values) {
      throw new Error('series response did not contain all necessary information')
    }

    let ensuredLabelTranformer = this.ensureLabelTransformer(labelTransformer)
    let prefixSuffixLabelTransformer = this.prefixSuffixLabelTransformer(query)

    let toBits = FlowDatasource.isFunctionPresent(query, 'toBits');
    let perSecond = FlowDatasource.isFunctionPresent(query, 'perSecond');
    let negativeEgress = FlowDatasource.isFunctionPresent(query, 'negativeEgress');
    let negativeIngress = FlowDatasource.isFunctionPresent(query, 'negativeIngress');
    let combineIngressEgress = FlowDatasource.isFunctionPresent(query, 'combineIngressEgress');
    let onlyIngress = FlowDatasource.isFunctionPresent(query, 'onlyIngress');
    let onlyEgress = FlowDatasource.isFunctionPresent(query, 'onlyEgress');

    let start = flowSeries.start.valueOf();
    let end = flowSeries.end.valueOf();
    let columns = flowSeries.columns;
    let values = flowSeries.values;
    let timestamps = flowSeries.timestamps;
    let timestampsInRange = timestamps
        .map((timestamp, timestampIdx) => { return { timestamp, timestampIdx }})
        .filter(({timestamp}) => timestamp >= start && timestamp <= end)

    let step = timestamps[1] - timestamps[0];

    let multiplier = 1
    if (perSecond) {
      multiplier /= step / 1000;
    }
    if (toBits) {
      // Convert from bytes to bits
      multiplier *= 8;
    }

    if (combineIngressEgress) {

      return flowSeries.columns
          // use the ingress columns to drive the calculation of combined series
          .filter(column => column.ingress)
          .map(column => {

            const datapoints = timestampsInRange
                .map(({timestamp, timestampIdx}) => {
                  const sum = columns
                      // determine the indexes of those columns that have the same label as the current column
                      .map((c, colIdx) => { return { c, colIdx } })
                      .filter(({c}) => c.label === column.label)
                      // get the values in those of those columns
                      .map(({colIdx}) => values[colIdx][timestampIdx])
                      // and sum them up
                      .reduce((previous, current) => previous + (current ? current : 0), 0)
                  return [sum * multiplier, timestamp]
                })

            return {
              target: _.flow(ensuredLabelTranformer, prefixSuffixLabelTransformer)(column.label),
              datapoints
            }
          })

    } else {

      return flowSeries.columns
          .map((column, colIdx) => { return { column, colIdx } })
          .filter(({column}) => !(onlyIngress && !column.ingress || onlyEgress && column.ingress))
          .map(({column, colIdx}) => {
            const sign = negativeIngress && column.ingress || negativeEgress && !column.ingress ? -1 : 1
            const inOutLabelTransformer: (s: string) => string = s => s + (column.ingress ? ' (In)' : ' (Out)')

            const datapoints = timestampsInRange
                .map(({timestamp, timestampIdx}) => {
                  const v = values[colIdx][timestampIdx]
                  return [v === null || v === undefined || Number.isNaN(v) ? null : v * multiplier * sign, timestamp]
                })

            return {
              target: _.flow(ensuredLabelTranformer, inOutLabelTransformer, prefixSuffixLabelTransformer)(column.label),
              datapoints
            }
          })
    }

  }

  static getFirstFunction(target, name) {
    let matchingFunctions = _.filter(target.functions, function (f) {
      return f.name === name;
    });
    return matchingFunctions.length > 0 ? matchingFunctions[0] : null;
  }

  static getFunctions(target, name) {
    let matchingFunctions = _.filter(target.functions, function (f) {
      return f.name === name;
    });
    return matchingFunctions.length > 0 ? matchingFunctions : null;
  }

  static isFunctionPresent(target, name) {
    return FlowDatasource.getFirstFunction(target, name) !== null;
  }

  getFunctionParameterOrDefault(target: any, name: string, idx: number, def?: any) {
    let func = FlowDatasource.getFirstFunction(target, name);
    if (func === null) {
      // No match, use the default value
      return def;
    }

    // Return the parameter value, and perform any required template variable substitutions
    if (_.isString(func.parameters[idx])) {
      return this.templateSrv.replace(func.parameters[idx]);
    } else {
      return func.parameters[idx];
    }
  }

  getFunctionParametersOrDefault(target: any, name: string, idx: number, def?: any) {
    let funcs = FlowDatasource.getFunctions(target, name);
    if (funcs === null) {
      // No match, use the default value
      return def;
    }

    let returnFuncs = [] as any[];
    funcs.forEach((func) => {
      if (func.parameters[idx]) {
        returnFuncs.push(this.templateSrv.replace(func.parameters[idx]));
      }
    });

    // Return the parameter value, and perform any required template variable substitutions
    return returnFuncs;
  }
}
