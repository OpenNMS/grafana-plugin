import _ from 'lodash';

import {
  DataQuery,
  DataQueryRequest,
  DataQueryResponse,
  rangeUtil,
  TableData,
  TimeSeries
} from '@grafana/data';

import { ClientDelegate } from 'lib/client_delegate';
import { dscpLabel, dscpSelectOptions } from 'lib/tos_helper';
import { processSelectionVariables, swapColumns, SimpleOpenNMSRequest } from 'lib/utils';
import { OnmsFlowTable } from 'opennms/src/model/OnmsFlowTable';
import { OnmsFlowSeries } from 'opennms/src/model/OnmsFlowSeries';

interface FlowDataQuery extends DataQuery {
  metric: string
}

export class FlowDatasource {
  type?: string;
  url?: string;
  name?: string;
  client: ClientDelegate;
  simpleRequest: SimpleOpenNMSRequest;

  /** @ngInject */
  constructor(instanceSettings: any, public backendSrv: any, public templateSrv: any) {
    this.type = instanceSettings.type;
    this.url = instanceSettings.url;
    this.name = instanceSettings.name;
    this.client = new ClientDelegate(instanceSettings, backendSrv);
    this.simpleRequest = new SimpleOpenNMSRequest(backendSrv, this.url);
  }

  query(options: DataQueryRequest<FlowDataQuery>): Promise<DataQueryResponse> {

    // filter queries that have their metric set
    // -> when we initially create a query no metric is selected
    const queriesWithMetrics = options.targets.filter(query => query.metric && !query.hide)

    const allAreSummaries = queriesWithMetrics.every(target => FlowDatasource.isFunctionPresent(target, 'asTableSummary'))
    const allAreSeries = queriesWithMetrics.every(target => !FlowDatasource.isFunctionPresent(target, 'asTableSummary'))

    if (!allAreSummaries && !allAreSeries) {
      throw new Error("The 'asTableSummary' transformation must be included in all queries of a panel or in none of them.");
    }

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
        const start = options.range.from.valueOf();
        const end = options.range.to.valueOf();
        // @ts-ignore
        step = Math.floor((end - start) / options.maxDataPoints);
      }

      const seriesPromises = queriesWithMetrics.map(query => this.querySeries(options, query, step))

      return Promise.all(seriesPromises).then(series => {
        return {
          // querySeries returns TimeSeries[]
          // -> flatMap it
          data: series.flatMap(s => s)
        }
      })
    }

  }

  querySummary(options: DataQueryRequest, query: FlowDataQuery): Promise<TableData> {

    let start = options.range.from.valueOf();
    let end = options.range.to.valueOf();

    // Combine
    let N = this.getFunctionParameterOrDefault(query, 'topN', 0, 10);
    let includeOther = FlowDatasource.isFunctionPresent(query, 'includeOther');
    // Filter
    let exporterNode = this.getFunctionParameterOrDefault(query, 'withExporterNode', 0);
    let ifIndex = this.getFunctionParameterOrDefault(query, 'withIfIndex', 0);
    let dscp = processSelectionVariables(this.getFunctionParametersOrDefault(query, 'withDscp', 0, null));
    let applications = processSelectionVariables(this.getFunctionParametersOrDefault(query, 'withApplication', 0, null));
    let conversations = processSelectionVariables(this.getFunctionParametersOrDefault(query, 'withConversation', 0, null));
    let hosts = processSelectionVariables(this.getFunctionParametersOrDefault(query, 'withHost', 0, null));

    switch (query.metric) {
      case 'conversations':
        if (conversations && conversations.length > 0) {
          return this.client.getSummaryForConversations(conversations, start, end, includeOther, exporterNode, ifIndex, dscp).then(table => {
            return this.toTable(query, table)
          });
        } else {
          return this.client.getSummaryForTopNConversations(N, start, end, includeOther, exporterNode, ifIndex, dscp).then(table => {
            return this.toTable(query, table)
          });
        }
      case 'applications':
        if (applications && applications.length > 0) {
          return this.client.getSummaryForApplications(applications, start, end, includeOther, exporterNode, ifIndex, dscp).then(table => {
            return this.toTable(query, table)
          });
        } else {
          return this.client.getSummaryForTopNApplications(N, start, end, includeOther, exporterNode, ifIndex, dscp).then(table => {
            return this.toTable(query, table)
          });
        }
      case 'hosts':
        if (hosts && hosts.length > 0) {
          return this.client.getSummaryForHosts(hosts, start, end, includeOther, exporterNode, ifIndex, dscp).then(table => {
            return this.toTable(query, table)
          });
        } else {
          return this.client.getSummaryForTopNHosts(N, start, end, includeOther, exporterNode, ifIndex, dscp).then(table => {
            return this.toTable(query, table)
          });
        }
      case 'dscps':
        return this.client.getSummaryForDscps(start, end, exporterNode, ifIndex, dscp).then(table => {
          return this.toTable(query, table, dscpLabel)
        });
      default:
        throw 'Unsupported target metric: ' + query.metric;
    }
  }

  querySeries(options: DataQueryRequest, query: FlowDataQuery, step: number): Promise<TimeSeries[]> {

    let start = options.range.from.valueOf();
    let end = options.range.to.valueOf();

    // Combine
    let N = this.getFunctionParameterOrDefault(query, 'topN', 0, 10);
    let includeOther = FlowDatasource.isFunctionPresent(query, 'includeOther');
    // Filter
    let exporterNode = this.getFunctionParameterOrDefault(query, 'withExporterNode', 0);
    let ifIndex = this.getFunctionParameterOrDefault(query, 'withIfIndex', 0);
    let dscp = processSelectionVariables(this.getFunctionParametersOrDefault(query, 'withDscp', 0, null));
    let applications = processSelectionVariables(this.getFunctionParametersOrDefault(query, 'withApplication', 0, null));
    let conversations = processSelectionVariables(this.getFunctionParametersOrDefault(query, 'withConversation', 0, null));
    let hosts = processSelectionVariables(this.getFunctionParametersOrDefault(query, 'withHost', 0, null));

    switch (query.metric) {
      case 'conversations':
        if (conversations && conversations.length > 0) {
          return this.client.getSeriesForConversations(conversations, start, end, step, includeOther, exporterNode, ifIndex, dscp).then(series => {
            return this.toSeries(query, series)
          });
        } else {
          return this.client.getSeriesForTopNConversations(N, start, end, step, includeOther, exporterNode, ifIndex, dscp).then(series => {
            return this.toSeries(query, series)
          });
        }
      case 'applications':
        if (applications && applications.length > 0) {
          return this.client.getSeriesForApplications(applications, start, end, step, includeOther, exporterNode, ifIndex, dscp).then(series => {
            return this.toSeries(query, series)
          });
        } else {
          return this.client.getSeriesForTopNApplications(N, start, end, step, includeOther, exporterNode, ifIndex, dscp).then(series => {
            return this.toSeries(query, series)
          });
        }
      case 'hosts':
        if (hosts && hosts.length > 0) {
          return this.client.getSeriesForHosts(hosts, start, end, step, includeOther, exporterNode, ifIndex, dscp).then(series => {
            return this.toSeries(query, series)
          });
        } else {
          return this.client.getSeriesForTopNHosts(N, start, end, step, includeOther, exporterNode, ifIndex, dscp).then(series => {
            return this.toSeries(query, series)
          });
        }
      case 'dscps':
        return this.client.getSeriesForDscps(start, end, step, exporterNode, ifIndex, dscp).then(series => {
          return this.toSeries(query, series, dscpLabel)
        });
      default:
        throw 'Unsupported target metric: ' + query.metric;
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

    let applications = /applications\((\d*)\)/;
    let conversations = /conversations\((\d*)\)/;
    let hosts = /hosts\((\d*)\)/;
    let locations = /locations\((.*)\)/;
    let exporterNodesRegex = /exporterNodesWithFlows\((.*)\)/;
    let interfacesOnExporterNodeRegex = /interfacesOnExporterNodeWithFlows\(\s*([^,]+).*\)/; // just pick the first arg and ignore anything else
    let dscpOnExporterNodeAndInterfaceRegex = /dscpOnExporterNodeAndInterface\(\s*([^,]+),\s*([^,]+),\s*([^,]+),\s*([^\s]+\s*)\)/;

    const start = this.templateSrv.timeRange.from.valueOf();
    const end = this.templateSrv.timeRange.to.valueOf();

    let locationsQuery = query.match(locations);
    if (locationsQuery) {
      return this.metricFindLocations();
    }

    let applicationsQuery = query.match(applications);
    if (applicationsQuery) {
      let limit = applicationsQuery.length > 1 && !isNaN(parseInt(applicationsQuery[1], 10)) ? parseInt(applicationsQuery[1], 10) : 0;
      return this.metricFindApplications(start, end, limit);
    }

    let conversationsQuery = query.match(conversations);
    if (conversationsQuery) {
      let limit = conversationsQuery.length > 1 && !isNaN(parseInt(conversationsQuery[1], 10)) ? parseInt(conversationsQuery[1], 10) : 0;
      return this.metricFindConversations(start, end, limit);
    }

    let hostsQuery = query.match(hosts);
    if (hostsQuery) {
      let limit = hostsQuery.length > 1 && !isNaN(parseInt(hostsQuery[1], 10)) ? parseInt(hostsQuery[1], 10) : 0;
      return this.metricFindHosts(start, end, limit);
    }

    let exporterNodesQuery = query.match(exporterNodesRegex);
    if (exporterNodesQuery) {
      let exporterNodesQueryFilter = exporterNodesQuery.length > 1 ? exporterNodesQuery[1] : null;
      return this.metricFindExporterNodes(query, exporterNodesQueryFilter);
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

  metricFindLocations() {
    return this.simpleRequest.getLocations();
  }

  metricFindApplications(start: number, end: number, limit = 0){    
    return this.simpleRequest.getApplications(start, end, limit);    
  }
  
  metricFindHosts(start: number, end: number, limit = 0){
    return this.simpleRequest.getHosts(start, end, limit);
  }

  metricFindConversations(start: number, end: number, limit = 0){
    return this.simpleRequest.getConversations(start, end, limit);
  }
  

  metricFindExporterNodes(query?: any, filter?: string) {
    let self = this;
    return this.client.getExporters().then((exporters) => {
      let results = [] as any[];
      _.each(exporters, function (exporter) {
        results.push({text: exporter.label, value: exporter.id, expandable: true});
      });
      return self.getFilteredNodes(results, filter);
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
    let swapIngressEgress = FlowDatasource.isFunctionPresent(query, 'swapIngressEgress');

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

    if(Array.isArray(table.rows) && table.rows.length > 0 && swapIngressEgress){
      let inIndex = table.headers.indexOf('Bytes In');
      let outIndex = table.headers.indexOf('Bytes Out');
      table.rows = swapColumns(table.rows, inIndex, outIndex);
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
    let nanToZero = FlowDatasource.isFunctionPresent(query, 'nanToZero');
    let swapIngressEgress = FlowDatasource.isFunctionPresent(query, 'swapIngressEgress');

    if (swapIngressEgress) {
      flowSeries.columns = flowSeries.columns.map((column) => {
        column.ingress = !column.ingress;
        return column;
      });
    }

    let start = flowSeries.start.valueOf();
    let end = flowSeries.end.valueOf();
    let columnsWithIndex = flowSeries.columns.map((column, colIdx) => { return { column, colIdx }});
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
      const uniqueColumns =  [...new Set(flowSeries.columns.map(col => col.label))];
      
      return uniqueColumns          
          .map(col => {

            const datapoints = timestampsInRange
                .map(({timestamp, timestampIdx}) => {
                  const sum = columnsWithIndex
                      // determine the indexes of those columns that have the same label as the current column
                      .filter(({column}) => column.label === col)
                      // get the values of those columns ...
                      .map(({colIdx}) => {
                        const v = values[colIdx][timestampIdx]
                       return isNaN(Number(v)) && nanToZero ? 0 : v 
                      })
                      // ... and sum them up
                      .reduce((previous, current) => previous + (current ? current : 0), 0)
                  return [sum * multiplier, timestamp]
                })

            return {
              target: _.flow(ensuredLabelTranformer, prefixSuffixLabelTransformer)(col),
              datapoints
            }
          })

    } else {

      return columnsWithIndex
          .filter(({column}) => !(onlyIngress && !column.ingress || onlyEgress && column.ingress))
          .map(({column, colIdx}) => {
            const sign = negativeIngress && column.ingress || negativeEgress && !column.ingress ? -1 : 1
            const inOutLabelTransformer: (s: string) => string = s => s + (column.ingress ? ' (In)' : ' (Out)')

            const datapoints = timestampsInRange
                .map(({timestamp, timestampIdx}) => {
                  const v = Number(values[colIdx][timestampIdx])
                  return [isNaN(v) ? nanToZero ? 0 : null : v * multiplier * sign, timestamp]
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

  getFilteredNodes(exporterNodes?: any[], filter?: string): Promise<any> {
    let promises: Array<Promise<any>> = [];
    let propValue = filter ? filter.split('=') : null;
    if (propValue && propValue.length === 2) {
      let propertyKey = propValue[0].trim();
      let propertyValue = propValue[1].trim().replace(/^["'](.+(?=["']$))["']$/, '$1');

      _.each(exporterNodes, (exportedNode) => {
        let promise = this.client.getNode(exportedNode.value)
          .then(n => {
            let p = n[propertyKey];
            if (p === propertyValue) {
              return exportedNode;
            }
          });
        promises.push(promise);
      });
      return Promise.all(promises).then(results => {
        return results.filter(result => result)
      });
    } else { return Promise.resolve(exporterNodes); }
  }

}
