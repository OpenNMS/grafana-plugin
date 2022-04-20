import {QueryType, STRING_PROPERTY_TYPE} from './constants';
import {interpolate} from "./interpolate";
import _ from 'lodash';
import {FunctionFormatter} from '../../lib/function_formatter';
import {DataQuery, DataQueryRequest, DataQueryResponse, Field, FieldType} from "@grafana/data";
import {DataQueryResponseData} from "@grafana/data/types/datasource";
import {ClientDelegate} from '../../lib/client_delegate'
import {Client, ServerMetadata} from 'opennms'
import {SimpleOpenNMSRequest} from '../../lib/utils'

interface Query {
  start: number;
  end: number;
  step: number;
  relaxed: boolean; // enable relaxed mode, which allows for missing attributes
  maxrows: number;
  source: any[];
  expression: any[];
  filter: any;
}

interface PerfQuery extends DataQuery {
  type?: string
}

interface StringPropertyQuery extends PerfQuery {
  nodeId?: string
  resourceId?: string
  stringProperty?: string
}

type DefinedStringPropertyQuery = Required<StringPropertyQuery>

function isDefinedStringPropertyQuery(q: StringPropertyQuery | undefined): q is DefinedStringPropertyQuery {
  return q !== undefined && !!q.nodeId && !!q.resourceId && !!q.stringProperty
}

export class OpenNMSDatasource {
  type?: string;
  url?: string;
  name?: string;
  basicAuth?: string;
  withCredentials = false;
  interval: number;
  timeout = 10000;
  searchLimit = 25;
  target?: any;
  clientDelegate: ClientDelegate;
  simpleRequest: SimpleOpenNMSRequest;

  /** @ngInject */
  constructor(instanceSettings: any, public backendSrv: any, public templateSrv: any) {
    this.type = instanceSettings.type;
    this.url = instanceSettings.url;
    this.name = instanceSettings.name;
    this.clientDelegate = new ClientDelegate(instanceSettings, backendSrv);
    this.simpleRequest = new SimpleOpenNMSRequest(backendSrv, this.url);

    // This variable is referenced by the calculateInterval() method in metrics_panel_ctrl.ts
    this.interval = (instanceSettings.jsonData || {}).timeInterval;

    if (instanceSettings.jsonData && instanceSettings.jsonData.timeout) {
        this.timeout = parseInt(instanceSettings.jsonData.timeout,10) * 1000;
    }
  }



  decorateError(err: any) {
    let ret = err;
    if (err.err) {
      ret = err.err;
    }
    let statusText = ret.statusText || 'Request failed.';

    // cancelled property causes the UI to never complete on failure
    if (ret.cancelled) {
      delete ret.cancelled;
      statusText = 'Request timed out.';
    }
    if (err.cancelled) {
      delete err.cancelled;
      statusText = 'Request timed out.';
    }

    if (!ret.message) {
      ret.message = statusText;
    }
    if (!ret.status) {
      ret.status = 'error';
    }
    return Promise.reject(ret);
  }

  // constructs a single string valued data frame field
  private static field(name: string, value: string): Field<string, string[]> {
    return {
      name,
      type: FieldType.string,
      config: {},
      values: [value]
    }
  }

  // constructs all data frame fields for a string property
  private static fields(o: { nodeId: string, nodeLabel: string, resourceId: string, resourceLabel: string, stringPropertyKey: string, stringPropertyValue: string }): Array<Field<string, string[]>> {
    return [
      OpenNMSDatasource.field('nodeId', o.nodeId),
      OpenNMSDatasource.field('nodeLabel', o.nodeLabel),
      OpenNMSDatasource.field('resourceId', o.resourceId),
      OpenNMSDatasource.field('resourceLabel', o.resourceLabel),
      OpenNMSDatasource.field(o.stringPropertyKey, o.stringPropertyValue)
    ]
  }

  queryStringPropertiesOfNode(nodeId: string, queries: DefinedStringPropertyQuery[]): Promise<DataQueryResponseData[]> {
    return this.simpleRequest.doOpenNMSRequest( {
      url: '/rest/resources/fornode/' + encodeURIComponent(nodeId),
      method: 'GET'
    }).then(response => {
      return queries.flatMap(query => {
        return response.data.children.resource
            .filter(resource => (resource.id as string).endsWith(`.${query.resourceId}`))
            .flatMap(resource => this.extractStringProperty(query, resource, response.data))
      })
    })
  }

  queryAllStringProperties(selection: { nodes: Set<string>, nodeSubresources: Set<string>, stringProperties: Set<string> }): Promise<DataQueryResponseData[]> {
    return this.simpleRequest.doOpenNMSRequest({
      url: '/rest/resources/select',
      method: 'GET',
      params: {
        nodes: Array.from(selection.nodes).join(','),
        nodeSubresources: Array.from(selection.nodeSubresources).join(','),
        stringProperties: Array.from(selection.stringProperties).join(',')
      }
    }).then(response =>
        response.data.flatMap(node =>
            node.children.resource.flatMap(resource =>
                Object.entries<string>(resource.stringPropertyAttributes).flatMap(([key, value]) => {
                      return {
                        fields: OpenNMSDatasource.fields({
                          nodeId: node.name,
                          nodeLabel: node.label,
                          resourceId: resource.name,
                          resourceLabel: resource.label,
                          stringPropertyKey: key,
                          stringPropertyValue: value
                        })
                      }
                    }
                )
            )
        )
    )
  }

  private extractStringProperty(query: DefinedStringPropertyQuery, resource: any, node?: any): DataQueryResponseData[] {
    return Object.keys(resource.stringPropertyAttributes)
        .filter(key => key === query.stringProperty)
        .flatMap(key => {
          return {
            refId: query.refId,
            fields: OpenNMSDatasource.fields({
              nodeId: query.nodeId,
              nodeLabel: node ? node.label : query.nodeId,
              resourceId: query.resourceId,
              resourceLabel: resource.label,
              stringPropertyKey: key,
              stringPropertyValue: resource.stringPropertyAttributes[key]
            })
          }
        })
  }

  queryStringProperties(request: DataQueryRequest<StringPropertyQuery>): Promise<DataQueryResponse> {
    const definedQueries: DefinedStringPropertyQuery[] = request.targets
        .filter(q => !q.hide)
        .filter(isDefinedStringPropertyQuery)
        .map(q => {
          return {
            ...q,
            nodeId: this.templateSrv.replace(q.nodeId),
            resourceId: this.templateSrv.replace(q.resourceId)
          }
        })
    return this.clientDelegate.getClientWithMetadata().then((client: Client) => {
      const metadata: ServerMetadata = client.http.server.metadata;
      if (metadata.selectPartialResources()) {
        return this.queryStringPropertiesForAllNodesInBulk(definedQueries)
      } else {
        return this.queryStringPropertiesForEachNodeSeparately(definedQueries)
      }
    })
  }

  queryStringPropertiesForAllNodesInBulk(definedQueries: DefinedStringPropertyQuery[]): Promise<DataQueryResponse> {
    // send a single request that selects all nodes, subresources, and string properties
    const selection =
        definedQueries.reduce<{ nodes: Set<string>, nodeSubresources: Set<string>, stringProperties: Set<string> }>((accu, query) => {
          accu.nodes.add(query.nodeId)
          accu.nodeSubresources.add(query.resourceId)
          accu.stringProperties.add(query.stringProperty)
          return accu
        }, {nodes: new Set(), nodeSubresources: new Set(), stringProperties: new Set()})
    return this.queryAllStringProperties(selection).then(datas => {
      return {
        data: datas
      }
    })
  }

  queryStringPropertiesForEachNodeSeparately(definedQueries: DefinedStringPropertyQuery[]): Promise<DataQueryResponse> {
    const groupedByNodeId = definedQueries.reduce<{ [key: string]: DefinedStringPropertyQuery[] }>((accu, query) => {
      (accu[query.nodeId] = accu[query.nodeId] || []).push(query)
      return accu
    }, {})
    // send a request for each node separately...
    const datas: Array<Promise<DataQueryResponseData[]>> =
        Object.keys(groupedByNodeId).map((nodeId) => {
          const queries = groupedByNodeId[nodeId]
          return this.queryStringPropertiesOfNode(nodeId, queries)
        })
    // ... and then combine all results into a single DataQueryResponse
    return Promise.all(datas).then(datas => {
      return {
        data: datas.flatMap(x => x)
      }
    })
  }

  query(options: DataQueryRequest<PerfQuery>) {
    const queries = options.targets.filter(q => !q.hide && q.type)
    if (queries.every(query => query.type === STRING_PROPERTY_TYPE)) {
      return this.queryStringProperties(options)
    } else if (queries.some(query => query.type === STRING_PROPERTY_TYPE)) {
      return Promise.resolve(
          {
            data: [],
            error: {
              message: "string property queries can not be mixed with other kinds of queries"
            }
          }
      )
    }

    const self = this;

    // Generate the query
    var [query, labels] = this.buildQuery(options);

    // Issue the request
    var request;
    if (!Array.isArray(query) && query.source.length > 0) {
      request = this.simpleRequest.doOpenNMSRequest({
        url: '/rest/measurements',
        data: query,
        method: 'POST',
        headers: {'Content-Type': 'application/json'}
      });
    } else {
      // There are no sources listed, let Grafana display "No data points" to the user
      return Promise.resolve({ data: [] });
    }

    return request
      // Convert the results to the expected format
      .then((response) => {
        if (response.status < 200 || response.status >= 300) {
          console.warn('Response code:',response);
          return Promise.reject(response);
        }

        return OpenNMSDatasource.processMeasurementsResponse(response);
      })
      // Sort resulting series by labels
      .then((result) => {
          result.data = _.sortBy(result.data, (s) => _.indexOf(labels, s.label));
          return result;
      })
      .catch(err => {
        return Promise.reject(self.decorateError(err));
      });
  }

  // Used for testing the connection from the datasource configuration page
  testDatasource() {
    return this.simpleRequest.doOpenNMSRequest({
      url: '/rest/info',
      method: 'GET'
    }).then(response => {
      if (response.status === 200) {
        return {status: "success", message: "Data source is working", title: "Success"};
      } else {
        return {
          status: "danger",
          message: "OpenNMS provided a response, but no metadata was found.",
          title: "Unexpected Response " + response.status
        };
      }
    }).catch(err => {
      return this.decorateError(err);
    });
  }

  // Used by template queries
  metricFindQuery(query) {
    if (query === null || query === undefined || query === "") {
      return Promise.resolve([]);
    }

    var interpolatedQuery = _.first(this.interpolateValue(query));

    if (interpolatedQuery !== undefined) {
      const functions = FunctionFormatter.findFunctions(interpolatedQuery);

      for (const func of functions) {
        if(func.name === 'locations'){
          return this.metricFindLocations.apply(this, func.arguments);
        } else if (func.name === 'nodeFilter') {
          return this.metricFindNodeFilterQuery.apply(this, func.arguments);
        } else if (func.name === 'nodeResources') {
          return this.metricFindNodeResourceQuery.apply(this, func.arguments);
        } else {
          console.warn('Unknown function in interpolated query: ' + interpolatedQuery, func);
        }
      }
    }

    return Promise.resolve([]);
  }

  metricFindLocations() {
    return this.simpleRequest.getLocations();
  }

  metricFindNodeFilterQuery(query) {
    return this.simpleRequest.getNodesByFilter(query);
  }

  metricFindNodeResourceQuery(query, ...options) {
    var textProperty = "id", resourceType = '*';
    if (options.length > 0) {
      textProperty = options[0];
    }
    if (options.length > 1) {
      resourceType = options[1];
    }
    return this.simpleRequest.doOpenNMSRequest({
      url: '/rest/resources/' + encodeURIComponent(OpenNMSDatasource.getNodeResource(query)),
      method: 'GET',
      params: {
        depth: 1
      }
    }).then(function (response) {
      var results = [] as any[];
      _.each(response.data.children.resource, function (resource) {
        var resourceWithoutNodePrefix = resource.id.match(/node(Source)?\[.*?\]\.(.*)/);
        var textValue;
        switch (textProperty) {
          case "id":
            textValue = resourceWithoutNodePrefix[2];
            break;
          case "label":
            textValue = resource.label;
            break;
          case "name":
            textValue = resource.name;
            break;
          default:
            textValue = resourceWithoutNodePrefix[2];
            console.warn(`Unknown resource property '${textProperty}' specified. Using 'id' instead.`);
        }
        if ((resourceType === '*' && resourceWithoutNodePrefix) || (resourceWithoutNodePrefix[2].indexOf(resourceType + '[') === 0)) {
          results.push({text: textValue, value: resourceWithoutNodePrefix[2], expandable: true});
        }
      });
      return results;
    });
  }

  buildQuery(options): [Query, string[]] {
    const maxDataPoints = options.maxDataPoints || 300;
    const intervalMs = options.intervalMs || 60 * 1000;

    var self = this,
      start = options.range.from.valueOf(),
      end = options.range.to.valueOf(),
      step = Math.floor((end - start) / maxDataPoints);
      step = (step < intervalMs) ? intervalMs : step;

    var query = {
      start: start,
      end: end,
      step: step,
      relaxed: true, // enable relaxed mode, which allows for missing attributes
      maxrows: maxDataPoints,
      source: [] as any[],
      expression: [] as any[],
    } as Query;

    var labels = [] as string[];

    _.each(options.targets, function (target) {
      var transient = false;
      if (target.hide) {
        transient = true;
      }

      if (target.type === QueryType.Attribute) {
        if (!((target.nodeId && target.resourceId && target.attribute))) {
          return;
        }

        var label = target.label;
        if (label === undefined || label === '') {
          label = target.attribute;
        }

        // Build the source
        let source = {
          aggregation: target.aggregation,
          attribute: target.attribute,
          label: label,
          resourceId: target.resourceId,
          nodeId: target.nodeId, // temporary attribute used for interpolation
          transient: transient,
          datasource: undefined as any,
        };

        if (target.subattribute !== undefined && target.subattribute !== '') {
          source.datasource = target.subattribute;
        }
        if (target.fallbackAttribute !== undefined && target.fallbackAttribute !== '') {
          source['fallback-attribute'] = target.fallbackAttribute;
        }

        // Perform variable substitution - may generate additional queries
        source = self.interpolateSourceVariables(source, options.scopedVars, (interpolatedSource: any) => {
            // Calculate the effective resource id after the interpolation
            interpolatedSource.resourceId = OpenNMSDatasource.getRemoteResourceId(interpolatedSource.nodeId, interpolatedSource.resourceId);
            delete interpolatedSource.nodeId;
        });
        query.source = query.source.concat(source);

        labels = labels.concat(_.map(source, 'label'));

      } else if (target.type === QueryType.Expression) {
        if (!((target.label && target.expression))) {
          return;
        }

        // Build the expression
        let expression = {
          "label": target.label,
          "value": target.expression,
          "transient": transient
        };

        // Perform variable substitution - may generate additional expressions
        expression = self.interpolateExpressionVariables(expression, options.scopedVars);
        query.expression = query.expression.concat(expression);

        labels = labels.concat(_.map(expression, 'label'));

      } else if (target.type === QueryType.Filter) {
        if (!((target.filter))) {
          return;
        }

        // Interpolate the filter parameters
        var interpolatedFilterParms = self.interpolateVariables(target.filterParameters, _.keys(target.filterParameters), options.scopedVars);

        var filters = _.map(interpolatedFilterParms, (filterParms) => {
          // Build the filter definition
          var parameters = [] as any[];
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

    return [query, labels];
  }

  interpolateSourceVariables(source: any, scopedVars: any, callback?: (value: any) => void) {
    return this.interpolateVariables(source, ['nodeId', 'resourceId', 'attribute', 'datasource', 'label'], scopedVars, callback);
  }

  interpolateExpressionVariables(expression: any, scopedVars?: any) {
    return this.interpolateVariables(expression, ['value', 'label'], scopedVars);
  }

  interpolateValue(value: any, scopedVars?: any) {
    return _.map(this.interpolateVariables({'value': value}, ['value'], scopedVars), function(entry) {
      return entry.value;
    });
  }

  interpolateVariables(object: any, attributes: any, scopedVars: any, callback?: (value: any) => void) {
    // Reformat the variables to work with our interpolate function
    var variables = [] as any[];
    _.each(this.templateSrv.variables, function(templateVariable) {
      var variable = {
        name: templateVariable.name,
        value: [] as any[]
      };

      // If this templateVar exists in scopedVars, we need to look at the scoped values
      if (scopedVars && scopedVars[variable.name] !== undefined) {
        variable.value.push(scopedVars[variable.name].value);
      } else {
        // Single-valued?
        if (_.isString(templateVariable.current.value)) {
          variable.value.push(templateVariable.current.value);
        } else {
          _.each(templateVariable.current.value, function(value) {
            if (value === "$__all") {
              _.each(templateVariable.options, function(option) {
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
      }

      variables.push(variable);
    });
    return interpolate(object, attributes, variables, callback);
  }

  static processMeasurementsResponse(response) {
    var labels = response.data.labels;
    var columns = response.data.columns;
    var timestamps = response.data.timestamps;
    var metadata = response.data.metadata;
    var series = [] as any[];
    var i, j, nRows, nCols, datapoints;
    var value, atLeastOneNonNaNValue;

    if (timestamps !== undefined) {
      nRows = timestamps.length;
      nCols = columns.length;

      for (i = 0; i < nCols; i++) {
        atLeastOneNonNaNValue = false;
        datapoints = [] as any[];
        for (j = 0; j < nRows; j++) {
          // Skip rows that are out-of-ranges - this can happen with RRD data in narrow time spans
          if (timestamps[j] < response.data.start || timestamps[j] > response.data.end) {
            continue;
          }

          value = columns[i].values[j];
          // Replace literal 'NaN' values with nulls
          if (value === 'NaN') {
            value = null;
          }

          if (!atLeastOneNonNaNValue && !isNaN(value)) {
            atLeastOneNonNaNValue = true;
          }
          datapoints.push([value, timestamps[j]]);
        }

        let label = labels[i];
        if (metadata && metadata.resources) {
          label = FunctionFormatter.format(label, metadata);
        }

        // Skip series that are all NaNs
        // When querying in relaxed mode, expressions that operate against attribute that are missing may only contain
        // NaNs. In this case, we don't want to show them at all.
        if (atLeastOneNonNaNValue) {
          series.push({
            target: label,
            label: labels[i],
            datapoints: datapoints
          });
        }
      }
    }

    return {data: series};
  }

  static flattenResourcesWithAttributes(resources, resourcesWithAttributes) {
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

  static getNodeResource(nodeId) {
    var prefix = "";
    if (nodeId.indexOf(":") > 0) {
      prefix = "nodeSource[";
    } else {
      prefix = "node[";
    }
    return prefix + nodeId + "]";
  }

  static getRemoteResourceId(nodeId, resourceId) {
    return OpenNMSDatasource.getNodeResource(nodeId) + "." + resourceId;
  }

  searchForNodes(query, offset) {
    return this.simpleRequest.doOpenNMSRequest({
      url: '/rest/nodes',
      method: 'GET',
      params: {
        offset: offset,
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

  getResourcesWithAttributesForNode(nodeId) {
    var interpolatedNodeId = _.first(this.interpolateValue(nodeId));

    return this.simpleRequest.doOpenNMSRequest({
      url: '/rest/resources/fornode/' + encodeURIComponent(interpolatedNodeId),
      method: 'GET',
      params: {
        depth: -1
      }
    }).then(function (results) {
      return OpenNMSDatasource.flattenResourcesWithAttributes([results.data], []);
    });
  }

  getAvailableFilters() {
    return this.simpleRequest.doOpenNMSRequest({
      url: '/rest/measurements/filters',
      method: 'GET'
    });
  }

  suggestAttributes(nodeId: string, resourceId: string, query: string) {
    var interpolatedNodeId = _.first(this.interpolateValue(nodeId)),
        interpolatedResourceId = _.first(this.interpolateValue(resourceId));
    var remoteResourceId = OpenNMSDatasource.getRemoteResourceId(interpolatedNodeId, interpolatedResourceId);

    return this.simpleRequest.doOpenNMSRequest({
      url: '/rest/resources/' + encodeURIComponent(remoteResourceId),
      method: 'GET',
      params: {
        depth: -1
      }
    }).then(function (results) {
      query = query.toLowerCase();
      var attributes = [] as any[];
      _.each(results.data.rrdGraphAttributes, function (value, key) {
        if (key.toLowerCase().indexOf(query) >= 0) {
          attributes.push(key);
        }
      });
      attributes.sort();

      return attributes;
    });
  }

  suggestStringProperties(nodeId: string, resourceId: string, query: string) {
    var interpolatedNodeId = _.first(this.interpolateValue(nodeId)),
        interpolatedResourceId = _.first(this.interpolateValue(resourceId));
    var remoteResourceId = OpenNMSDatasource.getRemoteResourceId(interpolatedNodeId, interpolatedResourceId);

    return this.simpleRequest.doOpenNMSRequest({
      url: '/rest/resources/' + encodeURIComponent(remoteResourceId),
      method: 'GET',
      params: {
        depth: -1
      }
    }).then(function (results) {
      query = query.toLowerCase();
      var stringProperties = [] as any[];
      _.each(results.data.stringPßropertyAttributes, function (value, key) {
        if (key.toLowerCase().indexOf(query) >= 0) {
          stringProperties.push(key);
        }
      });
      stringProperties.sort();

      return stringProperties;
    });
  }

}
