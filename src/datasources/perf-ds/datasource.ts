import {QueryType, STRING_PROPERTY_TYPE} from './constants';
import {interpolate} from "./interpolate";
import _ from 'lodash';
import {FunctionFormatter} from '../../lib/function_formatter';
import angular from 'angular';
import {DataQuery, DataQueryRequest, DataQueryResponse, FieldType} from "@grafana/data";
import {DataQueryResponseData} from "@grafana/data/types/datasource";

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

  /** @ngInject */
  constructor(instanceSettings: any, public $q: angular.IQService, public backendSrv: any, public templateSrv: any) {
    this.type = instanceSettings.type;
    this.url = instanceSettings.url;
    this.name = instanceSettings.name;

    // This variable is referenced by the calculateInterval() method in metrics_panel_ctrl.ts
    this.interval = (instanceSettings.jsonData || {}).timeInterval;

    if (instanceSettings.jsonData && instanceSettings.jsonData.timeout) {
        this.timeout = parseInt(instanceSettings.jsonData.timeout,10) * 1000;
    }
  }

  doOpenNMSRequest(options: any): Promise<any> {
    if (this.basicAuth || this.withCredentials) {
      options.withCredentials = true;
    }
    if (this.basicAuth) {
      options.headers = options.headers || {};
      options.headers.Authorization = this.basicAuth;
    }

    options.url = this.url + options.url;
    if (this.timeout) {
      options.timeout = this.timeout;
    }

    return this.backendSrv.datasourceRequest(options);
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
    return this.$q.reject(ret);
  }

  someQueriesAreStringPropertyQueries(request: DataQueryRequest<PerfQuery>): boolean {
    return request.targets.some(query => query.type === STRING_PROPERTY_TYPE)
  }

  queryStringPropertiesOfNode(nodeId: string, queries: DefinedStringPropertyQuery[]): Promise<DataQueryResponseData[]> {
    return this.doOpenNMSRequest({
      url: '/rest/resources/fornode/' + encodeURIComponent(nodeId),
      method: 'GET',
      headers: {'Content-Type': 'application/json'}
    }).then(response => {
      return queries.flatMap(query => {
        return response.data.children.resource
            .filter(resource => (resource.id as string).endsWith(`.${query.resourceId}`))
            .flatMap(resource => this.extractStringProperty(query, resource, response.data))
      })
    })
  }

  /*
  // not used for now because the node label is not available when subresources are queried directly
  queryStringPropertiesOfResource(query: DefinedStringPropertyQuery): Promise<DataQueryResponseData[]> {
    return this.doOpenNMSRequest({
      url: '/rest/resources/node[' + query.nodeId + '].' + query.resourceId,
      method: 'GET',
      headers: {'Content-Type': 'application/json'}
    }).then(response => {
      // no node data is available when a resource is directly addressed
      // -> no node label is available
      return this.extractStringProperty(query, response.data, undefined)
    })
  }
  */

  extractStringProperty(query: DefinedStringPropertyQuery, resource: any, node?: any): DataQueryResponseData[] {
    return Object.keys(resource.stringPropertyAttributes)
        .filter(key => key === query.stringProperty)
        .flatMap(key => {
          return {
            refId: query.refId,
            fields: [
              {
                name: 'nodeId',
                type: FieldType.string,
                config: {},
                values: [query.nodeId]
              },
              {
                name: 'nodeLabel',
                type: FieldType.string,
                values: [node ? node.label : query.nodeId]
              },
              {
                name: 'resourceId',
                type: FieldType.string,
                config: {},
                values: [query.resourceId]
              },
              {
                name: 'resourceLabel',
                type: FieldType.string,
                config: {},
                values: [resource.label]
              },
              {
                name: key,
                type: FieldType.string,
                config: {},
                values: [resource.stringPropertyAttributes[key]]
              }
            ]
          }
        })
  }

  queryStringProperties(request: DataQueryRequest<StringPropertyQuery>): Promise<DataQueryResponse> {
    const definedQueries = request.targets
        .filter(q => !q.hide)
        .filter(isDefinedStringPropertyQuery)
        .map(q => {
          return {
            ...q,
            nodeId: this.templateSrv.replace(q.nodeId),
            resourceId: this.templateSrv.replace(q.resourceId)
          }
        })
    const groupedByNodeId = definedQueries.reduce<{ [key: string]: DefinedStringPropertyQuery[] }>((accu, query) => {
      (accu[query.nodeId] = accu[query.nodeId] || []).push(query)
      return accu
    }, {})
    const datas: Array<Promise<DataQueryResponseData[]>> =
        // send a request for each node separately and query the complete node resource
        // -> may be optimized in future, by batch querying for multiple nodes in one go and returning only relevant parts
        //    (could be activated by a feature toggle in opennms-js/ServerMetadata)
        Object.keys(groupedByNodeId).map((nodeId) => {
          const queries = groupedByNodeId[nodeId]
          return this.queryStringPropertiesOfNode(nodeId, queries)
        })
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
      request = this.doOpenNMSRequest({
        url: '/rest/measurements',
        data: query,
        method: 'POST',
        headers: {'Content-Type': 'application/json'}
      });
    } else {
      // There are no sources listed, let Grafana display "No data points" to the user
      return this.$q.when({ data: [] });
    }

    return request
      // Convert the results to the expected format
      .then((response) => {
        if (response.status < 200 || response.status >= 300) {
          console.warn('Response code:',response);
          return self.$q.reject(response);
        }

        return OpenNMSDatasource.processMeasurementsResponse(response);
      })
      // Sort resulting series by labels
      .then((result) => {
          result.data = _.sortBy(result.data, (s) => _.indexOf(labels, s.label));
          return result;
      })
      .catch(err => {
        return self.$q.reject(self.decorateError(err));
      });
  }

  // Used for testing the connection from the datasource configuration page
  testDatasource() {
    return this.doOpenNMSRequest({
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
      return this.$q.resolve([]);
    }

    var interpolatedQuery = _.first(this.interpolateValue(query));

    if (interpolatedQuery !== undefined) {
      const functions = FunctionFormatter.findFunctions(interpolatedQuery);

      for (const func of functions) {
        if (func.name === 'nodeFilter') {
          return this.metricFindNodeFilterQuery.apply(this, func.arguments);
        } else if (func.name === 'nodeResources') {
          return this.metricFindNodeResourceQuery.apply(this, func.arguments);
        } else {
          console.warn('Unknown function in interpolated query: ' + interpolatedQuery, func);
        }
      }
    }

    return this.$q.resolve([]);
  }

  metricFindNodeFilterQuery(query) {
    return this.doOpenNMSRequest({
      url: '/rest/nodes',
      method: 'GET',
      params: {
        filterRule: query,
        limit: 0
      }
    }).then(function (response) {
      if (response.data.count > response.data.totalCount) {
        console.warn("Filter matches " + response.data.totalCount + " records, but only " + response.data.count + " will be used.");
      }
      var results = [] as any[];
      _.each(response.data.node, function (node) {
        var nodeCriteria = node.id.toString();
        if (node.foreignId !== null && node.foreignSource !== null) {
          nodeCriteria = node.foreignSource + ":" + node.foreignId;
        }
        results.push({text: node.label, value: nodeCriteria, expandable: true});
      });
      return results;
    });
  }

  metricFindNodeResourceQuery(query, ...options) {
    var textProperty = "id", resourceType = '*';
    if (options.length > 0) {
      textProperty = options[0];
    }
    if (options.length > 1) {
      resourceType = options[1];
    }
    return this.doOpenNMSRequest({
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
    return this.doOpenNMSRequest({
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

    return this.doOpenNMSRequest({
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
    return this.doOpenNMSRequest({
      url: '/rest/measurements/filters',
      method: 'GET'
    });
  }

  suggestAttributes(nodeId: string, resourceId: string, query: string) {
    var interpolatedNodeId = _.first(this.interpolateValue(nodeId)),
        interpolatedResourceId = _.first(this.interpolateValue(resourceId));
    var remoteResourceId = OpenNMSDatasource.getRemoteResourceId(interpolatedNodeId, interpolatedResourceId);

    return this.doOpenNMSRequest({
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

    return this.doOpenNMSRequest({
      url: '/rest/resources/' + encodeURIComponent(remoteResourceId),
      method: 'GET',
      params: {
        depth: -1
      }
    }).then(function (results) {
      query = query.toLowerCase();
      var stringProperties = [] as any[];
      _.each(results.data.stringPropertyAttributes, function (value, key) {
        if (key.toLowerCase().indexOf(query) >= 0) {
          stringProperties.push(key);
        }
      });
      stringProperties.sort();

      return stringProperties;
    });
  }

}
