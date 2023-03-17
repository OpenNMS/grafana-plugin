import { DataFrame, DataQueryResponse, DataSourceApi, DataSourceInstanceSettings } from "@grafana/data";
import { ClientDelegate } from "lib/client_delegate";
import { SimpleOpenNMSRequest } from "lib/utils";
import { PerformanceTypeOptions } from "./constants";
import { measurementResponseToDataFrame } from "./PerformanceHelpers";
import {
    OnmsMeasurementsQueryRequest,
    OnmsMeasurementsQueryResponse,
    OnmsMeasurementsQuerySource,
    OnmsResourceDto,
    PerformanceDataSourceOptions,
    PerformanceQuery,
    PerformanceQueryRequest
} from "./types";
import { collectInterpolationVariables, interpolate } from "./queries/interpolate";
import {
    buildAttributeQuerySource,
    buildExpressionQuery,
    buildFilterQuery,
    buildPerformanceMeasurementQuery,
    getRemoteResourceId,
    isValidAttributeTarget,
    isValidExpressionTarget,
    isValidFilterTarget,
    isValidMeasurementQuery
} from "./queries/queryBuilder";
import { queryStringProperties } from "./queries/queryStringProperties"
import { TemplateSrv, getTemplateSrv, getBackendSrv } from "@grafana/runtime";

export class PerformanceDataSource extends DataSourceApi<PerformanceQuery> {
    type: string;
    url?: string;
    name: string;
    client: ClientDelegate;
    simpleRequest: SimpleOpenNMSRequest;
    templateSrv: TemplateSrv;

    constructor(instanceSettings: DataSourceInstanceSettings<PerformanceDataSourceOptions>) {
        super(instanceSettings);
        this.type = instanceSettings.type;
        this.url = instanceSettings.url;
        this.name = instanceSettings.name;
        this.client = new ClientDelegate(instanceSettings, getBackendSrv());
        this.simpleRequest = new SimpleOpenNMSRequest(getBackendSrv(), this.url);
        this.templateSrv = getTemplateSrv();
    }

    isQueryValidStringPropertySearch = (targets: PerformanceQuery[]) => {
        const activeTargets = targets.filter(t => !t.hide)

        const totalStrings = activeTargets.filter((t) =>
            t.performanceType.value &&
            t.performanceType.value === PerformanceTypeOptions.StringProperty.value &&
            t.performanceState?.stringProperty?.value)
        let typeOfQuery = 'normal'

        if (totalStrings.length > 0 && totalStrings.length === activeTargets.length) {
            typeOfQuery = 'string'
        } else if (totalStrings.length > 0 && totalStrings.length < activeTargets.length) {
            typeOfQuery = 'invalid'
        }
        return typeOfQuery;
    }

    createSourcesFromAttributeTarget(query: OnmsMeasurementsQueryRequest,
      target: PerformanceQuery, options: PerformanceQueryRequest<PerformanceQuery>) {

      const source = buildAttributeQuerySource(target);
      const interpolationVars = collectInterpolationVariables(this.templateSrv, options.scopedVars)
      const attributes = ['nodeId', 'resourceId', 'attribute', 'datasource', 'label']

      const callback = (interpolatedSource: OnmsMeasurementsQuerySource) => {
          if (interpolatedSource.nodeId !== undefined) {
              // Calculate the effective resource id after the interpolation
              interpolatedSource.resourceId = getRemoteResourceId(interpolatedSource.nodeId, interpolatedSource.resourceId);
              // remove nodeId since it should not be part of the Rest API measurement request payload
              delete interpolatedSource.nodeId;
          }
      }

      const sources = interpolate(source, attributes, interpolationVars, callback)

      if (query.source && query.source.length > 0) {
          return query.source.concat(sources)
      }

      return sources
    }

    createExpressionsFromExpressionTarget(query: OnmsMeasurementsQueryRequest,
      target: PerformanceQuery, options: PerformanceQueryRequest<PerformanceQuery>, i: number) {

      const expression = buildExpressionQuery(target, i)
      const interpolationVars = collectInterpolationVariables(this.templateSrv, options.scopedVars)
      const attributes = ['value', 'label']

      const expressions = interpolate(expression, attributes, interpolationVars)

      if (query.expression && query.expression.length > 0) {
          return query.expression.concat(expressions)
      }

      return expressions
    }

    createFiltersFromFilterTarget(query: OnmsMeasurementsQueryRequest,
      target: PerformanceQuery, options: PerformanceQueryRequest<PerformanceQuery>) {

      const interpolationVars = collectInterpolationVariables(this.templateSrv, options.scopedVars)
      const attributes = Object.keys(target.filterState)
      const interpolatedFilterParams = interpolate(target.filterState, attributes, interpolationVars)
      const filters = buildFilterQuery(target, interpolatedFilterParams)

      // Only add the filter attribute to the query when one or more filters are specified since
      // OpenNMS versions before 17.0.0 do not support it
      if (filters.length > 0) {
          if (query.filter && query.filter.length > 0) {
              return query.filter.concat(filters)
          } else {
              return filters
          }
      }

      return query.filter
    }

    async stringPropertySearch(request: PerformanceQueryRequest<PerformanceQuery>) {
        return queryStringProperties(this.client, this.simpleRequest, this.templateSrv, request)
    }

    async query(options: PerformanceQueryRequest<PerformanceQuery>): Promise<DataQueryResponse> {
        const searchType = this.isQueryValidStringPropertySearch(options?.targets);

        if (searchType === 'string') {
            return this.stringPropertySearch(options);
        } else if (searchType === 'invalid') {
            throw new Error('string property queries cannot be mixed with other kinds of queries')
        }

        const maxDataPoints = options.maxDataPoints || 300;
        const intervalMs = options.intervalMs || 60 * 1000;
        const start = options.range.from.valueOf();
        const end = options.range.to.valueOf();
        let step = Math.floor((end - start) / maxDataPoints);
        step = (step < intervalMs) ? intervalMs : step;

        const query = buildPerformanceMeasurementQuery(start, end, step, maxDataPoints)

        let dataFrames: DataFrame[] = []

        for (let i = 0; i < options.targets.length; i++) {
            const target = options.targets[i];

            if (target.performanceType?.value === PerformanceTypeOptions.Attribute.value) {
                if (isValidAttributeTarget(target)) {
                  query.source = this.createSourcesFromAttributeTarget(query, target, options)
                }
            } else if (target.performanceType?.value === PerformanceTypeOptions.Expression.value) {
                if (isValidExpressionTarget(target)) {
                  query.expression = this.createExpressionsFromExpressionTarget(query, target, options, i)
                }
            } else if (target.performanceType?.value === PerformanceTypeOptions.Filter.value) {
                if (isValidFilterTarget(target)) {
                    query.filter = this.createFiltersFromFilterTarget(query, target, options)
                }
            }

            if (isValidMeasurementQuery(query)) {
                const responseData = await this.doMeasuremmentQuery(query)

                if (responseData) {
                    try {
                        // convert to DataFrame format
                        const dataFrame = measurementResponseToDataFrame(responseData, target.refId)
                        dataFrames.push(dataFrame)
                    } catch (e) {
                        console.error(e);
                    }
                }
            }
        }

        return { data: dataFrames } as DataQueryResponse
    }

    async metricFindQuery(query, options) {
        let queryResults: Array<{ text: string, value: string }> = []

        return queryResults
    }

    async testDatasource(): Promise<any> {
        return await this.client.testConnection()
    }

    async doMeasuremmentQuery(query: OnmsMeasurementsQueryRequest) {
        const response = await this.simpleRequest.doOpenNMSRequest({
            url: '/rest/measurements',
            data: query,
            method: 'POST',
            headers: { 'Content-Type': 'application/json' }
        });

        return response && response.data ? response.data as OnmsMeasurementsQueryResponse : null
    }

    async doResourcesRequest(resourceId: string) {
        const response = await this.simpleRequest.doOpenNMSRequest({
            url: '/rest/resources/' + encodeURIComponent(resourceId),
            method: 'GET',
            params: {
                depth: -1
            }
        })

        return response && response.data ? response.data as OnmsResourceDto : null
    }

    async doResourcesForNodeRequest(nodeId: string) {
        const response = await this.simpleRequest.doOpenNMSRequest({
            url: '/rest/resources/fornode/' + encodeURIComponent(nodeId),
            method: 'GET'
        })

        return response && response.data ? response.data as OnmsResourceDto : null
    }
}
