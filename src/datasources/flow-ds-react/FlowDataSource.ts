import { DataQueryResponse, DataSourceApi, DataSourceInstanceSettings } from '@grafana/data';
import { TemplateSrv, getTemplateSrv, getBackendSrv } from '@grafana/runtime';
import { ClientDelegate } from 'lib/client_delegate';
import { SimpleOpenNMSRequest } from 'lib/utils';
import { FlowStrings } from './constants';
import { GrafanaError } from 'opennms'
import { isString} from  'lodash'

import {
    buildFullQueryData,
    checkForTableSummary,
    extractDataFromQuery,
    queryOpenNMS,
    queryTemplateVariable
} from './helpers';
import {
    FlowDataSourceOptions,
    FlowQuery,
    FlowQueryRequest
} from './types';

export class FlowDataSource extends DataSourceApi<FlowQuery> {
    type: string;
    url?: string | undefined;
    name: string;
    client: ClientDelegate;
    simpleRequest: SimpleOpenNMSRequest;
    templateSrv: TemplateSrv

    constructor(instanceSettings: DataSourceInstanceSettings<FlowDataSourceOptions>) {
        super(instanceSettings);
        this.type = instanceSettings.type;
        this.url = instanceSettings.url;
        this.name = instanceSettings.name;
        this.client = new ClientDelegate(instanceSettings, getBackendSrv());
        this.simpleRequest = new SimpleOpenNMSRequest(getBackendSrv(), this.url);
        this.templateSrv = getTemplateSrv();
    }

    async query(options: FlowQueryRequest<FlowQuery>): Promise<DataQueryResponse> {
        const partialQueryData = extractDataFromQuery(options.targets);
        const fullQueryData = buildFullQueryData(partialQueryData, this.templateSrv);
        const { allAreSummaries } = checkForTableSummary(fullQueryData)
        const type = allAreSummaries ? FlowStrings.summaries : FlowStrings.series;
        return await queryOpenNMS(fullQueryData, options, type, { client: this.client, simpleRequest: this.simpleRequest} );
    }

    async testDatasource(): Promise<any> {
        const defaultErrorMessage = 'Cannot connect to API';
        let response = { status: '', message: '' }
        try {
            await this.client.getClientWithMetadata();
            response = { status: FlowStrings.SuccessStatus, message: FlowStrings.Success }
        } catch (err) {
            let message = '';
            if (isString(err)) {
                message = err;
            } else {
                let grafanaError = err as GrafanaError
                if (grafanaError) {
                    message = 'Fetch error: ' + (grafanaError.data.statusText ? grafanaError.data.statusText : defaultErrorMessage);
                    if (grafanaError.data && grafanaError.data?.error && grafanaError.data?.message) {
                        message += ': ' + grafanaError.data.error + '. ' + grafanaError.data.message;
                    }
                }
            }
            response = { status: "error", message: message }
            console.log('CAUGHT!', err);
        }
        return response
    }

    async metricFindQuery(query) {
        if (query === null || query === undefined || query === "") {
            return Promise.resolve([]);
        }
        let response = await queryTemplateVariable(query, this.templateSrv, this.client, this.simpleRequest);
        return response;
    }

}
