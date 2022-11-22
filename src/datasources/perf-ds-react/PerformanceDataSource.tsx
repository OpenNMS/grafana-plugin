import { DataQueryResponse, DataSourceApi, DataSourceInstanceSettings, QueryResultMeta } from "@grafana/data";
import { ClientDelegate } from "lib/client_delegate";
import { SimpleOpenNMSRequest } from "lib/utils";
import { PerformanceDataSourceOptions, PerformanceQuery, PerformanceQueryRequest } from "./types";

export interface OnmsQueryResultMeta extends QueryResultMeta {
    entity_metadata: any[];
}

export class PerformanceDataSource extends DataSourceApi<PerformanceQuery> {
    type: string;
    url?: string | undefined;
    name: string;
    client: ClientDelegate;
    simpleRequest: SimpleOpenNMSRequest;

    constructor(instanceSettings: DataSourceInstanceSettings<PerformanceDataSourceOptions>, public backendSrv: any, public templateSrv: any) {
        super(instanceSettings);
        this.type = instanceSettings.type;
        this.url = instanceSettings.url;
        this.name = instanceSettings.name;
        this.client = new ClientDelegate(instanceSettings, backendSrv);
        this.simpleRequest = new SimpleOpenNMSRequest(backendSrv, this.url);
    }

    async query(options: PerformanceQueryRequest<PerformanceQuery>): Promise<DataQueryResponse> {
        const data: Array<{ target: string; label: string; datapoints: [[string, string]]; }> = []
        const maxDataPoints = options.maxDataPoints || 300;
        const intervalMs = options.intervalMs || 60 * 1000;

        const start = options.range.from.valueOf();
        const end = options.range.to.valueOf();
        let step = Math.floor((end - start) / maxDataPoints);
        step = (step < intervalMs) ? intervalMs : step;

        var query = {
            start: start,
            end: end,
            step: step,
            relaxed: true, // enable relaxed mode, which allows for missing attributes
            maxrows: maxDataPoints,
            source: [] as any[],
            expression: [] as any[],
        };

        for (let i = 0; i < options.targets.length; i++) {
            const target = options.targets[i];
            const newQuery = { ...query };
            newQuery.source = []
            console.log('HERE IS MY NEW QUERY!',newQuery);
            const source = {
                attribute: target.attribute.attribute.name,
                ['fallback-attribute']: target.attribute.fallbackAttribute.name,
                label: target.attribute.label || target.attribute.attribute.name,
                resourceId: target.attribute.resource.id.replace('node[', 'nodeSource['),
                transient: false
            }
            newQuery.source.push(source)
            const response = await this.simpleRequest.doOpenNMSRequest({
                url: '/rest/measurements',
                data: newQuery,
                method: 'POST',
                headers: { 'Content-Type': 'application/json' }
            });
            try {

                data.push(this.processMeasurementsResponse(response))
            } catch (e) {
                console.error(e);
            }
        }
        console.log('HERE IS MY DATA!',{data})
        return { data }
    }

    processMeasurementsResponse(response) {
        const labels = response.data.labels;
        const columns = response.data.columns;
        const timestamps = response.data.timestamps;
        const metadata = response.data.metadata;
        let series: { target: string, label: string, datapoints: [[string, string]] } = { target: '', label: '', datapoints: [['', '']] };
        let datapoints: [[string, string]] = [['', '']];
        var value, atLeastOneNonNaNValue;

        if (timestamps !== undefined) {
            const nRows = timestamps.length;
            const nCols = columns.length;

            for (let i = 0; i < nCols; i++) {
                atLeastOneNonNaNValue = false;
                datapoints = [['', '']];
                for (let j = 0; j < nRows; j++) {
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
                    //    label = FunctionFormatter.format(label, metadata);
                }

                // Skip series that are all NaNs
                // When querying in relaxed mode, expressions that operate against attribute that are missing may only contain
                // NaNs. In this case, we don't want to show them at all.
                if (atLeastOneNonNaNValue) {
                    series = {
                        target: label,
                        label: labels[i],
                        datapoints: datapoints
                    }
                }
            }
        }
        return series;
    }
    async testDatasource(): Promise<any> {
        console.log('Testing the data source!');
        try {

            const metadata = await this.client.getClientWithMetadata();
            console.log('Testing the data source1!', metadata);
        } catch (e) {
            console.log('CAUGHT!', e);
        }
        return { status: 'success', message: 'Success' }
    }
}
