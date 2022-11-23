import { DataQueryResponse, DataSourceApi, DataSourceInstanceSettings } from "@grafana/data";
import { ClientDelegate } from "lib/client_delegate";
import { SimpleOpenNMSRequest } from "lib/utils";
import { PerformanceTypeOptions } from "./constants";
import { measurementResponseToGrafanaSeries } from "./PerformanceHelpers";
import { PerformanceDataSourceOptions, PerformanceQuery, PerformanceQueryRequest } from "./types";

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
            if (target.performanceType.value === PerformanceTypeOptions.Attribute.value) {
                const source = {
                    attribute: target.attribute.attribute.name,
                    ['fallback-attribute']: target.attribute.fallbackAttribute.name,
                    label: target.attribute.label || target.attribute.attribute.name,
                    resourceId: target.attribute.resource.id.replace('node[', 'nodeSource['),
                    transient: false
                }
                query.source.push(source)
            } else if (target.performanceType.value === PerformanceTypeOptions.Expression.value) {
                query.expression.push({
                    label: target.label || 'expression' + i,
                    value: target.expression,
                    transient: target.hide
                })
            }
            const response = await this.simpleRequest.doOpenNMSRequest({
                url: '/rest/measurements',
                data: query,
                method: 'POST',
                headers: { 'Content-Type': 'application/json' }
            });
            try {
                data.push(measurementResponseToGrafanaSeries(response))
            } catch (e) {
                console.error(e);
            }
        }
        return { data }
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
