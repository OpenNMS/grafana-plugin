import { DataQueryResponse, DataSourceApi, DataSourceInstanceSettings, QueryResultMeta, TableData } from "@grafana/data";
import { ClientDelegate } from "lib/client_delegate";
import { SimpleOpenNMSRequest } from "lib/utils";
import { queryEntity } from "./EntityHelper";
import { EntityDataSourceOptions, EntityQuery, EntityQueryRequest } from "./types";

export interface OnmsQueryResultMeta extends QueryResultMeta {
    entity_metadata: any[];
}

export class EntityDataSource extends DataSourceApi<EntityQuery> {
    type: string;
    url?: string | undefined;
    name: string;
    client: ClientDelegate;
    simpleRequest: SimpleOpenNMSRequest;

    constructor(instanceSettings: DataSourceInstanceSettings<EntityDataSourceOptions>, public backendSrv: any, public templateSrv: any) {
        super(instanceSettings);
        this.type = instanceSettings.type;
        this.url = instanceSettings.url;
        this.name = instanceSettings.name;
        this.client = new ClientDelegate(instanceSettings, backendSrv);
        this.simpleRequest = new SimpleOpenNMSRequest(backendSrv, this.url);
    }

    async query(options: EntityQueryRequest<EntityQuery>): Promise<DataQueryResponse> {
        const fullData: TableData[] = [];
        for (let target of options.targets) {
            const filter = target?.filter;
            try {
                const rowData = await queryEntity(target?.selectType?.label, filter, this.client);
                fullData.push(rowData);
            } catch (e) {
                console.error(e);
            }
        }
        return { data: fullData }
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
