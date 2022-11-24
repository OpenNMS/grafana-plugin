import { DataQueryResponse, DataSourceApi, DataSourceInstanceSettings, QueryResultMeta, TableData } from "@grafana/data";
import { getTemplateSrv } from '@grafana/runtime';
import { ClientDelegate } from "lib/client_delegate";
import { SimpleOpenNMSRequest } from "lib/utils";
import { API } from "opennms";
import { queryEntity, queryProperties } from "./EntityHelper";
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
    addVariablesToFilter(filter: API.Filter) {
        const myVariables = getTemplateSrv().getVariables();
        const nodeVariable: any = myVariables.find((d: any) => d.query.startsWith('nodes('));
        let localFilter = new API.Filter();
        filter.clauses?.forEach((existingFilter) => {
            if (existingFilter.operator.label === 'AND'){
                localFilter.withAndRestriction(new API.Restriction(existingFilter.restriction.attribute,existingFilter.restriction.comparator,))
            }
        })
        if (nodeVariable) {
            localFilter.withAndRestriction(new API.Restriction(nodeVariable.name, API.Comparators.EQ, nodeVariable.current.value))
        }
        myVariables.filter((d: any) => !d.query.startsWith('node')).forEach((key: any,value) => {
            if (key.query.startsWith('locations')){
                if (key.current.text){
                    localFilter.withAndRestriction(new API.Restriction('location',API.Comparators.EQ,key.current.value))
                }
            }
        })
        return localFilter;
    }

    async query(options: EntityQueryRequest<EntityQuery>): Promise<DataQueryResponse> {
        const fullData: TableData[] = [];
        for (let target of options.targets) {
            let filter = target?.filter;
            filter = this.addVariablesToFilter(filter);
            try {
                const rowData = await queryEntity(target?.selectType?.label, filter, this.client);
                fullData.push(rowData);
            } catch (e) {
                console.error(e);
            }
        }
        return { data: fullData }
    }

    async metricFindQuery(query, options) {
        let queryResults: Array<{ text: string, value: string }> = []
        if (query) {
            let entityQueries = [['alarms', 'Alarms'], ['nodeFilter', 'Nodes']]
            let foundQuery = entityQueries.find((d) => query.startsWith(d[0]))
            if (foundQuery) {
                let entityType = foundQuery[1]
                let filter = new API.Filter();
                filter = this.addVariablesToFilter(filter);
                let results = await queryEntity(entityType, filter, this.client);
                queryResults = results.rows.map((row) => {
                    return { text: row[1], value: row[0] }
                })
            } else if (query.startsWith('nodes')) {
                queryResults = await queryProperties(query, this.client)
            }
        }
        return queryResults
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
