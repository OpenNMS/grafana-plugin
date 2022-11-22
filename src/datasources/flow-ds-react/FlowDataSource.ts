import { DataQueryResponse, DataSourceApi, DataSourceInstanceSettings } from '@grafana/data';
import { ClientDelegate } from 'lib/client_delegate';
import { SimpleOpenNMSRequest } from 'lib/utils';
import { FlowStrings } from './constants';
import {
    buildFullQueryData,
    checkForTableSummary,
    extractDataFromQuery,
    queryOpenNMS,
    metricFindHosts,
    metricFindDscpOnExporterNodeAndInterface,
    metricFindApplications,
    metricFindConversations, 
    metricFindExporterNodes, 
    metricFindInterfacesOnExporterNode, 
    metricFindLocations, 
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

    constructor(instanceSettings: DataSourceInstanceSettings<FlowDataSourceOptions>, public backendSrv: any, public templateSrv: any) {
        super(instanceSettings);
        this.type = instanceSettings.type;
        this.url = instanceSettings.url;
        this.name = instanceSettings.name;
        this.client = new ClientDelegate(instanceSettings, backendSrv);
        this.simpleRequest = new SimpleOpenNMSRequest(backendSrv, this.url);
    }

    async query(options: FlowQueryRequest<FlowQuery>): Promise<DataQueryResponse> {
        const partialQueryData = extractDataFromQuery(options.targets)
        const fullQueryData = buildFullQueryData(partialQueryData);
        const { allAreSummaries } = checkForTableSummary(fullQueryData)
        const type = allAreSummaries ? FlowStrings.summaries : FlowStrings.series;
        return queryOpenNMS(fullQueryData, options, type, this.client);
    }

    async testDatasource(): Promise<any> {
        let response = { status: '', message: '' }
        try {
            await this.client.getClientWithMetadata();
            response = { status: FlowStrings.SuccessStatus, message: FlowStrings.Success }
        } catch (e) {
            response = { status: FlowStrings.FailureStatus, message: e as string }
            console.error(e);
        }
        return response
    }

    // async metricFindQuery(query) {
    //     if (query === null || query === undefined || query === "") {
    //         return Promise.resolve([]);
    //     }
    //     query = this.templateSrv.replace(query);

    //     let applications = /applications\((.*)\)/;
    //     let conversations = /conversations\((.*)\)/;
    //     let hosts = /hosts\((.*)\)/;
    //     let locations = /locations\((.*)\)/;
    //     let exporterNodesRegex = /exporterNodesWithFlows\((.*)\)/;
    //     let interfacesOnExporterNodeRegex = /interfacesOnExporterNodeWithFlows\(\s*([^,]+).*\)/; // just pick the first arg and ignore anything else
    //     let dscpOnExporterNodeAndInterfaceRegex = /dscpOnExporterNodeAndInterface\(\s*([^,]+),\s*([^,]+),\s*([^,]+),\s*([^\s]+\s*)\)/;

    //     const start = this.templateSrv.timeRange.from.valueOf();
    //     const end = this.templateSrv.timeRange.to.valueOf();

    //     let locationsQuery = query.match(locations);
    //     if (locationsQuery) {
    //         return await this.metricFindLocations();
    //     }

    //     let applicationsQuery = query.match(applications);
    //     if (applicationsQuery) {
    //         let limit = applicationsQuery.length > 1 && !isNaN(parseInt(applicationsQuery[1].trim(), 10)) ? parseInt(applicationsQuery[1].trim(), 10) : 0;
    //         return await this.metricFindApplications(start, end, limit);
    //     }

    //     let conversationsQuery = query.match(conversations);
    //     if (conversationsQuery) {
    //         let args = conversationsQuery.length > 1 ? conversationsQuery[1].split(',').map(v => v.trim()) : null;
    //         if (args) {
    //             //first argument should be application, then location, then protocol, and last limit which should be a number
    //             //when multiple args are passed assume last is limit
    //             if (args.length === 1 && !isNaN(parseInt(args[0], 10))) {
    //                 return await metricFindConversations(start, end, null, null, null, parseInt(args[0], 10));
    //             } else if (args.length === 1) {
    //                 return await metricFindConversations(start, end, args[0]);
    //             } else if (args.length === 2 && _.every(args, s => isNaN(parseInt(s, 10)))) {
    //                 return await metricFindConversations(start, end, args[0], args[1]);
    //             } else if (args.length === 2 && !_.every(args, s => isNaN(parseInt(s, 10)))) {
    //                 return await metricFindConversations(start, end, args[0], null, null, parseInt(args[1], 10));
    //             } else if (args.length === 3 && _.every(args, s => isNaN(parseInt(s, 10)))) {
    //                 return await metricFindConversations(start, end, args[0], args[1], args[2]);
    //             } else if (args.length === 3 && !_.every(args, s => isNaN(parseInt(s, 10)))) {
    //                 return await metricFindConversations(start, end, args[0], args[1], null, parseInt(args[2], 10));
    //             } else if (args.length === 4 && !_.every(args, s => isNaN(parseInt(s, 10)))) {
    //                 return await metricFindConversations(start, end, args[0], args[1], args[2], parseInt(args[3], 10));
    //             }
    //         }
    //         return await metricFindConversations(start, end);
    //     }

    //     let hostsQuery = query.match(hosts);
    //     if (hostsQuery) {
    //         let args = hostsQuery.length > 1 ? hostsQuery[1].split(',').map(v => v.trim()) : null;
    //         if (args) {
    //             if (args.length === 1 && !isNaN(parseInt(args[0], 10))) {
    //                 return await metricFindHosts(start, end, null, parseInt(args[0], 10));
    //             } else if (args.length === 1) {
    //                 return await metricFindHosts(start, end, args[0]);
    //             } else if (args.length === 2 && !isNaN(parseInt(args[1], 10))) {
    //                 return await metricFindHosts(start, end, args[0], parseInt(args[1], 10));
    //             }
    //         }
    //         return await metricFindHosts(start, end);


    //     }

    //     let exporterNodesQuery = query.match(exporterNodesRegex);
    //     if (exporterNodesQuery) {
    //         let exporterNodesQueryFilter = exporterNodesQuery.length > 1 ? exporterNodesQuery[1] : null;
    //         return await metricFindExporterNodes(query, exporterNodesQueryFilter);
    //     }

    //     let interfacesOnExporterNodeQuery = query.match(interfacesOnExporterNodeRegex);
    //     if (interfacesOnExporterNodeQuery) {
    //         return await metricFindInterfacesOnExporterNode(interfacesOnExporterNodeQuery[1]);
    //     }

    //     let dscpOnExporterNodeAndInterfaceQuery = query.match(dscpOnExporterNodeAndInterfaceRegex);
    //     if (dscpOnExporterNodeAndInterfaceQuery) {
    //         return await metricFindDscpOnExporterNodeAndInterface(
    //             dscpOnExporterNodeAndInterfaceQuery[1], // node
    //             dscpOnExporterNodeAndInterfaceQuery[2], // interface
    //             dscpOnExporterNodeAndInterfaceQuery[3], // start millis
    //             dscpOnExporterNodeAndInterfaceQuery[4], // end millis
    //         );
    //     }

    //     return await Promise.resolve([]);
    // }

}
