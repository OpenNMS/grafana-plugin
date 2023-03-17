import {
    ArrayVector,
    DataFrame,
    DataQueryRequest,
    DataQueryResponse,
    DataQueryResponseData,
    Field,
    FieldType,
    Vector
} from "@grafana/data";
import { TemplateSrv } from "@grafana/runtime"
import { Client, ServerMetadata } from "opennms"
import { ClientDelegate } from "../../../lib/client_delegate"
import { SimpleOpenNMSRequest } from "../../../lib/utils"
import {
    DefinedStringPropertyQuery,
    PerformanceQuery
} from "./../types";

interface RestResourceSelectQuery {
    nodes: Set<string>;
    nodeSubresources: Set<string>;
    stringProperties: Set<string>;
}

interface RestResourceDTOCollection {
    resource: RestResourceDTO[];
}

interface RestResourceDTOBase {
    id: string;
    label: string;
}

interface RestResourceDTO extends RestResourceDTOBase {
    name: string;
    children: RestResourceDTOCollection;
    stringPropertyAttributes: Map<string,string>;
}

// constructs a single string valued data frame field
const toStringField = (name: string, value: string): Field<string, Vector<string>> => {
    return {
        name,
        type: FieldType.string,
        config: {},
        values: new ArrayVector<string>([value])
    }
}

// constructs all data frame fields for a string property
const toStringFields = (node: RestResourceDTOBase, resource: RestResourceDTOBase, key: string, value: string) => {
    return [
        toStringField('nodeId', node.id),
        toStringField('nodeLabel', node.label),
        toStringField('resourceId', resource.id),
        toStringField('resourceLabel', resource.label),
        toStringField(key, value)
    ]
}

const isDefinedStringPropertyQuery = (q: PerformanceQuery | undefined) => {
    const ps = q?.performanceState

    return ps && ps.node.id && ps.resource.id && ps.stringProperty.value ? true : false
}

export const getDefinedStringPropertyQueries = (templateSrv: TemplateSrv, targets: PerformanceQuery[]) => {
    const definedQueries: DefinedStringPropertyQuery[] = targets
        .filter(q => !q.hide)
        .filter(isDefinedStringPropertyQuery)
        .map(q => {
            return {
                //...q,
                // DataQuery fields
                refId: q.refId,
                hide: q.hide,
                key: q.key,
                queryType: q.queryType,
                datasource: q.datasource,

                // StringPropertyQuery fields
                nodeId: templateSrv.replace('' + q.performanceState.node.id),
                resourceId: templateSrv.replace(q.performanceState.resource.id),
                stringProperty: q.performanceState.stringProperty.value
            } as DefinedStringPropertyQuery
        })

    return definedQueries
}

const queryAllStringProperties = async (simpleRequest: SimpleOpenNMSRequest,
    selection: RestResourceSelectQuery): Promise<DataQueryResponse> => {

    const response = await simpleRequest.doOpenNMSRequest({
        url: '/rest/resources/select',
        method: 'GET',
        params: {
            nodes: Array.from(selection.nodes).join(','),
            nodeSubresources: Array.from(selection.nodeSubresources).join(','),
            stringProperties: Array.from(selection.stringProperties).join(',')
        }
    })

    const responseData = response.data as RestResourceDTO[];

    const fieldSets =
        responseData.flatMap(node =>
            node.children.resource.flatMap(resource =>
                //Object.entries<string>(resource.stringPropertyAttributes).flatMap(([key, value]) => {
                Array.from(resource.stringPropertyAttributes.entries()).flatMap(([key, value]) => {
                    return {
                        fields: toStringFields(node, resource, key, value)
                    }
                })
            )
        )

    const dataFrames =
        fieldSets.map(f => {
            return {
                fields: f.fields,
                length: f.fields.length
            } as DataFrame
        })

    return {
        data: dataFrames
    } as DataQueryResponse
}

const queryStringPropertiesForAllNodesInBulk = async (
    simpleRequest: SimpleOpenNMSRequest,
    definedQueries: DefinedStringPropertyQuery[]): Promise<DataQueryResponse> => {

    // send a single request that selects all nodes, subresources, and string properties
    const selection =
        definedQueries.reduce<RestResourceSelectQuery>(
            (accu, query) => {
                accu.nodes.add(query.nodeId)
                accu.nodeSubresources.add(query.resourceId)
                accu.stringProperties.add(query.stringProperty)
                return accu
            },
        { nodes: new Set(), nodeSubresources: new Set(), stringProperties: new Set() })

    const response: DataQueryResponse = await queryAllStringProperties(simpleRequest, selection)

    return response
}

const extractStringProperties = (
    query: DefinedStringPropertyQuery,
    resource: RestResourceDTO,
    node?: RestResourceDTO): DataFrame[] /*DataQueryResponseData[]*/ => {

    const matchingKeys =
        Object.keys(resource.stringPropertyAttributes)
        .filter(key => key === query.stringProperty)

    const dataFrames = matchingKeys.map(key => {
        return {
            refId: query.refId,
            fields: [
                toStringField('nodeId', query.nodeId),
                toStringField('nodeLabel', node ? node.label : query.nodeId),
                toStringField('resourceId', query.resourceId),
                toStringField('resourceLabel', resource.label),
                toStringField(key, resource.stringPropertyAttributes[key])
            ],
            length: 5
        } as DataFrame
    })

    return dataFrames
}

const queryStringPropertiesOfNode = async (
    simpleRequest: SimpleOpenNMSRequest,
    nodeId: string,
    queries: DefinedStringPropertyQuery[]): Promise<DataQueryResponseData[]> => {

    const response = await simpleRequest.doOpenNMSRequest({
        url: '/rest/resources/fornode/' + encodeURIComponent(nodeId),
        method: 'GET'
    })

    const nodeDto = response.data as RestResourceDTO;

    const data =
        queries.flatMap(query => {
            return nodeDto.children.resource
                .filter(resource => resource.id.endsWith(`.${query.resourceId}`))
                .flatMap(resource => extractStringProperties(query, resource, nodeDto))
        })

    return data
}

const queryStringPropertiesForEachNodeSeparately = async (
    simpleRequest: SimpleOpenNMSRequest,
    definedQueries: DefinedStringPropertyQuery[]): Promise<DataQueryResponse> => {

    const groupedByNodeId = definedQueries.reduce<{ [key: string]: DefinedStringPropertyQuery[] }>(
        (accu, query) => {
            if (!accu[query.nodeId]) {
              accu[query.nodeId] = []
            }

            accu[query.nodeId].push(query)
            return accu
    }, {})

    // send a request for each node separately...
    const datas: Array<Promise<DataQueryResponseData[]>> =
        Object.keys(groupedByNodeId).map((nodeId) => {
            const queries = groupedByNodeId[nodeId]
            return queryStringPropertiesOfNode(simpleRequest, nodeId, queries)
        })

    // ... and then combine all results into a single DataQueryResponse
    return Promise.all(datas).then(datas => {
        return {
            data: datas.flatMap(x => x)
        }
    })
}

export const queryStringProperties = async (
    clientDelegate: ClientDelegate,
    simpleRequest: SimpleOpenNMSRequest,
    templateSrv: TemplateSrv,
    request: DataQueryRequest<PerformanceQuery>): Promise<DataQueryResponse> => {

    const definedQueries = getDefinedStringPropertyQueries(templateSrv, request.targets)

    const client: Client = await clientDelegate.getClientWithMetadata()
    const metadata: ServerMetadata = client.http.server.metadata;

    if (metadata.selectPartialResources()) {
        return queryStringPropertiesForAllNodesInBulk(simpleRequest, definedQueries)
    } else {
        return queryStringPropertiesForEachNodeSeparately(simpleRequest, definedQueries)
    }
}
