import {
    DataQueryRequest,
    DataQueryResponse,
    DataQueryResponseData,
    FieldType
} from "@grafana/data";
import { TemplateSrv } from "@grafana/runtime"
import { Client, ServerMetadata } from "opennms"
import { ClientDelegate } from "../../../lib/client_delegate"
import { getResourceId, SimpleOpenNMSRequest, trimChar } from "../../../lib/utils"
import { OnmsResourceSelectQuery, OnmsResourceDto} from '../../../lib/api_types'
import {
    DefinedStringPropertyQuery,
    PerformanceQuery,
    
} from "./../types";



// constructs a single string valued data frame field
const toStringField = (name: string, value: string) => {
    return {
        name,
        type: FieldType.string,
        config: {},
        values: [value]
    }
}

// constructs all data frame fields for a string property
const toStringFields = (node: OnmsResourceDto, resource: OnmsResourceDto, key: string, value: string) => {
    return [
        toStringField('nodeId', node.name || node.id),
        toStringField('nodeLabel', node.label),
        toStringField('resourceId', resource.name || resource.id),
        toStringField('resourceLabel', resource.label),
        toStringField(key, value)
    ]
}

const isDefinedStringPropertyQuery = (q: PerformanceQuery | undefined) => {
    const ps = q?.stringPropertyState

    return ps && (ps.node.id || ps.node.label) && (ps.resource.id || ps.resource.label) && ps.stringProperty.value ? true : false
}

export const getDefinedStringPropertyQueries = (templateSrv: TemplateSrv, request: DataQueryRequest<PerformanceQuery>) => {
    const definedQueries: DefinedStringPropertyQuery[] = request.targets
        .filter(q => !q.hide)
        .filter(isDefinedStringPropertyQuery)
        .map(q => {
            const nodeId = q.stringPropertyState.node.id || q.stringPropertyState.node.label
            const resourceId = q.stringPropertyState.resource.id || q.stringPropertyState.resource.label
            return {
                //...q,
                // DataQuery fields
                refId: q.refId,
                hide: q.hide,
                key: q.key,
                queryType: q.queryType,
                datasource: q.datasource,

                // StringPropertyQuery fields
                nodeId: trimChar(templateSrv.replace(nodeId, request.scopedVars), '{', '}'),
                resourceId: trimChar(templateSrv.replace(getResourceId(resourceId), request.scopedVars), '{', '}'),
                stringProperty: q.stringPropertyState.stringProperty.value
            } as DefinedStringPropertyQuery
        })

    return definedQueries
}

const queryAllStringProperties = async (simpleRequest: SimpleOpenNMSRequest,
    selection: OnmsResourceSelectQuery): Promise<DataQueryResponse> => {

    const response = await simpleRequest.doOpenNMSRequest({
        url: '/rest/resources/select',
        method: 'GET',
        params: {
            nodes: Array.from(selection.nodes).join(','),
            nodeSubresources: Array.from(selection.nodeSubresources).join(','),
            stringProperties: Array.from(selection.stringProperties).join(',')
        }
    })

    const responseData = response.data as OnmsResourceDto[];

    const fieldSets =
        responseData.flatMap(node =>
            node.children.resource.flatMap(resource =>
                Object.entries<string>(resource.stringPropertyAttributes).flatMap(([key, value]) => {
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
            } 
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
        definedQueries.reduce<OnmsResourceSelectQuery>(
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
    resource: OnmsResourceDto,
    node?: OnmsResourceDto): any[] /*DataQueryResponseData[]*/ => {

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
        } 
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

    const nodeDto = response.data as OnmsResourceDto;

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

    const definedQueries = getDefinedStringPropertyQueries(templateSrv, request)

    const client: Client = await clientDelegate.getClientWithMetadata()
    const metadata: ServerMetadata = client.http.server.metadata;

    let result: DataQueryResponse
    if (metadata.selectPartialResources()) {
        result = await queryStringPropertiesForAllNodesInBulk(simpleRequest, definedQueries)
    } else {
        result = await queryStringPropertiesForEachNodeSeparately(simpleRequest, definedQueries)
    }
    return result
}
