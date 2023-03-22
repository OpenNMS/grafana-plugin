/**
 * Api namespace to add types for different onms rest api's models
 */
export interface OnmsMeasurementResource {
    id: string,
    link?: string,
    name?: string,
    label?: string
}

export interface OnmsLocationResponse {
    data: {
        count: number, 
        totalCount: number, 
        location?: any[]
    }
}

export interface OnmsFlowsResponse {
    data: any[]
}
