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

export interface OnmsResourceSelectQuery {
    nodes: Set<string>;
    nodeSubresources: Set<string>;
    stringProperties: Set<string>;
}

// See org.opennms.netmgt.model.resource.ResourceDTO
// Response from /rest/resources/fornode and other queries
export interface OnmsRrdGraphAttribute {
    name: string;
    relativePath: string;
    rrdFile: string;
  }
  
  export interface OnmsResourceCollection {
    resource: OnmsResourceDto[];
  }
  
  export interface OnmsResourceDto {
    id: string;
    label: string;
    name: string;
    link?: string;
    typeLabel?: string;
    parentId?: string;
    children: OnmsResourceCollection;
    stringPropertyAttributes: { [key: string]: string };
    externalValueAttributes: { [key: string]: string };
    rrdGraphAttributes: { [key: string]: OnmsRrdGraphAttribute };
  }
