
import { DataQuery, DataQueryRequest, DataSourceJsonData, QueryEditorProps, QueryResultMeta, SelectableValue } from "@grafana/data";
import { PerformanceAttributeState } from "./PerformanceAttribute";
import { PerformanceDataSource } from "./PerformanceDataSource";

/**
 * These are options configured for each DataSource instance
 */
export interface PerformanceDataSourceOptions extends DataSourceJsonData {
  path?: string;
}

export interface PerformanceQuery extends DataQuery {
  queryText?: string;
  constant?: number;
  expression?: string;
  label?: string;
  performanceType: QuickSelect;
  attribute: PerformanceAttributeState;
  filter: {name: string};
  filterState: {};
  performanceState: PerformanceStringPropertyState;
}

export interface PerformanceQueryRequest<T extends DataQuery> extends DataQueryRequest<T> {
  queryText: string;
}

export interface StringPropertyQuery extends DataQuery {
    type?: string;
    nodeId?: string;
    resourceId?: string;
    stringProperty?: string;
}

export type DefinedStringPropertyQuery = Required<StringPropertyQuery>

export interface OnmsMeasurementsQuerySource {
  label: string;
  resourceId: string;
  attribute: string;
  ['fallback-attribute']: string;
  aggregation: string;  // should be 'AVERAGE', 'MIN', 'MAX' or 'LAST'
  transient: boolean;
  nodeId?: string; // this may be added dynamically
}

export interface OnmsMeasurementsQueryExpression {
  label: string;
  value: string;  // this is the jexl or similar expression
  transient: boolean;
}

export interface OnmsMeasurementsQueryFilterParam {
    key: string;
    value: string | { value: string }
}

export interface OnmsMeasurementsQueryFilter {
  name: string;
  parameter: OnmsMeasurementsQueryFilterParam[]
}

// See features/measurements/api, package org.opennms.netmgt.measurements.model.QueryRequest
export interface OnmsMeasurementsQueryRequest {
  start: number;
  end: number;
  step: number;
  relaxed: boolean; // enable relaxed mode, which allows for missing attributes
  maxrows: number;
  source: OnmsMeasurementsQuerySource[];
  expression: OnmsMeasurementsQueryExpression[];
  filter: OnmsMeasurementsQueryFilter[];
}

export interface OnmsMeasurementsQueryNode {
  id: number;
  ["foreign-source"]: string;
  ["foreign-id"]: string;
  label: string;
}

export interface OnmsMeasurementsQueryResource {
  id: string;
  ["parent-id"]: string;
  label: string;
  name: string;
  ["node-id"]: number;
  node: OnmsMeasurementsQueryNode;
}

export interface OnmsMeasurementsQueryMetadata {
  resources: Array<{ resource: OnmsMeasurementsQueryResource }>;
  nodes: Array<{ node: OnmsMeasurementsQueryNode }>;
}

// See features/measurements/api, package org.opennms.netmgt.measurements.model.QueryResponse
export interface OnmsMeasurementsQueryResponse {
  step: number;
  start: number;
  end: number;
  timestamps: number[];
  labels: string[];
  columns: Array<{ values: Array<string | number | null> }>;  // TODO: Is this returned as string or number in Json???
  constants: Array<{ key: string, value: string }>;
  metadata: OnmsMeasurementsQueryMetadata;
}

export interface QuickSelect {
 label?: string, 
 value?: number 
}

export type PerformanceQueryEditorProps = QueryEditorProps<PerformanceDataSource, PerformanceQuery, PerformanceDataSourceOptions>;

export interface OnmsQueryResultMeta extends QueryResultMeta {
    entity_metadata: any[];
}

export interface PerformanceStringPropertyProps {
    updateQuery: Function;
    loadNodes: (query?: string | undefined) => Promise<Array<SelectableValue<{ id: string }>>>;
    loadResourcesByNodeId: Function;
}

export interface PerformanceStringPropertyState {
    node: { id: string };
    resource: { id: string, stringPropertyAttributes: Record<string, string> };
    stringProperty: { label: string, value: string };
}
