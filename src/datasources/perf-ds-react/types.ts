
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

export interface QuickSelect {
 label?: string, 
 value?: number 
}

export type PerformanceQueryEditorProps = QueryEditorProps<PerformanceDataSource, PerformanceQuery, PerformanceDataSourceOptions>;

export interface OnmsQueryResultMeta extends QueryResultMeta {
    entity_metadata: any[];
}

export interface SeriesResponse {
    target: string,
    label: string,
    datapoints: [[string, string]]
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
