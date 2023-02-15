import { API } from 'opennms'
import {
  Column,
  DataQuery,
  DataQueryRequest,
  DataSourceJsonData,
  SelectableValue,
  QueryEditorProps,
  TableData
} from '@grafana/data';
import { EntityDataSource } from './EntityDataSource';
import { GrafanaDatasource } from '../../hooks/useDataSources'

/**
 * These are options configured for each DataSource instance
 */
export interface EntityDataSourceOptions extends DataSourceJsonData {
  path?: string;
}

export interface EntityQuery extends DataQuery {
  queryText?: string;
  constant?: number;
  selectType: SelectableValue;
  filter: API.Filter;
  clauses: any;
}

export interface EntityQueryRequest<T extends DataQuery> extends DataQueryRequest<T> {
  queryText: string
  enforceTimeRange?: boolean
  entityType?: string
}

export interface Comparator {
  i: number,
  l: string,
  aliases: string[]
}

export interface SearchType {
  i: string,
  l: string,
  comparators: Comparator[]
}

export interface SearchOption {
  id: string,
  name: string,
  orderBy: boolean,
  label: string,
  type: SearchType,
  values: string[],
  value?: { values: string[], type: SearchType, id: string, orderBy: boolean, name: string, label: string };
}

export type Properties = AlarmProperties | NodeProperties | {};

export interface AlarmProperties {
  location: string;
  service: string;
  category: string;
  ipAddress: string;
  lastEventSeverity: string,
  severity: string,
  troubleTicketState: string
}

export interface NodeProperties {
  category: string;
  ifIndex: number;
  ipAddress: string;
  ipHostname: string;
  location: string;
  parentId: number;
}

export interface IpInterfaceProperties {
  ipAddress: string;
  hostname: string;
  snmpPrimary: string;
}
export interface SNMPInterfaceProperties {
  ifDescr: string;
  ifName: string;
  ifSpeed: string;
  ifAlias: string;
}
export interface MonitoredServiceProperties {
  ipAddress: string;
  type: string;
  statusId: string;
}
export interface OutagesProperties {
  foreignSource: string;
  nodeLabel: string;
  ipAddress: string;
  monitoredServiceTypeName: string;
  ifLostService: string;
  ifRegainedService: string;
  perspective: string;
}

export enum OnmsEntityType {
  AND,
  OR,
  FIRST
}

export enum OnmsEntityNestType {
  TOP,
  NESTED,
  SUB
}

export interface OnmsEntityClause {
  attribute: SearchOption;
  comparator: { l: string, i: number, aliases: string[] };
  comparedValue: string;
  comparedString: string | number;
  type: OnmsEntityType;
  nestingType: OnmsEntityNestType;
}

export interface EntityClauseProps {
  propertiesAsArray: SearchOption[]
  index: number
  clause: OnmsEntityClause,
  addClause: (col: number) => void,
  addNestedClause: (col: number) => void,
  addSubClause: (col: number) => void,
  removeClause: (col: number) => void,
  setAttribute: (col: number, attribute: SelectableValue<{ values: string[] | undefined, type: SearchType | undefined }>) => void
  setComparator: (col: number, comparator: SelectableValue<Comparator>) => void
  setComparedValue: (col: number, value: SelectableValue<string>) => void
  setComparedString: (col: number, value: string | number | Date) => void
  setClauseType: (col: number, value: number) => void
  loading: boolean
}

export interface EntityClauseLabelProps {
  type: OnmsEntityType,
  nestingType: OnmsEntityNestType,
  index: number,
  setClauseType: Function
}

export interface OnmsColumn extends Column {
  resource?: string,
  featured?: boolean,
  visible?: boolean
}

export interface OnmsTableData extends TableData {
  // override
  columns: OnmsColumn[]
}

export type EntityQueryEditorProps = QueryEditorProps<EntityDataSource, EntityQuery, EntityDataSourceOptions>;

/**
 * Used by FilterPanel and EntityDataSource
 * entity: Entity name for filter (e.g. "nodes", "alarms")
 * attribute: Entity attribute to filter on (e.g. "id", "label", "Alarm Type")
 * selectionType: whether this filter is single/multi/text
 * altColumnLabel: User customized label for this filter, to display on FilterPanelControl component
 */
export interface ActiveFilter {
    entity: SelectableValue<string | number>
    attribute: SelectableValue<{ id: string | number }>
    selectionType: SelectableValue<string>
    altColumnLabel: string
}

// Used by FilterPanel and EntityDataSource
export interface FilterSelectableValues {
    filterId: string,
    values: Array<SelectableValue<string | number>>
}

/**
 * FilterPanel data saved to localStorage for use by Entity Datasource.
 */
export interface FilterEditorData {
    datasource: SelectableValue<GrafanaDatasource> | undefined,
    activeFilters: ActiveFilter[]
    selectableValues: FilterSelectableValues[]
}

export type OnmsRow = (number | string | moment.Moment | boolean | undefined)[]
