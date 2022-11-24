import { DataQuery, DataQueryRequest, DataSourceJsonData, SelectableValue } from "@grafana/data";
import { ClientDelegate } from "lib/client_delegate";
import { SimpleOpenNMSRequest } from "lib/utils";

//Workaround for this not being available in all browsers.
//We check for it's use and fall back to a random number
//if it's not present.
declare global {
  interface Crypto {
    randomUUID: () => string;
  }
}

/**
 * These are options configured for each DataSource instance
 */
export interface FlowDataSourceOptions extends DataSourceJsonData {
  path?: string;
}

export interface FlowQuery extends DataQuery {
  queryText?: string;
  constant?: number;
  segment: number | undefined,
  functions: Array<SelectableValue<string>> | undefined,
  functionParameters: Array<string | undefined>,
  parameterOptions: Array<SelectableValue<string>>
}

export interface FlowQueryData {
  segment: number | undefined,
  functions: Array<SelectableValue<string>> | undefined,
  functionParameters: Array<string | undefined>,
  parameterOptions: Array<SelectableValue<string>>
  refId: string
}

export interface FlowFunction {
  excludeFunctions?: number[],
  parentSegments?: number[],
  parameter?: string,
  allowMultiple?: boolean,
  parameterOptions?: (input: string, client: ClientDelegate, start: number | undefined, end: number | undefined) => Promise<string[]>
}

export interface SegmentSubOption {
  label: string;
}

export interface SegmentOption {
  label: string;
  options: SegmentSubOption[]
}

export interface FlowQueryRequest<T extends DataQuery> extends DataQueryRequest<T> {
  queryText: string;
}

export type FlowParsedFunction = Record<string, string | undefined>

export type FlowParsedQueryRow = {
  segment: { id: string | undefined | number, label: string | undefined },
  queryFunctions: FlowParsedFunction[]
  refId: string
}

export type FlowParsedQueryData = FlowParsedQueryRow[];

export interface FlowQueryFunctionProps {
  autofocus: boolean,
  options: Array<{
    label: string;
    options?: Array<{
      label: string;
    }>;
  }>;
  onChange: (functionName: SelectableValue<string>) => void;
  value: SelectableValue<string>;
  moveMeLeft: (index: number) => void;
  moveMeRight: (index: number) => void;
  removeMe: (index: number) => void;
  index: number;
  last: boolean;
  activeParameter: string | undefined,
  setActiveParameter: (activeParam: string, index: number) => void,
  parameterOption: SelectableValue<string>,
  setParameterOption: (parameterOption: SelectableValue<string>, index: number) => void
  client: ClientDelegate;
  start: number | undefined;
  end: number | undefined;
}

export interface ToolsProps {
  moveMeLeft: (index: number) => void,
  moveMeRight: (index: number) => void,
  index: number,
  hideTools: () => void,
  visible: boolean,
  last: boolean
}

export interface ToolButtonProps {
  onClick: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void,
  children: any,
  style?: {}
}

export interface FlowTemplateVariableClientService {
  client: ClientDelegate,
  simpleRequest: SimpleOpenNMSRequest
}

export interface FlowTemplateVariableQueryService {
  function: FlowTemplateVariableQueryFunction,
  start: number,
  end: number,
  limit?: number,
  pattern?: string,
  application?: string,
  location?:string, 
  protocol?: string,
  nodeFilter?: string,
  nodeCriteria?: string,
  nodeId? : string,
  node?: string, 
  iface?: string,
}

export interface FlowTemplateVariableQueryFunction {
  name?: any,
  result?: any
}
