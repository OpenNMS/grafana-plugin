import { DataSourcePlugin } from '@grafana/data'
import { FlowConfigEditor } from './FlowConfigEditor'
import { FlowDataSource } from './FlowDataSource'
import { FlowQueryEditor } from './FlowQueryEditor'
import { FlowQueryEditorHelp } from './FlowQueryEditorHelp'
import { FlowDataSourceOptions, FlowQuery } from './types'

/**
 * Value that is used in the backend, but never sent over HTTP to the frontend
 */
export interface BasicSecureJsonData {
  apiKey?: string
}

export type QueryTypesResponse = {
  queryTypes: string[]
};

export const plugin = new DataSourcePlugin<FlowDataSource, FlowQuery, FlowDataSourceOptions>(FlowDataSource)
  .setConfigEditor(FlowConfigEditor)
  .setQueryEditor(FlowQueryEditor)
  .setQueryEditorHelp(FlowQueryEditorHelp);
