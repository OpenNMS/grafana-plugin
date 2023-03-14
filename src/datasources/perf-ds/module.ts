import { DataSourcePlugin } from '@grafana/data';
import { PerformanceConfigEditor } from './PerformanceConfigEditor';
import { PerformanceDataSource } from './PerformanceDataSource';
import { PerformanceQueryEditor } from './PerformanceQueryEditor';
import { PerformanceDataSourceOptions, PerformanceQuery } from './types';


export const plugin = new DataSourcePlugin<PerformanceDataSource, PerformanceQuery, PerformanceDataSourceOptions>(PerformanceDataSource)
.setConfigEditor(PerformanceConfigEditor)
.setQueryEditor(PerformanceQueryEditor);
