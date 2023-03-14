import { DataSourcePlugin } from '@grafana/data';
import { EntityConfigEditor } from './EntityConfigEditor';
import { EntityDataSource } from './EntityDataSource';
import { EntityQueryEditor } from './EntityQueryEditor';
import { EntityDataSourceOptions, EntityQuery } from './types';


export const plugin = new DataSourcePlugin<EntityDataSource, EntityQuery, EntityDataSourceOptions>(EntityDataSource)
.setConfigEditor(EntityConfigEditor)
.setQueryEditor(EntityQueryEditor);
