import { DataSourcePluginOptionsEditorProps } from '@grafana/data';
import { DataSourceHttpSettings } from '@grafana/ui';
import React from 'react';
import { EntityDataSourceOptions } from './types';

interface Props extends DataSourcePluginOptionsEditorProps<EntityDataSourceOptions> { }

export const EntityConfigEditor: React.FC<Props> = ({ onOptionsChange, options }) => {
    return (
        <DataSourceHttpSettings
            defaultUrl="https://api.example.com"
            dataSourceConfig={options}
            onChange={onOptionsChange}
        />
    );
};
