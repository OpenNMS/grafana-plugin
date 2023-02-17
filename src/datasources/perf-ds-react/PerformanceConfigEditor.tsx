import React from 'react';
import { DataSourcePluginOptionsEditorProps } from '@grafana/data';
import { DataSourceHttpSettings } from '@grafana/ui';
import { PerformanceDataSourceOptions } from './types';

interface Props extends DataSourcePluginOptionsEditorProps<PerformanceDataSourceOptions> { }

export const PerformanceConfigEditor: React.FC<Props> = ({ onOptionsChange, options }) => {
    return (
        <DataSourceHttpSettings
            defaultUrl="https://api.example.com"
            dataSourceConfig={options}
            onChange={onOptionsChange}
        />
    )
}
