import { DataSourcePluginOptionsEditorProps } from '@grafana/data';
import { DataSourceHttpSettings } from '@grafana/ui';
import React from 'react';
import { FlowDataSourceOptions } from './types';

interface Props extends DataSourcePluginOptionsEditorProps<FlowDataSourceOptions> {}

export const FlowConfigEditor: React.FC<Props> = ({ onOptionsChange, options }) => {
  return (
    <DataSourceHttpSettings
      defaultUrl="https://api.example.com"
      dataSourceConfig={options}
      onChange={onOptionsChange}
    />
  );
};
