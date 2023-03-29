import React from 'react'
import { DataSourcePluginOptionsEditorProps } from '@grafana/data'
import { DataSourceHttpSettings } from '@grafana/ui'
import { EntityDataSourceOptions } from './types'
import { ClearFilterData } from '../../components/ClearFilterData'

interface Props extends DataSourcePluginOptionsEditorProps<EntityDataSourceOptions> { }

export const EntityConfigEditor: React.FC<Props> = ({ onOptionsChange, options }) => {
  return (
    <>
      <DataSourceHttpSettings
          defaultUrl="https://api.example.com"
          dataSourceConfig={options}
          onChange={onOptionsChange}
      />
      <ClearFilterData />
    </>
  )
}
