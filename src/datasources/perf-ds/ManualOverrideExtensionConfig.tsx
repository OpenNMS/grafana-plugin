import React  from 'react'
import { DataSourceSettings } from '@grafana/data'
import { InlineField, Switch } from '@grafana/ui'
import { PerformanceDataSourceOptions } from './types'

interface Props {
  onOptionsChange: (o: DataSourceSettings<PerformanceDataSourceOptions>) => void
  options: DataSourceSettings<PerformanceDataSourceOptions>
}

export const ManualOverrideExtensionConfig: React.FC<Props> = ({ onOptionsChange, options}) => {
  const tooltipText = 'Enable manual override UI components for some fields, e.g. Performance Attribute Node, ' +
    'where you can enter a template variable or other value manually. ' +
    'Generally should not be needed.'

  const onChange = (value: boolean) => {
    const newOptions = {
      ...options,
      jsonData: {
        ...options.jsonData,
       allowManualOverrideExtensions: value
      }
    }

    onOptionsChange(newOptions)
  }

  return (
    <>
      <style>
        {
          `
          .spacer {
            margin-top: 10px;
            margin-bottom: 10px;
          }
          .perf-config-editor-switch {
              display: flex;
              align-items: center;
              height: 32px;
              width: 32px;
          }
          .perf-config-editor-switch label {
              min-width: 32px;
              width: 32px;
          }
          `
      }
      </style>
      <h3 className='spacer'>Additional Options</h3>
      
      <InlineField className='perf-config-editor-switch-field' label='Allow manual override extensions:' tooltip={tooltipText}>
        <div className='perf-config-editor-switch'>
          <Switch
            value={options.jsonData.allowManualOverrideExtensions}
            onChange={() => onChange(!options.jsonData.allowManualOverrideExtensions)} />
        </div>
      </InlineField>
    </>
  )
}
