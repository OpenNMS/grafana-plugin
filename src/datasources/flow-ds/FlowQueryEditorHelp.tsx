import React from 'react'
import { QueryEditorHelpProps } from '@grafana/data'
import { DatasourceQueryEditorHelpLink } from '../../components/DatasourceQueryEditorHelpLink'
import { FlowQuery } from './types'

export const FlowQueryEditorHelp: React.FC<QueryEditorHelpProps<FlowQuery>> = () => {
  return (
    <DatasourceQueryEditorHelpLink
      datasourceName='Flow Datasource'
      relativeLink='datasources/flow_datasource.html'
    />
  )
}
