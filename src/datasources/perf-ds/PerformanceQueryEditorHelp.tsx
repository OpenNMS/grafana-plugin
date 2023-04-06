import React from 'react'
import { QueryEditorHelpProps } from '@grafana/data'
import { DatasourceQueryEditorHelpLink } from '../../components/DatasourceQueryEditorHelpLink'
import { PerformanceQuery } from './types'

export const PerformanceQueryEditorHelp: React.FC<QueryEditorHelpProps<PerformanceQuery>> = () => {
  return (
    <DatasourceQueryEditorHelpLink
      datasourceName='Performance Datasource'
      relativeLink='datasources/performance_datasource.html'
    />
  )
}
