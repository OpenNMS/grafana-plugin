import React from 'react'
import { QueryEditorHelpProps } from '@grafana/data'
import { PerformanceQuery } from './types'

export const PerformanceQueryEditorHelp: React.FC<QueryEditorHelpProps<PerformanceQuery>> = () => {

  return (
    <div>
      Help for Performance Data source queries
    </div>
  )
}
