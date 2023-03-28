import React from 'react'
import { QueryEditorHelpProps } from '@grafana/data'
import { FlowQuery } from './types'

export const FlowQueryEditorHelp: React.FC<QueryEditorHelpProps<FlowQuery>> = () => {

  return (
    <div>
      Help for Flow Data source queries
    </div>
  )
}
