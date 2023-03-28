import React from 'react'
import { QueryEditorHelpProps } from '@grafana/data'
import { EntityQuery } from './types'

export const EntityQueryEditorHelp: React.FC<QueryEditorHelpProps<EntityQuery>> = () => {

  return (
    <div>
      Help for Entity Data source queries
    </div>
  )
}
