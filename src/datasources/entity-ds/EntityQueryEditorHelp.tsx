import React from 'react'
import { QueryEditorHelpProps } from '@grafana/data'
import { DatasourceQueryEditorHelpLink } from '../../components/DatasourceQueryEditorHelpLink'
import { EntityQuery } from './types'

export const EntityQueryEditorHelp: React.FC<QueryEditorHelpProps<EntityQuery>> = () => {
  return (
    <DatasourceQueryEditorHelpLink
      datasourceName='Entity Datasource'
      relativeLink='datasources/entity_datasource.html'
    />
  )
}
