import { SelectableValue } from '@grafana/data'
import { GrafanaDatasource } from 'hooks/useDataSources'
import { ActiveFilter } from '../../datasources/entity-ds/types'

export interface FilterControlProps {
  dashboardUid?: string
  filterEditor: {
    datasource: SelectableValue<GrafanaDatasource> | undefined,
    activeFilters: ActiveFilter[],
    isHorizontalLayout?: boolean
  }
}
