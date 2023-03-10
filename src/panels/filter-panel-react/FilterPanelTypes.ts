import { SelectableValue } from '@grafana/data'
import { GrafanaDatasource } from 'hooks/useDataSources'
import { ActiveFilter } from '../../datasources/entity-ds-react/types'

export interface FilterControlProps {
    filterEditor: {
        datasource: SelectableValue<GrafanaDatasource> | undefined,
        activeFilters: ActiveFilter[]
    }
}
