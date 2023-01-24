import { SelectableValue } from '@grafana/data'
import { GrafanaDatasource } from 'hooks/useDataSources'
import { ActiveFilter } from '../../hooks/useFilterData'

export interface FilterControlProps {
    filterEditor: {
        datasource: SelectableValue<GrafanaDatasource> | undefined,
        activeFilters: ActiveFilter[]
    }
}
