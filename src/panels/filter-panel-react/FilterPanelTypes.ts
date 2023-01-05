import { SelectableValue } from "@grafana/data"
import { GrafanaDatasource } from "hooks/useDataSources"
import { SearchProperty } from "opennms/src/API"

export interface ActiveFilter {
    attribute: SelectableValue<{ id: string | number }>,
    entity: SelectableValue<string | number>,
}
export interface FilterControlProps {
    filterEditor: {
        datasource: SelectableValue<GrafanaDatasource> | undefined,
        activeFilters: ActiveFilter[],
        properties: Record<string,SearchProperty>,
        filterSelectionTypes: Array<SelectableValue<string>>,
        altColumnLabels: string[],
    }
}
