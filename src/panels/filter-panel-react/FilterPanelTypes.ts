import { SelectableValue } from "@grafana/data"
import { GrafanaDatasource } from "hooks/useDataSources"

/**
 * entity: Entity name for filter (e.g. "nodes", "alarms")
 * attribute: Entity attribute to filter on (e.g. "id", "label", "Alarm Type")
 * selectionType: whether this filter is single/multi/text
 * altColumnLabel: User customized label for this filter, to display on FilterPanelControl component
 */
export interface ActiveFilter {
    entity: SelectableValue<string | number>
    attribute: SelectableValue<{ id: string | number }>
    selectionType: SelectableValue<string>
    altColumnLabel: string
}

export interface FilterControlProps {
    filterEditor: {
        datasource: SelectableValue<GrafanaDatasource> | undefined,
        activeFilters: ActiveFilter[]
    }
}

export interface FilterSelectableValues {
    filterId: string,
    values: Array<SelectableValue<string | number>>
}

// FilterPanel data saved to localStorage for use by Entity Datasource
export interface FilterEditorData {
    datasource: SelectableValue<GrafanaDatasource> | undefined,
    activeFilters: ActiveFilter[]
    selectableValues: FilterSelectableValues[]
}
