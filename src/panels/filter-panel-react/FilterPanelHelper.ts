import { SelectableValue } from '@grafana/data'
import { ActiveFilter } from './FilterPanelTypes'

export const getFilterIdFromParts = (entity: SelectableValue<string | number>, attribute: SelectableValue) => {
    const entityName = (entity.label || entity.value || '').toString()
    const attrName = attribute.id || attribute.label || ''

    return `${entityName}_${attrName}`
}

export const getFilterId = (filter: ActiveFilter) => {
    return getFilterIdFromParts(filter.entity, filter.attribute)
}
