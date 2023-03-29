import { FilterEditorData } from '../datasources/entity-ds/types'

const getCircularReplacer = () => {
    const seen = new WeakSet()

    return (key, value) => {
        if (typeof value === 'object' && value !== null) {
            if (seen.has(value)) {
                return
            }
            seen.add(value)
        }
        return value
    }
}

const FILTER_PANEL_STORAGE_KEY = 'opennms-filter-panel'

export const saveFilterEditorData = (data: FilterEditorData) => {
    localStorage.setItem(FILTER_PANEL_STORAGE_KEY, JSON.stringify(data, getCircularReplacer()))
}

export const loadFilterEditorData = (): FilterEditorData | null => {
    const json = localStorage.getItem(FILTER_PANEL_STORAGE_KEY)

    if (json) {
        const filterPanel = JSON.parse(json)
        return filterPanel as FilterEditorData
    }

    return null
}

export const clearFilterEditorData = () => {
  localStorage.removeItem(FILTER_PANEL_STORAGE_KEY)
}
